# Import Packages and Set Up Directories ----------------------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(googleLanguageR, googleCloudStorageR, sound, tuneR, fs,
               data.table, DescTools, tidyverse, here, magrittr)
pacman::p_load_gh("LiKao/VoiceExperiment")
devtools::source_gist("746685f5613e01ba820a31e57f87ec87")

# May take a few minutes to run the first time you're authenticating
here("Authentication_File.json") %T>%
  gl_auth %T>%
  gcs_auth

data_dir <- here("online-task", "analyses", "Data")
audio_dir <- here(data_dir, "audio")

pcpts_with_all_audio_saved <- dir_ls(path(audio_dir, "full")) %>%
  str_extract("(?<=full/)[[:alnum:]]*") %>%
  as_tibble_col(column_name = "Sub_Code") %>%
  count(Sub_Code) %>%
  filter(n == 4) %>%
  select(Sub_Code) %>%
  filter(! Sub_Code %in% c("qiAx7h9D", # when I re-run the voice encoding one last time, remove these participants # this guy didn't take the task seriously
                           "sJiC0naK", # this person's voice was consistently quite low
                           "OWNpYNOV", # this person's english was rather poor
                           "TeIzC5Si", # this WorkerID participated twice
                           # "nE0in8wT", # error presenting the stimuli that began on its trial 50
                           "gt3skyFD", # error trying to read in the wav file, there's some glitch
                           "tOGmbtQk")) # he had too many blank responses, and he didn't seem particularly focused

recording_durations <- path(audio_dir, "full") %>%
  dir_ls(glob = "*.wav") %>%
  as.character %>%
  as_tibble_col("Full_Audio_Path") %>%
  mutate(Recording_Duration = map_dbl(Full_Audio_Path, ~sound::duration(.x)))


# Import Data and Run through Google Speech-to-Text -----------------------

predictive_context <- function(mydf, either_block_or_side, relevant_task){
  mydf %>%
    group_by(Sub_Code, !!either_block_or_side) %>%
    mutate(!!paste0(quo_name(either_block_or_side), "_Bias") := 
             case_when(
               Task != relevant_task ~ NA_character_,
               sum(Congruency == "Congruent") / n() > .5 ~ "Congruent",
               TRUE ~ "Incongruent")
    )
}

prep_data <- 
  list(
    here("online-task", "analyses", "Data", "adaptive_control.csv"),
    here("images", "IPNP", "IPNP_spreadsheet_synonyms.csv"),
    here("online-task", "analyses", "Data", "adaptive_control_demographics.csv")) %>%
  map(fread) %>%
  reduce(left_join) %>%
  right_join(pcpts_with_all_audio_saved) %>%
  filter(Block > 0) %>%
  mutate(Full_Audio_Path = map2_chr(Sub_Code, Block, function(x, y) {
      path(audio_dir, "full") %>%
        dir_ls(regexp = paste0(x, "_.*_", y), fail = FALSE)
  })) %>%
  left_join(recording_durations) %>%
  group_by(Sub_Code, Block) %>%
  mutate(
    across(Nationality, ~if_else(is.na(.) | . == "O", "en-US", .)),
    Time_Skipped = max(Stim_Offset) %>%
      subtract(min(Trial_Start)) %>%
      divide_by(1000) %>%
      subtract(Recording_Duration),
    across(c(Fix_Onset, Fix_Offset, Stim_Onset, Stim_Offset, Trial_Start),
           . %>%
             subtract(min(Trial_Start)) %>%
             divide_by(1000) %>%
             subtract(Time_Skipped)),
    End_recording = 
      if_else(
        lead(Fix_Offset) > Stim_Onset + 58 | row_number() == n(),
        Stim_Onset + 58,
        lead(Fix_Offset, default = last(Fix_Offset) + 58))
  ) %>%
  rowwise() %>%
  filter(!0 %in% c(Audio_Connected, lag(Audio_Connected), lead(Audio_Connected),
                   Audio_Permitted, lag(Audio_Permitted), lead(Audio_Permitted),
                   Full_Screen, lag(Full_Screen), lead(Full_Screen))) %>%
  ungroup() %>%
  predictive_context(quo(Block), "Predictive_Blocks") %>%
  predictive_context(quo(Task_Side), "Predictive_Locations") %>%
  ungroup() %>%
  mutate(across(c(Synonyms),
                ~if_else(Dominant_Response == Label,
                         paste(Label, ., sep = ", "),
                         paste(Dominant_Response, Label, ., sep = ", ")))) %>%
  write_csv(here(data_dir, "prep_data.csv"))

prep_data <- read_csv(here(data_dir, "prep_data.csv")) # maybe can delete this line when i'm done with everything
# nrow(prep_data) %>% 

i_max <- prep_data %>% nrow %>% RoundTo(100, ceiling) %>% divide_by(100)
for (loop_count in 24:(i_max-1)){
  while_count <- 0
  while (while_count < 5){
    tryCatch({
      prep_data %>%
        filter(row_number() > (loop_count * 100 ),
               row_number() <= (loop_count * 100) + 100) %>%
        pmap_df(function(Stim_Onset, Trial, End_recording,
                         Sub_Code, Synonyms, Nationality,
                         Full_Audio_Path, ...){
          spliced_audio_dir <- path(audio_dir, "spliced", Sub_Code) %T>%
            dir_create %>%
            path(paste0("Trial_", Trial, ".wav"))
          
          readWave(Full_Audio_Path,
                   from = Stim_Onset, to = End_recording, units = "seconds") %T>%
            writeWave(spliced_audio_dir, extensible = FALSE)
          
          transcribed_list <- gl_speech(spliced_audio_dir,                            # less likely to get errors if you run the script late at...
                                        languageCode = Nationality,                   # ...night, I guess there's fewer other people trying to...
                                        speechContexts =                              # ...access the API at that time
                                          list(phrases = strsplit(Synonyms, ',') %>%
                                                 unlist %>%
                                                 trimws))
          
          Sys.sleep(1)
          
          precise_timing <- spliced_audio_dir %>%
            read.wav(filter = list(high = 4000)) %>%
            onsets
          
          if(length(transcribed_list$timings) > 0){
            df <- transcribed_list %>%
              pluck('transcript') %>%
              select(transcript, confidence) %>%
              mutate(across(transcript, trimws))
          } else {
            df <- tibble(transcript = NA, confidence = NA)
          }
          
          mutate(df,
                 Sub_Code,
                 Trial,
                 preciseStartTime = pluck(precise_timing, first, "start"),
                 preciseEndTime = pluck(precise_timing, last, "end"))
        }) %>%
        write_csv(here(data_dir, paste0("pull_in_from_google", loop_count,  ".csv")))
      
      break
    }, errror=function(e) {
      while_count <- while_count + 1
    }
    )
  }
}




walk(124:127, function(x) {
  prep_data %>%
# pull_in_from_google <- prep_data %>%
  filter(row_number() > (x * 100 ),
         row_number() <= (x * 100) + 100) %>%
    # filter(row_number() > 12622) %>%
  pmap_df(function(Stim_Onset, Trial, End_recording,
                   Sub_Code, Synonyms, Nationality,
                   Full_Audio_Path, ...){
    # print(Stim_Onset)
    # print(End_recording)
  #   broom <- Trial
  #   loom <- Sub_Code
  #   if (nrow(tomabio %>% filter(Trial == broom - 1, Sub_Code == loom)) > 0) {
  # print(Sub_Code)
  #       print(Trial)
    spliced_audio_dir <- path(audio_dir, "spliced", Sub_Code) %T>%
      dir_create %>%
      path(paste0("Trial_", Trial, ".wav"))

    readWave(Full_Audio_Path,
             from = Stim_Onset, to = End_recording, units = "seconds") %T>%
      writeWave(spliced_audio_dir, extensible = FALSE)
    
    transcribed_list <- gl_speech(spliced_audio_dir,                            # less likely to get errors if you run the script late at...
                                  languageCode = Nationality,                   # ...night, I guess there's fewer other people trying to...
                                  speechContexts =                              # ...access the API at that time
                                    list(phrases = strsplit(Synonyms, ',') %>%
                                           unlist %>%
                                           trimws))
      # transcriber(spliced_audio_dir, Nationality, Synonyms)
    # transcribed_list <- transcriber(spliced_audio_dir, Nationality, Synonyms, "command_and_search")
    # transcribed_list <- tryCatch({
      # transcriber(spliced_audio_dir, Nationality, Synonyms)
    # }, error = function(e) {
      # transcriber(spliced_audio_dir, Nationality, Synonyms)
      # print(Sub_Code)
      # print(Trial)
    # })

    Sys.sleep(1)

    # if (length(transcribed_list$timings) > 0){
    precise_timing <- spliced_audio_dir %>%
      read.wav(filter = list(high = 4000)) %>%
      onsets
    
    if(length(transcribed_list$timings) > 0){
      df <- transcribed_list %>%
        pluck('transcript') %>%
        select(transcript, confidence) %>%
        mutate(across(transcript, trimws))
    } else {
      df <- tibble(transcript = NA, confidence = NA)
    }
    
    mutate(df,
           Sub_Code,
           Trial,
           preciseStartTime = pluck(precise_timing, first, "start"),
           preciseEndTime = pluck(precise_timing, last, "end"))
  }) %>%
  # right_join(prep_data) %T>% # commenting this line out for now because I'm not sure how it deals with multiple responses/rows for one trial
  # write_csv(here(data_dir, "pull_in_from_google.csv")) #%>%
  write_csv(here(data_dir, paste0("pull_in_from_google", x,  ".csv"))) #%>%
})


# Clean speech-to-text data -----------------------------------------------

pull_in_from_google2 <- path(data_dir) %>%
  dir_ls(regexp = "pull_in_from_google\\d") %>%
  map_df(fread) %>%
  right_join(prep_data) %>% #remove this line from final version
  group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # so that each trial only takes up 1 row,...
                   -confidence)) %>%                                            # though each trial might only be one row (also some nations' lack a confidence rating)...
  summarise(across(transcript, ~paste0(., collapse = "")),                      # anyways; depends on whether Google spits...
            across(preciseStartTime, min),                                      # out multiple rows for $transcript
            across(preciseEndTime, max),
            across(confidence, mean)) %>% # might not wind up using this last `across` line
  ungroup() %>%
  left_join(read_csv(here(data_dir, "corrected_responses.csv")),
            by=c("Sub_Code", "Trial")) %>%
  mutate(
    transcript = coalesce(transcript.y, transcript.x),
    across(confidence, ~if_else(is.na(transcript.y), ., NA_real_)),
    across(c(Dominant_Response, Label, Synonyms, transcript),
                ~str_remove_all(., " ") %>% tolower),
    Timely_Response = if_else(Stim_Onset + preciseStartTime <= Stim_Offset,
                              TRUE, FALSE)
  ) %>%
  mutate(Accuracy = pmap_lgl(., function(
    Synonyms, transcript, Label, Congruency, ...){
    
    Synonyms <- StrSplit(Synonyms, ",")
    Synonyms_and_plurals <- c(Synonyms,
                              paste0("a", Synonyms),
                              paste0(Synonyms, "s"),
                              paste0(Synonyms, "es"))
    
    transcript_beginning <- str_sub(transcript, end=str_count(Label))
    
    case_when(
      transcript_beginning == Label & Congruency == "Incongruent" ~ FALSE,
      transcript %in% Synonyms_and_plurals ~ TRUE,
      TRUE ~ NA)})
    
  ) %>%
  arrange(Sub_Code, Trial) %>%
  group_by(Sub_Code, transcript) %>%
  mutate(First_Word_Use = (row_number() == 1)) %>%                                                # If someone says the same word for multiple trials, only keep the first trial they said that word and filter out the other times
  group_by(Sub_Code, Block) %>%
  filter(Timely_Response, lag(Timely_Response), Accuracy, lag(Accuracy),
         First_Word_Use) %>%
  ungroup()

# # the following few lines are temporary until there's enough participants
# pull_in_from_google2 <- pull_in_from_google2 %>%
#   filter(Task == "Neutral") %>%
#   select(Dominant_Response) %>%
#   distinct() %>%
#   left_join(pull_in_from_google2)


# Plotting set up and functions -------------------------------------------

Neutral_Timings <- pull_in_from_google2 %>%
  filter(Task == "Neutral") %>%
  group_by(Dominant_Response) %>%
  summarise(RT = mean(preciseStartTime, na.rm = TRUE))

pull_in_from_google3 <- pull_in_from_google2 %>%
  filter(str_detect(Task, "Predictive")) %>%
  mutate(
    RT_comparison = map_dbl(
      Dominant_Response,
      ~filter(Neutral_Timings, Dominant_Response == .) %>% pull(RT)),
    RT_diff = preciseStartTime - RT_comparison,
    Bias = coalesce(Block_Bias, Task_Side_Bias))

pull_in_from_google3 %>%
  ggplot(aes(x = Congruency, y = RT_diff, fill = Bias)) +
  geom_split_violin(alpha = .5, show.legend = FALSE) +
  geom_boxplot(width = .2, position = position_dodge(.25), outlier.shape = NA,
               alpha = .3, aes(color = Congruency), size = .5) +
  scale_fill_manual(values=c("#D55E00", "#0072B2")) +
  scale_color_manual(values=c("#D55E00", "#0072B2")) +
  theme_bw()


(pull_in_from_google3 %>%
    mutate(across(Task, ~str_replace(., "_", " ") %>% paste0(",")),
           across(Congruency, ~paste(., "Trials"))) %>%
    ggplot(aes(x = NA, y = RT_diff, fill = Bias)) +
    geom_split_violin(alpha = .5, show.legend = FALSE) +
    geom_boxplot(width = .2, position = position_dodge(.25), outlier.shape = NA,
                 alpha = .3, aes(color = Congruency), size = .5) +
    scale_fill_manual(values=c("#D55E00", "#0072B2")) +
    scale_color_manual(values=c("#D55E00", "#0072B2")) +
    theme_minimal() +
    theme(#legend.position="right", legend.box = "vertical",
      legend.spacing = unit(1, "cm"), panel.grid.major.x=element_blank(),
      axis.text.x=element_blank(), axis.title.x=element_blank()) +
    guides(color = guide_legend("Trial", order = 1),
           fill = guide_legend("Block/Location", order = 2)) +
    facet_wrap(Task ~ Congruency, nrow = 1 ) +
    labs(
      title = "Response Time as a Function of Trial and Contextual Congruencies",
      subtitle = "Congruency was paired with a contextual feature, either block number or stimulus location, depending on the task\n",
      y = "Response Time (compared to control condition, in seconds)",
      caption = str_wrap(
        "Each of the values that compose each of the vertical 'raincloud'/violin 
      plots is for a given image that was presented to a given participant. 
      Specifically, for that image, it is a given participant's
      response time minus the average response time when that image was
      presented in the neutral condition.",
        140)))# %>%

#ggsave(file=here("online-task", "analyses", "test.svg"), width=10, height=8)


# Checking that the data looks like it generally should -------------------
 
#  no longer true
# # weird that so many more trials getting removed during filtering from the predictive tasks; need to investigate why
# pull_in_from_google2 %>% count(Sub_Code, Task) 

# faster rt for predictive than neutral congruent trials, which makes sense since it has the words on the screen; but likewise i'd expect incongruent trials to be slower to respond on predictive blocks (in fact not just expect it, it's sort of guaranteed according to stroop), so might want to see why that's not happening (or run more participants)
pull_in_from_google2 %>% group_by(Task, Congruency) %>% summarise(mean(preciseStartTime))

# for looking at which trials didn't have a response that registered either correctly or incorrectly; requires re-running pull_in_from_google2 without the filter at the bottom
pull_in_from_google2 %>%
  select(Congruency, Dominant_Resp, Dominant_Response, Fix_Offset, Fix_Onset, Full_Screen, Keep, Label, Stim_Offset, Stim_Onset, Task, Trial_Start, Worker_ID, Audio_Onset, End_recording, Pic_Num, transcript, Synonyms, confidence, Sub_Code, Trial, preciseStartTime, preciseEndTime, Accuracy) %>%
  filter(is.na(Accuracy)) %>%
  View()

# for seeing how many trials each participant didn't finish speaking before the ITI began; requires re-running pull_in_from_google2 without the filter at the bottom
pull_in_from_google2 %>%
  mutate(preciseStartTime = preciseStartTime + Stim_Onset,
         preciseEndTime = preciseEndTime + Stim_Onset) %>%
select(Sub_Code, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording) %>%
  group_by(preciseEndTime > Stim_Offset) %>%
  summarise(100 * n() / nrow(pull_in_from_google2))

# for looking at percent of correct responses; requires re-running pull_in_from_google2 without the filter at the bottom
pull_in_from_google2 %>%
  group_by(Accuracy) %>%
  summarise((100 * n()) / nrow(pull_in_from_google2))

# make sure the length of all the spliced audio recordings are basically the same; looks good, like all the recordings are the same length except the ones at the end of each block, which are by design going to yield a little shorter recordings than the rest of trials; okay actually at least one exception so far, and counting
trout <- tibble(jins = c(dir_ls(path(audio_dir, "spliced"), recurse = TRUE, glob = "*.wav"))) %>%
  rowwise() %>%
  mutate(broom = sound::duration(as.character(jins) ))
trout %>%
  ggplot() +
  geom_histogram(aes(x = broom))
trout %>%
  filter(broom < 3) %>%
  mutate(tomb = str_extract(jins, "_.*")) %>%
  mutate(tomb = str_extract(tomb, "[[:alnum:]]..")) %>%
  mutate(tomb = as.numeric(tomb)) %>%
  arrange(tomb) %>%
  View()
trout %>%
  filter(broom > 5) %>%
  mutate(tomb = str_extract(jins, "_.*")) %>%
  mutate(tomb = str_extract(tomb, "[[:alnum:]]..")) %>%
  mutate(tomb = as.numeric(tomb)) %>%
  mutate(mada = str_extract(jins, "(?<=spliced/)[[:alnum:]]*")) %>%
  arrange(tomb) %>%
  View()


# this code shows that there are delays in when the audio recordings actually begin, so i guess  i can add their length back onto the front of trial's start time
pinata <- list(
  here("online-task", "analyses", "Data", "adaptive_control.csv"),
  here("images", "IPNP", "IPNP_spreadsheet_synonyms.csv"),
  here("online-task", "analyses", "Data", "adaptive_control_demographics.csv")) %>%
  map(fread) %>%
  reduce(left_join) %>%
  right_join(pcpts_with_all_audio_saved) %>%
  filter(Trial %in% c(1, 62, 63, 124, 125, 186, 187, 248)) %>%
  group_by(Sub_Code, Block) %>%
  summarise(blockBegins = min(Trial_Start),
            blockEnds = max(Stim_Offset)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(recording_path = str_subset(as.character(path(audio_dir, "full") %>% dir_ls()), paste0("/Users/jackdolgin/Dropbox/Stages of Life/Grad School/Repos/Adaptive-Control/online-task/analyses/Data/audio/full/", Sub_Code, "_.*_", Block, ".wav"))) %>%
  rowwise() %>%
  mutate(recording_duration = sound::duration(recording_path),
         array_duration = (blockEnds - blockBegins) / 1000)

pinata %>%
  arrange(desc(duration_diff)) %>%
  View()
# 
# 
# get_actual_durations <- here(data_dir, "pull_in_from_google_new.csv") %>%
#   read_csv %>%
#   right_join(prep_data) %>% #remove this line from final version
#   group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # so that each trial only takes up 1 row,...
#                    -confidence)) %>%                                            # though each trial might only be one row (also some nations' lack a confidence rating)...
#   summarise(across(transcript, ~paste0(., collapse = "")),                      # anyways; depends on whether Google spits...
#             across(preciseStartTime, min),                                      # out multiple rows for $transcript
#             across(preciseEndTime, max),
#             across(confidence, mean)) %>% # might not wind up using this last `across` line
#   ungroup() %>%
#   left_join(read_csv(here(data_dir, "corrected_responses.csv")),
#             by=c("Sub_Code", "Trial")) %>%
#   mutate(recording_path = path(audio_dir, "spliced", Sub_Code, paste0("Trial_", Trial, ".wav"))) %>%
#   rowwise() %>%
#   mutate(recording_duration = sound::duration(as.character(recording_path)))

# are the differences in time between each trial always the same, or about the same?
pull_in_from_google2 %>%
  group_by(Sub_Code, Block) %>%
  mutate(time_diff = Stim_Onset - lag(Stim_Onset)) %>%
  arrange(desc(time_diff)) %>%
  select(Sub_Code, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording, time_diff) %>%
  filter(!is.na(time_diff)) %>%
  View

pull_in_from_google2 %>%
  ungroup() %>%
  select(Sub_Code, Task, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording) %>%
  mutate(time_diff = Stim_Onset - lag(Stim_Onset)) %>%
  filter(! is.na(time_diff)) %>%
  group_by(Sub_Code) %>%
  mutate(time_diff2 = max(time_diff)) %>%
  ungroup() %>%
  # filter(time_diff == time_diff2 | lag(time_diff) == time_diff2 | lead(time_diff) == time_diff2) %>%
  filter(time_diff > 5 | lag(time_diff) > 5 | lead(time_diff) > 5) %>%
  arrange(Trial) %>%
  View

pull_in_from_google2 %>%
  ungroup() %>%
  select(Sub_Code, Task, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording) %>%
  mutate(time_diff = Stim_Onset - lag(Stim_Onset)) %>%
  filter(! is.na(time_diff)) %>%
  group_by(Sub_Code) %>%
  mutate(time_diff2 = max(time_diff)) %>%
  ungroup() %>%
  filter(time_diff == time_diff2 | lag(time_diff) == time_diff2 | lead(time_diff) == time_diff2) %>%
  ggplot() +
  geom_histogram(aes(x = time_diff2))


pull_in_from_google2 %>%
  ungroup() %>%
  select(Sub_Code, Task, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording) %>%
  mutate(time_diff = Stim_Onset - lag(Stim_Onset)) %>%
  filter(! is.na(time_diff)) %>%
  group_by(Sub_Code) %>%
  mutate(time_diff2 = max(time_diff)) %>%
  ungroup() %>%
  # filter(time_diff == time_diff2 | lag(time_diff) == time_diff2 | lead(time_diff) == time_diff2) %>%
  filter(time_diff > 5) %>%
  ggplot() +
  geom_histogram(aes(x = Sub_Code), stat = 'count')





# looking at OmLtwAt4

list(
  here("online-task", "analyses", "Data", "adaptive_control.csv"),
  here("images", "IPNP", "IPNP_spreadsheet_synonyms.csv"),
  here("online-task", "analyses", "Data", "adaptive_control_demographics.csv")) %>%
  map(fread) %>%
  reduce(left_join) %>%
  right_join(pcpts_with_all_audio_saved) %>%
  filter(Block > 0) %>%
  mutate(Full_Audio_Path = map2_chr(Sub_Code, Block, function(x, y) {
    path(audio_dir, "full") %>%
      dir_ls(regexp = paste0(x, "_.*_", y), fail = FALSE)
  }))  %>% group_by(Sub_Code, Block) %>%
  filter(Sub_Code == "OmLtwAt4") %>%  group_by(Sub_Code, Block) %>%
  mutate(
    across(Nationality, ~if_else(is.na(.) | . == "O", "en-US", .)),
    Audio_Onset = 
      str_extract(Full_Audio_Path,
                  paste0("(?<=", Sub_Code, "_)\\d*\\.\\d*")) %>%
      as.numeric,
    across(c(Fix_Onset, Fix_Offset, Stim_Onset, Stim_Offset, Trial_Start),
           . %>% subtract(Audio_Onset) %>% divide_by(1000)),
    End_recording = 
      ifelse(
        lead(Fix_Offset) > Stim_Onset + 58 | row_number() == n(),
        Stim_Onset + 58,
        lead(Fix_Offset, default = last(Fix_Offset) + 58))
  ) %>%
  rowwise() %>%
  filter(!0 %in% c(Audio_Connected, lag(Audio_Connected), lead(Audio_Connected),
                   Audio_Permitted, lag(Audio_Permitted), lead(Audio_Permitted),
                   Full_Screen, lag(Full_Screen), lead(Full_Screen))) %>%
  ungroup() %>%
  select(Sub_Code, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, Stim_Offset, End_recording) %>%
  View


# looking at wPdpRZfw
here(data_dir, "pull_in_from_google_new.csv") %>%
  read_csv %>%
  right_join(prep_data) %>% #remove this line from final version
  group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # so that each trial only takes up 1 row,...
                   -confidence)) %>%                                            # though each trial might only be one row (also some nations' lack a confidence rating)...
  summarise(across(transcript, ~paste0(., collapse = "")),                      # anyways; depends on whether Google spits...
            across(preciseStartTime, min),                                      # out multiple rows for $transcript
            across(preciseEndTime, max),
            across(confidence, mean)) %>% # might not wind up using this last `across` line
  ungroup() %>%
  left_join(read_csv(here(data_dir, "corrected_responses.csv")),
            by=c("Sub_Code", "Trial")) %>%
  mutate(
    transcript = coalesce(transcript.y, transcript.x),
    across(confidence, ~if_else(is.na(transcript.y), ., NA_real_)),
    across(c(Dominant_Response, Label, Synonyms, transcript),
           ~str_remove_all(., " ") %>% tolower),
    Timely_Response = if_else(Stim_Onset + preciseStartTime <= Stim_Offset,
                              TRUE, FALSE)
  ) %>%
  filter(Sub_Code == "wPdpRZfw") %>%
  arrange(Sub_Code, Trial) %>%
  select(Sub_Code, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording) %>%
  mutate(hoopla = Fix_Onset - Trial_Start,
         across(c(preciseStartTime, preciseEndTime), ~ . + Stim_Onset)) %>%
  View


here(data_dir, "pull_in_from_google_new.csv") %>%
  read_csv %>%
  right_join(prep_data) %>% #remove this line from final version
  group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # so that each trial only takes up 1 row,...
                   -confidence)) %>%                                            # though each trial might only be one row (also some nations' lack a confidence rating)...
  summarise(across(transcript, ~paste0(., collapse = "")),                      # anyways; depends on whether Google spits...
            across(preciseStartTime, min),                                      # out multiple rows for $transcript
            across(preciseEndTime, max),
            across(confidence, mean)) %>% # might not wind up using this last `across` line
  ungroup() %>%
  left_join(read_csv(here(data_dir, "corrected_responses.csv")),
            by=c("Sub_Code", "Trial")) %>%
  mutate(
    transcript = coalesce(transcript.y, transcript.x),
    across(confidence, ~if_else(is.na(transcript.y), ., NA_real_)),
    across(c(Dominant_Response, Label, Synonyms, transcript),
           ~str_remove_all(., " ") %>% tolower),
    Timely_Response = if_else(Stim_Onset + preciseStartTime <= Stim_Offset,
                              TRUE, FALSE)
  ) %>%
  filter(Sub_Code == "wPIgEbkZ") %>%
  arrange(Sub_Code, Trial) %>%
  select(Sub_Code, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording) %>%
  mutate(hoopla = Fix_Onset - Trial_Start,
         across(c(preciseStartTime, preciseEndTime), ~ . + Stim_Onset),
         preciseStartTime_old = preciseStartTime - Stim_Onset) %>%
  View


# does rt increase over trials? it seems to be a lot faster during the first block than for the rest of the blocks; this graph represents the problem quite well

here(data_dir, "pull_in_from_google_new.csv") %>%
  read_csv %>%
  right_join(prep_data) %>% #remove this line from final version
  group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # so that each trial only takes up 1 row,...
                   -confidence)) %>%                                            # though each trial might only be one row (also some nations' lack a confidence rating)...
  summarise(across(transcript, ~paste0(., collapse = "")),                      # anyways; depends on whether Google spits...
            across(preciseStartTime, min),                                      # out multiple rows for $transcript
            across(preciseEndTime, max),
            across(confidence, mean)) %>% # might not wind up using this last `across` line
  ungroup() %>%
  left_join(read_csv(here(data_dir, "corrected_responses.csv")),
            by=c("Sub_Code", "Trial")) %>%
  mutate(
    transcript = coalesce(transcript.y, transcript.x),
    across(confidence, ~if_else(is.na(transcript.y), ., NA_real_)),
    across(c(Dominant_Response, Label, Synonyms, transcript),
           ~str_remove_all(., " ") %>% tolower),
    Timely_Response = if_else(Stim_Onset + preciseStartTime <= Stim_Offset,
                              TRUE, FALSE)
  ) %>%
  # filter(Sub_Code == "OmLtwAt4") %>%
  arrange(Sub_Code, Trial) %>%
  select(Sub_Code, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording) %>%
  # group_by(Trial, Sub_Code) %>%
  # summarise(preciseStartTime = mean(preciseStartTime, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = Trial, y = preciseStartTime)) +
  geom_smooth(aes(x = Trial, y = preciseStartTime)) +
  facet_wrap(vars(Sub_Code), ncol = 8)



#this is the same as that graph right above, but filtering out incorrect and no response answers removes a lot of the trials with that noise
potato <- here(data_dir, "pull_in_from_google_new.csv") %>%
  read_csv %>%
  right_join(prep_data) %>% #remove this line from final version
  group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # so that each trial only takes up 1 row,...
                   -confidence)) %>%                                            # though each trial might only be one row (also some nations' lack a confidence rating)...
  summarise(across(transcript, ~paste0(., collapse = "")),                      # anyways; depends on whether Google spits...
            across(preciseStartTime, min),                                      # out multiple rows for $transcript
            across(preciseEndTime, max),
            across(confidence, mean)) %>% # might not wind up using this last `across` line
  ungroup() %>%
  left_join(read_csv(here(data_dir, "corrected_responses.csv")),
            by=c("Sub_Code", "Trial")) %>%
  mutate(
    transcript = coalesce(transcript.y, transcript.x),
    across(confidence, ~if_else(is.na(transcript.y), ., NA_real_)),
    across(c(Dominant_Response, Label, Synonyms, transcript),
           ~str_remove_all(., " ") %>% tolower),
    Timely_Response = if_else(Stim_Onset + preciseStartTime <= Stim_Offset,
                              TRUE, FALSE)
  ) %>%
  mutate(Accuracy = pmap_lgl(., function(
    Synonyms, transcript, Label, Congruency, ...){
    
    Synonyms <- StrSplit(Synonyms, ",")
    Synonyms_and_plurals <- c(Synonyms,
                              paste0("a", Synonyms),
                              paste0(Synonyms, "s"),
                              paste0(Synonyms, "es"))
    
    transcript_beginning <- str_sub(transcript, end=str_count(Label))
    
    case_when(
      transcript_beginning == Label & Congruency == "Incongruent" ~ FALSE,
      transcript %in% Synonyms_and_plurals ~ TRUE,
      TRUE ~ NA)})
    
  ) %>%
  arrange(Sub_Code, Trial) %>%
  group_by(Sub_Code, transcript) %>%
  mutate(First_Word_Use = (row_number() == 1)) %>%                                                # If someone says the same word for multiple trials, only keep the first trial they said that word and filter out the other times
  group_by(Sub_Code, Block) %>%
  filter(Timely_Response, lag(Timely_Response), Accuracy, lag(Accuracy),
         First_Word_Use)

potato %>%
  ungroup() %>%
  arrange(Sub_Code, Trial) %>%
  select(Sub_Code, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording) %>%
  # group_by(Trial, Sub_Code) %>%
  # summarise(preciseStartTime = mean(preciseStartTime, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = Trial, y = preciseStartTime)) +
  geom_smooth(aes(x = Trial, y = preciseStartTime)) +
  facet_wrap(vars(Sub_Code), ncol = 8)


here(data_dir, "pull_in_from_google_new.csv") %>%
  read_csv %>%
  right_join(prep_data) %>% #remove this line from final version
  group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # so that each trial only takes up 1 row,...
                   -confidence)) %>%                                            # though each trial might only be one row (also some nations' lack a confidence rating)...
  summarise(across(transcript, ~paste0(., collapse = "")),                      # anyways; depends on whether Google spits...
            across(preciseStartTime, min),                                      # out multiple rows for $transcript
            across(preciseEndTime, max),
            across(confidence, mean)) %>% # might not wind up using this last `across` line
  ungroup() %>%
  left_join(read_csv(here(data_dir, "corrected_responses.csv")),
            by=c("Sub_Code", "Trial")) %>%
  mutate(
    transcript = coalesce(transcript.y, transcript.x),
    across(confidence, ~if_else(is.na(transcript.y), ., NA_real_)),
    across(c(Dominant_Response, Label, Synonyms, transcript),
           ~str_remove_all(., " ") %>% tolower),
    Timely_Response = if_else(Stim_Onset + preciseStartTime <= Stim_Offset,
                              TRUE, FALSE)
  ) %>%
  mutate(Accuracy = pmap_lgl(., function(
    Synonyms, transcript, Label, Congruency, ...){
    
    Synonyms <- StrSplit(Synonyms, ",")
    Synonyms_and_plurals <- c(Synonyms,
                              paste0("a", Synonyms),
                              paste0(Synonyms, "s"),
                              paste0(Synonyms, "es"))
    
    transcript_beginning <- str_sub(transcript, end=str_count(Label))
    
    case_when(
      transcript_beginning == Label & Congruency == "Incongruent" ~ FALSE,
      transcript %in% Synonyms_and_plurals ~ TRUE,
      TRUE ~ NA)})
    
  ) %>%
  arrange(Sub_Code, Trial) %>%
  group_by(Sub_Code, transcript) %>%
  mutate(First_Word_Use = (row_number() == 1)) %>%                                                # If someone says the same word for multiple trials, only keep the first trial they said that word and filter out the other times
  group_by(Sub_Code, Block) %>%
  ungroup() %>%
  arrange(Sub_Code, Trial) %>%
  select(Sub_Code, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording) %>%
  mutate(Duration = End_recording - Stim_Onset) %>%
  filter(! Trial %in% c(62, 124, 186, 248)) %>%
  # group_by(Trial, Sub_Code) %>%
  # summarise(preciseStartTime = mean(preciseStartTime, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = Trial, y = Duration)) +
  facet_wrap(vars(Sub_Code), ncol = 8)


# so the stimulus always seems to onset the same time after the trial begins
here(data_dir, "pull_in_from_google_new.csv") %>%
  read_csv %>%
  right_join(prep_data) %>% #remove this line from final version
  group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # so that each trial only takes up 1 row,...
                   -confidence)) %>%                                            # though each trial might only be one row (also some nations' lack a confidence rating)...
  summarise(across(transcript, ~paste0(., collapse = "")),                      # anyways; depends on whether Google spits...
            across(preciseStartTime, min),                                      # out multiple rows for $transcript
            across(preciseEndTime, max),
            across(confidence, mean)) %>% # might not wind up using this last `across` line
  ungroup() %>%
  left_join(read_csv(here(data_dir, "corrected_responses.csv")),
            by=c("Sub_Code", "Trial")) %>%
  mutate(
    transcript = coalesce(transcript.y, transcript.x),
    across(confidence, ~if_else(is.na(transcript.y), ., NA_real_)),
    across(c(Dominant_Response, Label, Synonyms, transcript),
           ~str_remove_all(., " ") %>% tolower),
    Timely_Response = if_else(Stim_Onset + preciseStartTime <= Stim_Offset,
                              TRUE, FALSE)
  ) %>%
  # filter(Sub_Code == "OmLtwAt4") %>%
  arrange(Sub_Code, Trial) %>%
  select(Sub_Code, Trial, Congruency, Dominant_Resp, Dominant_Response, Label, transcript, Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, preciseStartTime, preciseEndTime, Stim_Offset, End_recording) %>%
  mutate(booms = Stim_Onset - Trial_Start) %>%
  # group_by(Trial, Sub_Code) %>%
  # summarise(preciseStartTime = mean(preciseStartTime, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = Trial, y = booms)) +
  facet_wrap(vars(Sub_Code), ncol = 8)






#how much time people spend on instructions and during practice and during instructions between practice and main task doesn't seme to have any relationship with whether they'll have that weird first-block behavior
list(
  here("online-task", "analyses", "Data", "adaptive_control.csv"),
  here("images", "IPNP", "IPNP_spreadsheet_synonyms.csv"),
  here("online-task", "analyses", "Data", "adaptive_control_demographics.csv")) %>%
  map(fread) %>%
  reduce(left_join) %>%
  right_join(pcpts_with_all_audio_saved) %>% filter(Trial == 0 | Trial == 1) %>% group_by(Sub_Code) %>% summarise(first_start = min(Trial_Start),
                                                                                   last_start = max(Trial_Start)) %>%
  ungroup() %>%
  mutate(start_diff = last_start - first_start) %>%
  arrange(first_start) %>%
  View()