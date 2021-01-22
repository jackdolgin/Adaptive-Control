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
                           "ZaNEATXg", # this WorkerID participated twice
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
        lead(Fix_Offset, default = last(Fix_Offset) + 58)),
    across(c(Audio_Connected, Audio_Permitted, Full_Screen),
           list(lead, lag), .names = "{.col}_{.fn}")
  ) %>%
  filter(across(
    starts_with(c("Audio_Connected", "Audio_Permitted", "Full_Screen")),
    ~is.na(.) | . == 1
    )) %>%
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

dir_create(path(data_dir, "transcriptions"))

i_max <- prep_data %>% nrow %>% RoundTo(100, ceiling) %>% divide_by(100)
for (loop_count in 116:(i_max-1)){
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
        write_csv(here(data_dir,
                       "transcriptions",
                       paste0("transcription_pt", loop_count + 1,  ".csv")))
      
      break
    }, errror=function(e) {
      while_count <- while_count + 1
    }
    )
  }
}


# Clean speech-to-text data -----------------------------------------------

pull_in_from_google2 <- path(data_dir, "transcriptions") %>%
  dir_ls %>%
  map_df(fread) %>%
  right_join(prep_data) %>% #remove this line from final version
  filter(! Sub_Code %in% c("nE0in8wT",                                          # There was a technical issue with their recording
                           "ugpDTAuK")) %>%                                     # TV was on in the background during their recording
  group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # So that each trial only takes up 1 row,...
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
  mutate(First_Word_Use = (row_number() == 1)) %>%                              # If someone says the same word for multiple trials, only keep the first trial they said that word and filter out the other times
  group_by(Sub_Code, Block) %>%
  filter(Timely_Response, lag(Timely_Response), Accuracy, lag(Accuracy),
         First_Word_Use, preciseStartTime >= .3) %>%
  ungroup()


# Plotting set up and functions -------------------------------------------

Neutral_Timings <- pull_in_from_google2 %>%
  filter(Task == "Neutral") %>%
  group_by(Dominant_Response) %>%
  summarise(RT_stim_avg = mean(preciseStartTime, na.rm = TRUE))


pull_in_from_google3 <- pull_in_from_google2 %>%
    filter(str_detect(Task, "Predictive")) %>%
    left_join(Neutral_Timings) %>%
    mutate(RT_diff = preciseStartTime - RT_stim_avg,
           Bias = coalesce(Block_Bias, Task_Side_Bias),
           across(Task, ~str_replace(., "_", " ") %>% paste0(",")),
           across(Congruency, ~paste(., "Trials"))
    )

(pull_in_from_google3 %>%
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

# more or less the same amount of filtering out for each of three tasks; though check this again once data collection is final
pull_in_from_google2 %>% count(Sub_Code, Task) %>% group_by(Task) %>%
  summarise(mean(n))

# for looking at which trials didn't have a response that registered either correctly or incorrectly; requires re-running pull_in_from_google2 without the filter at the bottom
pull_in_from_google2 %>%
  select(Congruency, Dominant_Resp, Dominant_Response, Fix_Offset, Fix_Onset, Full_Screen, Keep, Label, Stim_Offset, Stim_Onset, Task, Trial_Start, Worker_ID, End_recording, Pic_Num, transcript, Synonyms, confidence, Sub_Code, Trial, preciseStartTime, preciseEndTime, Accuracy) %>%
  filter(is.na(Accuracy)) %>%
  group_by(Sub_Code) %>%
  mutate(counter = n()) %>%
  arrange(desc(counter))


# for seeing how many trials each participant didn't finish speaking before the ITI began; requires re-running pull_in_from_google2 without the filter at the bottom
pull_in_from_google2 %>%
  group_by(Timely_Response) %>%
  summarise(100 * n() / nrow(pull_in_from_google2))

# for looking at percent of correct responses; requires re-running pull_in_from_google2 without the filter at the bottom
pull_in_from_google2 %>%
  group_by(Accuracy) %>%
  summarise((100 * n()) / nrow(pull_in_from_google2))

# was able to salvage about 88.4% of all trials from participants with data that we kept