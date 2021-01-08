# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(googleLanguageR, googleCloudStorageR, tuneR, fs, data.table,
               DescTools, tidyverse, here, magrittr)
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
  select(Sub_Code)

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
  group_by(Sub_Code, Block) %>%
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
  filter(!0 %in% c(Audio_Connected, lag(Audio_Connected), lead(Audio_Connected),
                   Audio_Permitted, lag(Audio_Permitted), lead(Audio_Permitted),
                   Full_Screen, lag(Full_Screen), lead(Full_Screen))) %>%
  predictive_context(quo(Block), "Predictive_Blocks") %>%
  predictive_context(quo(Task_Side), "Predictive_Locations") %>%
  ungroup() %>%
  mutate(across(c(Synonyms),
                ~if_else(Dominant_Response == Label,
                         paste(Label, ., sep = ", "),
                         paste(Dominant_Response, Label, ., sep = ", "))))

walk(0:22, function(x) {
  prep_data %>%
# pull_in_from_google <- prep_data %>%
  filter(row_number() > x * 100,
         row_number() <= (x * 100) + 100) %>%
  pmap_df(function(Stim_Onset, Trial, End_recording,
                   Sub_Code, Synonyms, Nationality,
                   Full_Audio_Path, ...){
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
    
    transcribed_list <- gl_speech(spliced_audio_dir,
                                  languageCode = Nationality,
                                  speechContexts =
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

# pull_in_from_google2 <- here(data_dir, "pull_in_from_google.csv") %>%
pull_in_from_google2 <- here(data_dir, "pull_in_from_google_new.csv") %>%
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
  group_by(Sub_Code, Block) %>%
  filter(Timely_Response, lead(Timely_Response), lag(Timely_Response),
         Accuracy, lead(Accuracy), lag(Accuracy)) %>%
  ungroup()
 
#  no longer true
# # weird that so many more trials getting removed during filtering from the predictive tasks; need to investigate why
# pull_in_from_google2 %>% count(Sub_Code, Task) 


# the following few lines are temporary until there's enough participants
pull_in_from_google2 <- pull_in_from_google2 %>%
  filter(Task == "Neutral") %>%
  select(Dominant_Response) %>%
  distinct() %>%
  left_join(pull_in_from_google2)


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
      plots is for a given image that was presented across participants. 
      Specifically, for that image (say a banana), it is the average of all the 
      responses to a banana image for that given task and congruency condition. 
      It takes that average and then subtracts from it the average response 
      time when that image was presented in the neutral condition",
      140))) %>%

ggsave(file=here("online-task", "analyses", "test.svg"), width=10, height=8)
