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
  predictive_context(quo(Block), "Predictive_Blocks") %>%
  predictive_context(quo(Task_Side), "Predictive_Locations") %>%
  ungroup() %>%
  mutate(across(c(Synonyms),
                ~if_else(Dominant_Response == Label,
                         paste(Label, ., sep = ", "),
                         paste(Dominant_Response, Label, ., sep = ", "))))

transcriber <- function(x, y, z, b) {
  gl_speech(x,
            languageCode = y,
            speechContexts =
              list(phrases = strsplit(z, ',') %>%
                     unlist %>%
                     trimws),
            customConfig = list(model = b)
  )
}

pull_in_from_google <- prep_data %>%
  pmap_df(function(Stim_Onset, Trial, End_recording,
                   Sub_Code, Synonyms, Nationality,
                   Full_Audio_Path, ...){
    
    spliced_audio_dir <- path(audio_dir, "spliced", Sub_Code) %T>%
      dir_create %>%
      path(paste0("Trial_", Trial, ".wav"))

    readWave(Full_Audio_Path,
             from = Stim_Onset, to = End_recording, units = "seconds") %T>%
      writeWave(spliced_audio_dir, extensible = FALSE)
    # transcribed_list <- transcriber(spliced_audio_dir, Nationality, Synonyms, "command_and_search")
    transcribed_list <- tryCatch({
      transcriber(spliced_audio_dir, Nationality, Synonyms, "command_and_search")
    }, error = function(e) {
      transcriber(spliced_audio_dir, Nationality, Synonyms, "default")
    })
    
    if (length(transcribed_list$timings) > 0){
      precise_timing <- spliced_audio_dir %>%
        read.wav(filter = list(high = 4000)) %>%
        onsets
      
      transcribed_list$transcript %>%
        mutate_at(vars(transcript), trimws) %>%
        mutate(
          Sub_Code,
          Trial,
          preciseStartTime = pluck(precise_timing, first, "start"),
          preciseEndTime = pluck(precise_timing, last, "end"))
    }
    Sys.sleep(1)
  }) %>%
  # right_join(prep_data) %T>% # double check that i want to use right join, especially if there's missing spoken words
  write_csv(here(data_dir, "pull_in_from_google.csv")) #%>%


pull_in_from_google2 <- here(data_dir, "pull_in_from_google.csv") %>%
  read_csv %>%
  group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # so that each trial only takes up 1 row,...
                   -confidence)) %>%                                            # though each trial might only be one row (also some nations' lack a confidence rating)...
  summarise(across(transcript, ~paste0(., collapse = "")),                      # anyways; depends on whether Google spits...
            across(preciseStartTime, min),                                      # out multiple rows for $transcript
            across(preciseEndTime, max)) %>%                                    
  ungroup() %>%
  mutate_at(vars(Dominant_Response, Label, Synonyms, transcript),
            ~str_remove_all(., " ") %>% tolower) %>%
  mutate(Accuracy = pmap_lgl(., function(
    Synonyms, transcript, Label, Congruency, ...){
    case_when(
      transcript == Label & Congruency == "Incongruent" ~ FALSE,
      transcript %in% pluck(strsplit(Synonyms, ','), 1) ~ TRUE,
      TRUE ~ NA)})) %>%
  arrange(Sub_Code, Trial) %>%
  group_by(Sub_Code, Block) %>%
  mutate(
    Prev_Congruency = lag(Congruency),
    Prev_Accuracy = lag(Accuracy)) %>%
  ungroup() %>%
  filter(Accuracy,
         Prev_Accuracy)

Neutral_Timings <- pull_in_from_google2 %>%
  # filter(Task == "Neutral") %>%     # need to uncomment this later, because neutral trials should only be extracted from Task == "Neutral"
  group_by(Dominant_Response) %>%
  summarise(RT = mean(preciseStartTime, na.rm = TRUE))

pull_in_from_google3 <- pull_in_from_google2 %>%
  filter(str_detect(Task, "Predictive"),
         !is.na(preciseStartTime)) %>%
  mutate(
    RT_comparison = map_dbl(
      Dominant_Response,
      ~filter(Neutral_Timings, Dominant_Response == .) %>% pull(RT)),
    RT_diff = preciseStartTime - RT_comparison,
    Bias = coalesce(Block_Bias, Task_Side_Bias))

pull_in_from_google3 %>%
  ggplot(aes(x = Congruency, y = preciseStartTime, fill = Bias)) +
  geom_split_violin(alpha = .5, show.legend = FALSE) +
  geom_boxplot(width = .2, position = position_dodge(.25), outlier.shape = NA,
               alpha = .3, aes(color = Congruency), size = .5) +
  scale_fill_manual(values=c("#D55E00", "#0072B2")) +
  scale_color_manual(values=c("#D55E00", "#0072B2")) +
  theme_bw()


pull_in_from_google3 %>%
  ggplot(aes(x = NA, y = preciseStartTime, fill = Bias)) +
  geom_split_violin(alpha = .5, show.legend = FALSE) +
  geom_boxplot(width = .2, position = position_dodge(.25), outlier.shape = NA,
               alpha = .3, aes(color = Congruency), size = .5) +
  scale_fill_manual(values=c("#D55E00", "#0072B2")) +
  scale_color_manual(values=c("#D55E00", "#0072B2")) +
  theme_minimal() +
  theme(legend.position="top", legend.box = "horizontal",
        legend.spacing = unit(1, "cm"), panel.grid.major.x=element_blank(),
        axis.text.x=element_blank(), axis.title.x=element_blank()) +
  guides(color = guide_legend("Trial", order = 1),
         fill = guide_legend("Block/Location", order = 2)) +
  facet_wrap(Task ~ Congruency, nrow = 1)
