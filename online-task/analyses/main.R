# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(googleLanguageR, googleCloudStorageR, tuneR, fs, data.table,
               DescTools, tidyverse, future, here, magrittr)
pacman::p_load_gh("LiKao/VoiceExperiment")
devtools::source_gist("746685f5613e01ba820a31e57f87ec87")

# no_cores <- availableCores() - 1
# plan(multicore, workers = no_cores)
# 

here("Authentication_File.json") %T>%
  gl_auth %T>%
  gcs_auth

data_dir <- here("online-task", "analyses", "Data")
audio_dir <- here(data_dir, "audio")


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
  filter(Block > 0) %>%
  mutate(Full_Audio_Path = map2_chr(Sub_Code, Block, function(x, y) {
    path(audio_dir, "full") %>%
      dir_ls(regexp = paste0(x, "_.*_", y))
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
         ~ifelse(Dominant_Response == Label,
                      Label,
                      paste(Dominant_Response, Label, sep = ", "))))

pull_in_from_google <- prep_data %>%
  pmap_df(function(Stim_Onset, Trial, End_recording,
                   Sub_Code, Synonyms, Nationality,
                   Full_Audio_Path, ...){
    
    spliced_audio_dir <- path(audio_dir, "spliced", Sub_Code) %T>%
      dir_create %>%
      path(paste0("Trial_", Trial, ".wav"))
    
    readWave(Full_Audio_Path,
             from = Stim_Onset, to = End_recording, units = "seconds") %>%
      writeWave(spliced_audio_dir, extensible = FALSE)
    
    transcribed_list <- gl_speech(spliced_audio_dir, sampleRateHertz = 44100L,
                                  languageCode = Nationality,
                                  speechContexts =
                                    list(phrases = strsplit(Synonyms, ',') %>%
                                           unlist %>%
                                           trimws))
    
    if (length(transcribed_list$timings) > 0){
      precise_timing <- spliced_audio_dir %>% read.wav %>% onsets
      
      transcribed_list$transcript %>%
        mutate_at(vars(transcript), trimws) %>%
        mutate(
          Sub_Code,
          Trial,
          preciseStartTime = pluck(precise_timing, first, "start"),
          preciseEndTime = pluck(precise_timing, last, "end"))
    }
  }) %>%
  right_join(prep_data) %T>%
  write_csv(here(data_dir, "pull_in_from_google.csv")) #%>% 


pull_in_from_google2 <- here(data_dir, "pull_in_from_google.csv") %>%
  read_csv %>%
  group_by_at(vars(-preciseStartTime, -preciseEndTime, -transcript,             # so that each trial only takes up 1 row,...
                   -confidence)) %>%                                            # though each trial might only be one row...
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

