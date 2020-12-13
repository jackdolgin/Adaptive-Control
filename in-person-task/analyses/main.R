# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(googleLanguageR, googleCloudStorageR, tuneR, fs, data.table,
               DescTools, tidyverse, future, here, magrittr)
pacman::p_load_gh("LiKao/VoiceExperiment")
devtools::source_gist("746685f5613e01ba820a31e57f87ec87")

no_cores <- availableCores() - 1
plan(multicore, workers = no_cores)


here("Authentication_File.json") %T>%
  gl_auth %>%
  gcs_auth

mypath <- here("in-person-task", "Data")


predictive_context <- function(mydf, either_block_or_side, relevant_task){
  mydf %>%
    group_by(Participant, !!either_block_or_side) %>%
    mutate(!!paste0(quo_name(either_block_or_side), "_Bias") := 
             case_when(
               Task != relevant_task ~ NA_character_,
               sum(Congruency == "Congruent") / n() > .5 ~ "Congruent",
               TRUE ~ "Incongruent")
    )
}

prep_data <- dir_ls(mypath, glob = "*.csv", recurse = TRUE) %>%
  map_dfr(fread) %>%
  filter(Trial > 0) %>%
  select(-Date) %>%
  left_join(fread(here("images", "IPNP",
                       "IPNP_spreadsheet_synonyms.csv"))) %>%
  group_by(Participant) %>%
  mutate(
    Congruency = ifelse(Dominant_Response == Label, "Congruent", "Incongruent"),
    End_recording = ifelse(
      lead(Fix_Offset) > Stim_Onset + 58 | row_number() == n(),
      Stim_Onset + 58,
      lead(Fix_Offset, default = last(Fix_Offset) + 58))) %>%
  predictive_context(quo(Block), "Predictive Blocks") %>%
  predictive_context(quo(Task_Side), "Predictive Locations") %>%
  ungroup() %>%
  mutate_at(vars(Synonyms), ~paste(
    ., ifelse(Dominant_Response == Label,
              Label,
              paste(Dominant_Response, Label, sep = ", "))))

pull_in_from_google <- prep_data %>%
  pmap_df(function(Stim_Onset, Trial, End_recording,
                   Participant, Synonyms, ...){
    
    audio_path <- path(mypath, Participant, "audio",
                       paste0("Trial_", Trial, ".wav"))
    
    readWave(dir_ls(path(mypath, Participant, "audio", "full_recording"),
                    glob = "*.wav"),
             from = Stim_Onset, to = End_recording, units = "seconds") %>%
      writeWave(audio_path, extensible = FALSE)
    
    transcribed_list <- gl_speech(audio_path, sampleRateHertz = 44100L,
                                  speechContexts =
                                    list(phrases = strsplit(Synonyms, ',') %>%
                                           unlist %>%
                                           trimws))
    
    if (length(transcribed_list$timings) > 0){
      precise_timing <- audio_path %>% read.wav %>% onsets
      
      transcribed_list$transcript %>%
        mutate_at(vars(transcript), trimws) %>%
        mutate(
          Participant,
          Trial,
          preciseStartTime = pluck(precise_timing, first, "start"),
          preciseEndTime = pluck(precise_timing, last, "end"))
    }
  }) %>%
  right_join(prep_data) %T>%
  write_csv("pull_in_from_google.csv") #%>% 


pull_in_from_google2 <- read_csv("pull_in_from_google.csv") %>%
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
  arrange(Participant, Trial) %>%
  group_by(Participant, Block) %>%
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