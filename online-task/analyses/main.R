# Import Packages and Set Up Directories ----------------------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(hutils, gtools, googleLanguageR, googleCloudStorageR, sound,
               tuneR, fs, DescTools, tidyverse, furrr, here, magrittr,
               broomExtra, lme4, lmerTest, cowplot)
pacman::p_load_gh("LiKao/VoiceExperiment")
devtools::source_gist("746685f5613e01ba820a31e57f87ec87")

no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

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
  filter(! Sub_Code %in% c(                                                     # Remove these participants from the analyzed data
    "TeIzC5Si",                                                                 # second time participating
    "ZaNEATXg",                                                                 # second time participating
    "nYFDenHD",                                                                 # second time participating
    "9z9Be9lj",                                                                 # second time participating
    "9Ka1PGgd",                                                                 # second time participating
    "sTPZJGOH",                                                                 # second time participating
    "5g1a5F9S",                                                                 # second time participating
    "FPiMSr70",                                                                 # second time participating
    "1ADP4oEP",                                                                 # second time participating
    "rd5Rou3C",                                                                 # third time participating
    "gt3skyFD",                                                                 # error issue reading their voice data
    "nE0in8wT",                                                                 # error issue reading their voice data
    "sJiC0naK",                                                                 # voice was consistently low, difficult to analyze
    "qiAx7h9D",                                                                 # didn't take the task seriously
    "OWNpYNOV",                                                                 # poor command of Enligsh
    "tOGmbtQk",                                                                 # too many blank responses, and they didn't seem focused
    "ugpDTAuK",                                                                 # TV on in the background
    "lgPVM78L"                                                                  # child making noise in background and distracting the speaker
    ))                                                                

recording_durations <- path(audio_dir, "full") %>%
  dir_ls(glob = "*.wav") %>%
  as.character %>%
  as_tibble_col("Full_Audio_Path") %>%
  mutate(
    Recording_Duration = future_map_dbl(Full_Audio_Path, ~sound::duration(.x))  # Takes a couple minutes to run
  )


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

list(
  here("online-task", "analyses", "Data", "timing_and_task_log.csv"),
  here("images", "IPNP", "IPNP_spreadsheet_synonyms.csv"),
  here("online-task", "analyses", "Data",
       "demographics.csv")) %>%
  map(read_csv) %>%
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

prep_data <- read_csv(here(data_dir, "prep_data.csv"))

dir_create(path(data_dir, "chunked_transcriptions"))

i_max <- prep_data %>% nrow %>% RoundTo(100, ceiling) %>% divide_by(100)
for (loop_count in 0:(i_max - 1)){
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
          
          readWave(Full_Audio_Path, from = Stim_Onset,
                   to = End_recording, units = "seconds") %T>%
            writeWave(spliced_audio_dir, extensible = FALSE)
          
          transcribed_list <- gl_speech(spliced_audio_dir,
                                        languageCode = Nationality,
                                        speechContexts =
                                          list(phrases = strsplit(Synonyms,
                                                                  ',') %>%
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
                       "chunked_transcriptions",
                       paste0("transcription_pt", loop_count + 1,  ".csv")))
      
      break
    }, errror=function(e) {
      while_count <- while_count + 1
    }
    )
  }
}

transcribed_df <- path(data_dir, "chunked_transcriptions") %>%
  dir_ls %>%
  map_df(read_csv) %>%
  right_join(prep_data) %>%
  write_csv(here(data_dir, "all_transcriptions.csv"))


# Clean speech-to-text data -----------------------------------------------

mutated_transcriptions <- read_csv(here(data_dir, "all_transcriptions.csv")) %>%
  group_by(across(-c(preciseStartTime, preciseEndTime, transcript,              # So that each trial only takes up 1 row
                   confidence))) %>%                                            # Also some nations' English lack a confidence rating in Google's API
  summarise(across(transcript, ~paste0(., collapse = "")),
            across(preciseStartTime, min),
            across(preciseEndTime, max),
            across(confidence, mean)) %>%
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
  mutate(Accuracy = future_pmap_lgl(., function(
    Synonyms, transcript, Label, Congruency, ...){
    
    Synonyms <- StrSplit(Synonyms, ",")
    Synonyms_and_plurals <- c(Synonyms,
                              paste0("a", Synonyms),
                              paste0(Synonyms, "s"),
                              paste0(Synonyms, "es"))
    
    transcript_beginning <- str_sub(transcript, end = str_count(Label))
    
    case_when(
      transcript_beginning == Label & Congruency == "Incongruent" ~ FALSE,
      transcript %in% Synonyms_and_plurals ~ TRUE,
      TRUE ~ NA)})
  )

cleaned_df <- mutated_transcriptions %>%
  arrange(Sub_Code, Trial) %>%
  group_by(Sub_Code, transcript) %>%
  mutate(First_Word_Use = (row_number() == 1)) %>%                              # If someone says the same word for multiple trials, only keep the first trial they said that word and filter out the other times
  group_by(Sub_Code, Block) %>%
  filter(Timely_Response, lag(Timely_Response), Accuracy, lag(Accuracy),
         First_Word_Use, preciseStartTime >= .3) %>%
  ungroup()


# Statistical analyses set up and functions -------------------------------

options(contrasts = c("contr.sum","contr.poly"))

Neutral_Timings <- cleaned_df %>%
  filter(Task == "Neutral") %>%
  group_by(Dominant_Response) %>%
  summarise(RT_stim_avg = mean(preciseStartTime, na.rm = TRUE))

pull_in_from_google3 <- cleaned_df %>%
  filter(str_detect(Task, "Predictive")) %>%
  left_join(Neutral_Timings) %>%
  mutate(
    RT = preciseStartTime - RT_stim_avg,
    # RT = preciseStartTime,
    Bias = coalesce(Block_Bias, Task_Side_Bias)
  )

pull_in_from_google4 <- pull_in_from_google3 %>%
  mutate(prev_RT = if_else(Trial - 1 == lag(Trial), lag(RT), NA_real_)) %>%
  filter(!is.na(prev_RT)) %>%
  mutate(across(prev_RT, scale))

pull_in_from_google5 <- grouped_tidy(
  pull_in_from_google4,
  grouping.vars = Task,
  ..f = lmer,
  formula = RT ~ Congruency * Bias * prev_RT + (1 | Sub_Code),
  control = lmerControl(optimizer = "bobyqa")
)


# Plotting set up and functions -------------------------------------------

t_test_samples_layer <- function(samples) {
  
  extra_column <- "Bias"
  extra_main_effect <- NULL
  
  if (samples == 1) {
    bracket_inner_x <- 1.13
    bracket_width <- .04
    star_dist <- -.15
    group_head <- 2
  } else {
    extra_column %<->% extra_main_effect
    bracket_inner_x <- .86
    bracket_width <- -.07
    star_dist <- .09
    group_head <- 4
  }
  
  bracket_outer_x <- bracket_inner_x + bracket_width
  
  bracket_df <- pull_in_from_google4 %>%
    grouped_tidy(
      grouping.vars = 
        c(Task, Congruency, all_of(extra_column)),
      ..f = lmer,
      formula = 
        paste0("RT ~ ", extra_main_effect, " + (1 | Sub_Code)")
    ) %>%
    filter(!is.na(p.value),
           is.na(lead(p.value)),
           p.value < .05) %>%
    mutate(across(p.value, stars.pval)) %>%
    inner_join(pull_in_from_google4) %>%
    group_by(Task, Congruency, p.value, Bias) %>%
    summarise(across(RT, median),
              x_val = c(bracket_inner_x,
                        bracket_outer_x,
                        bracket_outer_x,
                        bracket_inner_x),
              .groups = "drop_last") %>%
    mutate(
      across(RT, ~if_else(
        is.null(extra_main_effect) & Bias == "Congruent",
        0, .)),
      RT_median = median(RT)) %>%
    filter(! between(row_number(), 3, 6))
  
  stars_df <- bracket_df %>%
    filter(row_number() == 1) %>%
    mutate(across(x_val, ~. - star_dist))
  
  dash_df <- mutate(bracket_df,
                    across(x_val, ~if_else(
                      between(row_number(), 2, 3),
                      . + .1,
                      . + .25)))
  
  dash_df_1 <- filter(dash_df, row_number() < 3)
  dash_df_2 <- filter(dash_df, row_number() >= 3)
  
  list(
    geom_path(bracket_df,
              mapping = aes(x_val, RT),
              inherit.aes = FALSE),
    geom_text(stars_df,
              mapping = aes(x_val, RT_median, label = p.value),
              inherit.aes = FALSE,
              hjust = "right"),
    geom_line(dash_df_1,
              mapping = aes(x_val, RT),
              inherit.aes = FALSE,
              linetype = "dotted"),
    geom_line(dash_df_2,
              mapping = aes(x_val, RT),
              inherit.aes = FALSE,
              linetype = "dotted")
  ) %>%
    head(group_head)
}

ggdraw(pull_in_from_google3 %>%
         ggplot(aes(x = NA,
                    y = RT,
                    fill = Bias)) +
         geom_split_violin(alpha = .5,
                           show.legend = FALSE) + 
         # geom_hline(yintercept = 0, colour = "gray100") +
         t_test_samples_layer(1) + 
         t_test_samples_layer(2) +
         geom_boxplot(aes(color = Congruency),
                      width = .2,
                      position = position_dodge(.25),
                      outlier.shape = NA,
                      alpha = .3,
                      size = .5) +
         scale_fill_manual(values = c("#D55E00",
                                      "#0072B2")) +
         scale_color_manual(values = c("#D55E00",
                                       "#0072B2")) +
         facet_wrap(Task ~ Congruency,
                    nrow = 1,
                    strip.position = "bottom") +
         theme_minimal() +
         theme(
           legend.spacing = unit(1, "cm"),
           panel.grid.major.x = element_blank(),
           # panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
           #                                 colour = "grey"), 
           axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           strip.text.x = element_blank(),
           plot.title = element_text(family = "sans",
                                     vjust = 1,
                                     size = 18,
                                     margin = margin(0 , 0, 15, 0)),
           plot.caption = element_text(vjust = -30),
           plot.margin = unit(c(10, 10, 70, 10), "pt")) +
         guides(
           fill = guide_legend("Block/Location Bias"),
           color = guide_legend("Trial")) +
         labs(
           title = paste("Response Time as a Function of Trial",
                         "and Contextual Congruencies"),
           subtitle = paste("Congruency was paired with a",
                            "contextual feature, either block",
                            "number or stimulus location,",
                            "depending on the task\n"),
           y = "Response Time (compared to control condition, in seconds)",
           caption = str_wrap(
             "Each of the values that compose each of the vertical
             'raincloud'/violin plots is for a given image that was
             presented to a given participant. Specifically, for
             that image, it is a given participant's response time
             minus the average response time when that image was
             presented in the neutral condition.",
             160))
) +
  draw_label("Predictive Blocks", 0.25, 0.86) + # might want to manually even this and the next line out more precisely
  draw_label("Predictive Locations", .68, 0.86) +
  draw_label("Congruent", .145, 0.14, size = 12) +
  draw_label("Incongruent", .35, 0.14, size = 12) +
  draw_label("Congruent", .55, 0.14, size = 12) +
  draw_label("Incongruent", .76, 0.14, size = 12) +
  draw_label("Trial Congruency", .45, .09) +
  draw_line(c(.1, .4), .18) +
  draw_line(c(.5, .8), .18)
  # %>%

#ggsave(file=here("online-task", "analyses", "test.svg"), width=10, height=8)


# Statistical Analyses ----------------------------------------------------



# Checking that the data looks like it generally should -------------------

# how many participants for each task
cleaned_df %>% ungroup() %>% count(Sub_Code, Task) %>% group_by(Task) %>%
  summarise(n())

# more or less the same amount of filtering out for each of three tasks; though check this again once data collection is final
cleaned_df %>% count(Sub_Code, Task) %>% group_by(Task) %>%
  summarise(mean(n))

# for looking at which trials didn't have a response that registered either correctly or incorrectly; requires re-running cleaned_df without the filter at the bottom
cleaned_df %>%
  select(Congruency, Dominant_Resp, Dominant_Response, Fix_Offset, Fix_Onset, Full_Screen, Keep, Label, Stim_Offset, Stim_Onset, Task, Trial_Start, End_recording, Pic_Num, transcript, Synonyms, confidence, Sub_Code, Trial, preciseStartTime, preciseEndTime, Accuracy) %>%
  filter(is.na(Accuracy)) %>%
  group_by(Sub_Code) %>%
  mutate(counter = n()) %>%
  arrange(desc(counter))


# for seeing how many trials each participant didn't finish speaking before the ITI began; requires re-running cleaned_df without the filter at the bottom
cleaned_df %>%
  group_by(Timely_Response) %>%
  summarise(100 * n() / nrow(cleaned_df))

# for looking at percent of correct responses; requires re-running cleaned_df without the filter at the bottom
cleaned_df %>%
  group_by(Accuracy) %>%
  summarise((100 * n()) / nrow(cleaned_df))

# was able to salvage about 88.4% of all trials from participants with data that we kept