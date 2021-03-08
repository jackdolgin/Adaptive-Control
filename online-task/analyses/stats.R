# Import Packages and Load Helper Functions and Data ----------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(hutils, gtools, tidyverse, furrr, here, magrittr, broomExtra,
               lme4, lmerTest, cowplot, png)
devtools::source_gist("746685f5613e01ba820a31e57f87ec87")

analyses_dir <- here("online-task", "analyses")

source(here(analyses_dir, "graph_builders.R"))

cleaned_df <- read_csv(here(analyses_dir, "cleaned.csv"))


function(precise_responses, first_block_only){
  
  
  # Trim Cleaned Dataframe, Subtract out Neutral Condition, and Graph -----
  
  semi_trimmed_df <- cleaned_df %>%
    mutate(
      across(
        Accuracy,
        ~pmap_lgl(list(precise_responses, ., Dominant_Response, transcript,
                       Label, Congruency),
                  function(precise_responses, Acc, Dominant_Response,
                           transcript, Label, Congruency) {
                    if_else(precise_responses, Acc, case_when(
                      transcript == Label & Congruency == "Incongruent" ~ FALSE,
                      transcript == Dominant_Response ~ TRUE,
                      TRUE ~ NA))
                  })
      ),
      Keep_Block = if_else(first_block_only & Block > 1, FALSE, TRUE)
    ) %>%
    filter(Keep_Block)
  
  cleaned_and_filtered_df <- semi_trimmed_df %>%
    group_by(Sub_Code, Block) %>%
    mutate(across(c(Accuracy, Timely_Response), lag, .names = "{.col}_lag")) %>%
    filter(
      row_number() > 1,                                                         # remove the first trial of each block
      Accuracy,
      Accuracy_lag,
      First_Word_Use,
      Timely_Response,
      Timely_Response_lag,
      preciseStartTime >= .3                                                    # if onset detection thinks audio was any quick than this, it must be wrong
    ) %>%
    ungroup()
  
  neutral_timings <- cleaned_and_filtered_df %>%
    filter(Task == "Neutral") %>%
    group_by(Dominant_Response) %>%
    summarise(RT_stim_avg = mean(preciseStartTime, na.rm = TRUE))
  
  predictive_tasks_df <- cleaned_and_filtered_df %>%
    filter(str_detect(Task, "Predictive")) %>%
    left_join(neutral_timings) %>%
    mutate(
      RT = preciseStartTime - RT_stim_avg,
      # RT = preciseStartTime,
      Bias = coalesce(Block_Bias, Task_Side_Bias),
      across(RT, list(lin_transformed = ~ . - min(.) + 1))
    )
  
  predictive_df_prevRT_filtered <- predictive_tasks_df %>%
    mutate(prev_RT = if_else(Trial - 1 == lag(Trial), lag(RT), NA_real_)) %>%
    filter(!is.na(prev_RT)) %>%
    mutate(across(prev_RT, scale))
  
  # split_violin_builder()
  ggsave(split_violin_builder("manuscriptr"),
         file = here("online-task", "analyses", "test.svg"),
         width = 10.2, height = 9)

  
  # Tabulate Results ------------------------------------------------------
  
  pull_in_from_google5 <- grouped_tidy(
    predictive_df_prevRT_filtered,
    grouping.vars = Task,
    ..f = lmer,
    formula = RT ~ Congruency * Bias * prev_RT + (1 | Sub_Code),
    control = lmerControl(optimizer = "bobyqa")
  )

  # shapi
  
  # Tabulate Methods Section Information ----------------------------------
  
  semi_trimmed_df <- semi_trimmed_df %>%
    group_by(Sub_Code, Block) %>%
    filter(row_number() != 1) %>%
    ungroup()
  
  possible_tot_trials <- semi_trimmed_df %>%
    tidyr::expand(Sub_Code, Trial) %>%
    nrow
  
  
  # Percentage of non- full screen trials
  
  possible_tot_trials - nrow(semi_trimmed_df)
  
  
  # Response Accuracies
  
  semi_trimmed_df %>%
    count(Accuracy) %>%
    mutate(percent = n / all_full_screen)
  
  
  # Percentage using the same word a second time
  
  correct_trials <- filter(semi_trimmed_df, Accuracy, lag(Accuracy))
  
  first_word_trials <- filter(correct_trials, First_Word_Use)
  
  correct_trials %>%
    nrow %>%
    subtract(nrow(first_word_trials)) %>%
    divide_by(nrow(correct_trials))
  
  
  # Percentage of late answer or no timing detected
  
  correct_trials %>%
    count(Timely_Response, preciseStartTime >= .3) %>%
    mutate(percent = n / nrow(correct_trials))
  
  
  
  # Percentage of early answers or late responses or trials preceded by such
  
  correct_trials %>%
    count(Timely_Response & lag(Timely_Response)
          & preciseStartTime >= .3 & lag(preciseStartTime >= .3)) %>%
    mutate(percent = n / nrow(correct_trials))
  
  # Percentage of trials preserved
  
  cleaned_and_filtered_df %>%
    nrow %>%
    divide_by(possible_tot_trials)
  
  
  # Average response time
  
  cleaned_and_filtered_df %>%
    group_by(Task) %>%
    summarise(mean(preciseStartTime))
}



raw_df %>%
  filter(Trial > 0) %>%
  group_by(Sub_Code, Block) %>%
  mutate(Trial_Duration = Trial_Start - lag(Trial_Start)) %>%
  ungroup() %>%
  arrange(Trial_Duration) %>%
  select(Trial_Duration, Sub_Code, Trial) %>%
  View

# Tabulate Demographics ---------------------------------------------------

demographics <- cleaned_df %>%
  group_by(Sub_Code) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Participants per task
count(demographics, Task)

# Age per task
demographics %>%
  group_by(Task) %>%
  summarise(across(Age, list(mean = ~mean(., na.rm = TRUE),
                             sd = ~sd(., na.rm = TRUE))))

# Age across tasks
summarise(demographics,
          across(Age, list(mean = ~mean(., na.rm = TRUE),
                           sd = ~sd(., na.rm = TRUE))))

# Gender per task
count(demographics, Task, Gender)

# Gender across tasks
count(demographics, Gender)

# Ethnicity per task
count(demographics, Task, Ethnicity)


# Assorted ----------------------------------------------------------------

# Dimensions of the refitted IPNP images

cleaned_df %>%
  pull(Image_Path) %>%
  unique() %>%
  map_dfr(function(x) {
    readPNG(here("images", x)) %>%
      dim %>%
      as_tibble_row(.name_repair = ~ make.names(c("height", "width")))
  }) %>%
  summarise(across(everything(), list(min = min, max = max)))


# Difference between behavioral data and recording, by block number

cleaned_df %>%
  group_by(Sub_Code, Block) %>%
  filter(row_number() == 1) %>%
  group_by(Block) %>%
  summarise(across(c(Recording_Duration, Time_Skipped), mean)) %>%
  mutate(Tots = Recording_Duration + Time_Skipped,
         across(where(is.numeric), as.character))

raw_imported %>%
  group_by(Sub_Code, Block) %>%
  mutate(
    across(Nationality, ~if_else(is.na(.) | . == "O", "en-US", .)),
    Time_Skipped = max(Stim_Offset) %>%
      subtract(min(Trial_Start)) %>%
      divide_by(1000) %>%
      subtract(Recording_Duration),
    Next_Lead = lead(Stim_Onset, default = 9999999)) %>%
  ungroup() %>%
  filter(Next_Lead != 9999999,
         ! Trial %in% c(1, 63, 125, 187)) %>%
  select(Trial_Start, Fix_Onset, Fix_Offset, Stim_Onset, Stim_Offset, Next_Lead,
         Recording_Duration, Time_Skipped, Sub_Code, Block, Trial) %>% 
  mutate(Time_Diff = (Next_Lead - Stim_Onset)) %>%
  group_by(Sub_Code, Block) %>%
  mutate(max_Time_Diff = max(Time_Diff),
         mean_Time_Diff = mean(Time_Diff)) %>%
  ungroup() %>%
  arrange(Time_Diff) %>%
  ungroup() %>%
  mutate(Fix_Onset_Diff = Fix_Onset - Trial_Start,
         Fix_Offset_Dif = Fix_Offset - Fix_Onset,
         Stim_Onset_Diff = Stim_Onset - Fix_Offset,
         Stim_Offset_Diff = Stim_Offset - Stim_Onset,
         Trial_Start_Diff = Next_Lead - Trial_Start,
         across(c(Fix_Onset_Diff, Fix_Offset_Dif, Stim_Onset_Diff,
                  Stim_Offset_Diff,Trial_Start_Diff), ~. - median(.) )) %>%
  group_by(Sub_Code, Block) %>%
  filter(row_number() == 1) %>%
  View()
