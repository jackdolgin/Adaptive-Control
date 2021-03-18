# Import Packages and Load Helper Functions and Data ----------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(gtools, DescTools, tidyverse, furrr, here, magrittr, lme4,
               lmerTest, broomExtra, cowplot, png)

analyses_dir <- here("online-task", "analyses")

no_cores <- availableCores() - 1L
plan(multisession, workers = no_cores)

cleaned_df <- read_csv(here(analyses_dir, "cleaned.csv"))


stats_graph_and_methods <- function(exact_resp_req, keep_first_block,
                                    keep_last_three_blocks, cautious_regr){
  
  source(here(analyses_dir, "graph_builders.R"), local = TRUE)
  
  running_row <- list()
  
  row_append <- function(val, name){
    running_row <<- list_modify(running_row, !!name := val)
  }
  
  # Trim Cleaned Dataframe and Subtract out Neutral Condition -------------
  
  semi_trimmed_df <- cleaned_df %>%
    mutate(
      across(
        Accuracy,
        ~pmap_lgl(list(exact_resp_req, ., Dominant_Response, transcript,
                       Label, Congruency),
                  function(exact_resp_req, Acc, Dominant_Response,
                           transcript, Label, Congruency) {
                    if_else(!exact_resp_req, Acc, case_when(
                      transcript == Label & Congruency == "Incongruent" ~ FALSE,
                      transcript == Dominant_Response ~ TRUE,
                      TRUE ~ NA))
                  })
      ),
      Keep_Block = if_else(
        (Block == 1L & keep_first_block) |
          (Block > 1L & keep_last_three_blocks),
        TRUE, FALSE
      )
    ) %>%
    group_by(Sub_Code, Block) %>%
    mutate(across(c(Accuracy, Timely_Response, RT, First_Word_Use),
                  lag, .names = "prev_{.col}")) %>%
    filter(row_number() > 1L,                                                   # remove the first trial of each block
           Keep_Block) %>%
    ungroup()
  
  cleaned_and_filtered_df <- filter(
    semi_trimmed_df,
    Accuracy,
    prev_Accuracy,
    First_Word_Use,
    prev_First_Word_Use,
    RT >= .3,                                                                   # if onset detection thinks audio was any quicker than this, it must be off or wrong; also removes trials where no RT was detected
    prev_RT >= .3,
    Timely_Response,
    prev_Timely_Response
  )
  
  neutral_timings <- cleaned_and_filtered_df %>%
    filter(Task == "Neutral") %>%
    group_by(Dominant_Response) %>%
    summarise(baseline_RT = mean(RT))
  
  dog_rt <- neutral_timings %>%
    filter(Dominant_Response == "dog") %>%
    pull(baseline_RT)

  neutral_timings %>%
    filter(Dominant_Response == "beaver") %>%
    pull(baseline_RT) %>%
    subtract(dog_rt) %>%
    multiply_by(1000) %>%
    round
  
  finished_df <- cleaned_and_filtered_df %>%
    filter(str_detect(Task, "Predictive")) %>%
    left_join(neutral_timings) %>%
    mutate(
      RT_diff = RT - baseline_RT,
      Bias = coalesce(Block_Bias, Task_Side_Bias)
    ) %>%
    group_by(Task) %>%
    mutate(across(prev_RT, scale),
           unique_block = paste(Sub_Code, Block, sep = "_"))

  
  # Graph and Stats -------------------------------------------------------
    
  regress_both_tasks <- function(df, keys, dv, analysis_type){
    
    condition <- str_extract(first(keys), "(?<=_).*")
    
    cautious_rand_effects <- case_when(
      condition == "Blocks" ~ "+ (1 | Block)",
      condition == "Locations" ~ "+ (1 | unique_block)"
    ) %>%
      rep(cautious_regr)
    
    if (analysis_type == "difference") {
      equation <- if_else("Bias" %in% colnames(keys), "~ 1", "~ Bias") %>%
        paste(dv, ., "+ (1 | Sub_Code)", cautious_rand_effects)
      
    } else if (analysis_type == "interaction") {
      equation <- "~ Congruency * Bias * prev_RT + (1 | Sub_Code)" %>%
        paste(dv, ., "+ (1 | baseline_RT)", cautious_rand_effects)
    }
    
    if (dv == "RT_diff") {
      equation %>%
        lmer(df) %>%
        tidy
    } else if (dv == "RT") {
      
      fitted <-glmer(
        equation,
        df,
        Gamma(link="identity"),
        glmerControl(optimizer = "bobyqa")
      )
      
      if (analysis_type == "interaction"){
        fitted %>%
          list %>%
          row_append(paste(condition, analysis_type, sep = "_")) 
        
      } else if (analysis_type == "difference"){
        soon_to_be_col_name <- paste(
          condition, "bias", analysis_type, pull(keys, 2), sep = "_")
        
        fitted %>%
          list %>%
          row_append(soon_to_be_col_name)
        
        fitted %>%
          tidy %>%
          filter(str_detect(term, "Bias")) %>%
          pull(p.value) %>%
          stars.pval %>%
          str_count("\\*") %>%
          row_append(paste0(soon_to_be_col_name, "_sigstars"))
      }
    }
  }
  
  "manuscript_not" %>%
    split_violin_builder %>%
    list %>%
    row_append("plot")

  # ggsave(split_violin_builder(),
  #        file = here("online-task", "analyses", "test.svg"),
  #        width = 10.2, height = 9)

  
  finished_df %>%
    group_by(Task) %>%
    group_walk(~ regress_both_tasks(.x, .y, "RT", "interaction"))# %>%
    # list %>%
    # row_append("stats_table")
    # stargazer
  
  finished_df %>%
    group_by(Task, Congruency) %>%
    group_walk(~ regress_both_tasks(.x, .y, "RT", "difference"))

  
  # Tabulate Methods Section Information ----------------------------------
  
  possible_tot_trials <- semi_trimmed_df %>%
    tidyr::expand(Sub_Code, Trial) %>%
    nrow %T>%
    row_append("possible_tot_trials")
  
  
  # Total non- full screen trials
  
  possible_tot_trials %>%
    subtract(nrow(semi_trimmed_df)) %>%
    divide_by(2L) %>%
    row_append("non_full_screen")
  
  
  # Response Accuracies
  
  semi_trimmed_df %>%
    filter(Accuracy == FALSE) %>%
    nrow %>%
    divide_by(nrow(semi_trimmed_df)) %>%
    row_append("perc_wrong")
  
  semi_trimmed_df %>%
    filter(is.na(Accuracy)) %>%
    nrow %>%
    divide_by(nrow(semi_trimmed_df)) %>%
    row_append("perc_NAcc")

  correct_trials <- filter(semi_trimmed_df, Accuracy)
    
  
  # Using the same word a second time
  
  first_word_trials <- filter(correct_trials, First_Word_Use)
  
  correct_trials %>%
    nrow %>%
    subtract(nrow(first_word_trials)) %>%
    divide_by(nrow(correct_trials)) %>%
    row_append("word_repeat")
  
  
  # Early trials
  
  first_word_trials %>%
    filter(RT < .3) %>%
    nrow %>%
    divide_by(nrow(first_word_trials)) %>%
    row_append("too_quick")

  
  # Slow trials
  
  first_word_trials %>%
    filter(!Timely_Response) %>%
    nrow %>%
    divide_by(nrow(first_word_trials)) %>%
    row_append("too_slow")
  
  
  # No RT detected
  
  first_word_trials %>%
    filter(is.na(RT)) %>%
    nrow %>%
    divide_by(nrow(first_word_trials)) %>%
    row_append("no_RT")
  
  
  # Percentage of trials preserved
  
  cleaned_and_filtered_df %>%
    nrow %>%
    divide_by(possible_tot_trials) %>%
    row_append("preserved")
  
  as_tibble_row(running_row)

}


get_sigs <- function(mixed_effect_model, col_suffix) {
  mixed_effect_model %>%
    tidy %>%
    filter(str_detect(
      term, "(Congruency.*Bias)|.*prev_RT")) %>%
    mutate(
      across(p.value, stars.pval),
      term = term %>%
        str_replace_all("(In)*(C|c)ongruent:*", "__x__") %>%
        str_remove("__x__$") %>%
        paste(col_suffix, ., sep = "_")) %>%
    pivot_wider(id_cols = c(), names_from = term, values_from = p.value) %>%
    mutate(across(everything(), list(sigstars = ~ str_count(., "\\*"))))
}


results <- c("exact_resp_req",
  "keep_first_block",
  "keep_last_three_blocks",
  "cautious_regr"
) %>%
  purrr::set_names(.) %>%
  map(~ c(TRUE, FALSE)) %>%
  expand.grid %>%
  as_tibble %>%
  filter(!(keep_first_block & cautious_regr & !keep_last_three_blocks),
         or(keep_first_block, keep_last_three_blocks)) %>%
  mutate(future_pmap_dfr(., stats_graph_and_methods))

results %>%
  mutate(map2_dfr(Blocks_interaction, "Blocks", get_sigs),
         map2_dfr(Locations_interaction, "Locations", get_sigs),
         across(starts_with("Blocks"), ~ ifelse(keep_last_three_blocks,         # Ignore cells where we're both controlling for participant but only including...
                                                ., NA_integer_))) %>%           # ...one block, so the predictive block condition no longer makes sense
  select(ends_with("sigstars") &
           (starts_with("Block") | !contains("prev_RT"))) %>%
  mutate(across(everything(), ~RoundTo(., 3, ceiling))) %>%
  count(across(everything())) %>%
  drop_na %>%
  nrow %>%
  equals(1L)

results_with_preferred_params <- filter(
  results,
  !exact_resp_req,
  keep_first_block,
  keep_last_three_blocks,
  cautious_regr
)

results_with_preferred_params %>%
  pull(Blocks_interaction) %>%
  pluck(1) %>%
  tidy


stargazer::stargazer(data = list(
  results_with_preferred_params %>%
    pull(Blocks_interaction) %>%
    pluck(1),
  results_with_preferred_params %>%
    pull(Locations_interaction) %>%
    pluck(1)),
  type = "latex", 
          header = FALSE, title = "Regression Results", 
          keep.stat = c("n", "rsq"))




# need to compare standard errors i guess, make sure it's just a lagged onset time in my experiment; could break my data into the first 62 trials for each participant, and look at the sd among those first 62 for mostly congruent or mostly incongruent first blocks; and compare that with the first 62 trial for Spinelli's participants (or at least a random selection of his participants so his group size is the same as mine (or maybe if i use standard error i can include ll his participants))
picnam_RT %>% group_by(list_type) %>% summarise(meanRT = sd(RT)) %>% arrange(list_type, meanRT) %>% View

# quite a bit of variation in how long the recordings last, even though they should all last exactly the same amount
cleaned_df %>% group_by(Sub_Code, Block) %>% summarise(Recording_Duration = mean(Recording_Duration)) %>% arrange(Recording_Duration) %>% View 

# but it looks like the block are actually taking a similar amount of time as the audio; it's not necessarily (or just) the audio; why would blocks take different amounts of time? and how consistent are these block durations for each person?
raw_df %>%
  group_by(Sub_Code, Block) %>%
  summarise(Time_Skipped = max(Stim_Offset) %>%
              subtract(min(Trial_Start)) %>%
              divide_by(1000)) %>% arrange(Time_Skipped) %>% View

# people with a full screen off at one point encompass more of the instances of inconsistent durations across blocks for a given participant, but still doesn't explain all of the big duration diffs; still, looks like a lot of the differences are between rather than within subject; next i'll want to look at how the recording durations compare to the behavioral block length; also, are there specific trials that are taking longer that explain the diff in trial length? are the diffs present for all trials
read_csv("/Users/jackdolgin/Dropbox/Stages of Life/Grad School/Repos/Adaptive-Control/online-task/analyses/transcribe/transcriptions_merged.csv") %>%
  group_by(Sub_Code) %>%
  summarise(n = n_distinct(Trial)) %>%
  arrange(n) %>% inner_join( raw_df %>%
                               filter(Block > 0) %>%
                               group_by(Sub_Code, Block) %>%
                               summarise(Time_Skipped = max(Stim_Offset) %>%
                                           subtract(min(Trial_Start)) %>%
                                           divide_by(1000)) %>% 
                               group_by(Sub_Code) %>%
                               summarise(Diff_Durations = max(Time_Skipped) - min(Time_Skipped))
  ) %>%
  arrange(Diff_Durations) %>% View


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
  filter(row_number() == 1L) %>%
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


adf %>% pull(RT_Predictive_Blocks) %>%
  pluck(1) %>%
  tidy() %>%
  filter(grepl(
    "(Congruency.*Bias)|.*prev_RT", term )) %>%
  mutate(term = term %>%
           str_replace_all("(In)*(C|c)ongruent:*", "__x__") %>%
           str_remove(., "__x__$")) %>%
  pivot_wider(id_cols = c(), names_from = term, values_from = p.value)
