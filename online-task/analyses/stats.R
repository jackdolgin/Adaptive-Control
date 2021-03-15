# Import Packages and Load Helper Functions and Data ----------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(gtools, tidyverse, furrr, here, magrittr, lme4, lmerTest,
               broomExtra, cowplot, png)
devtools::source_gist("746685f5613e01ba820a31e57f87ec87")

analyses_dir <- here("online-task", "analyses")

source(here(analyses_dir, "graph_builders.R"))

no_cores <- availableCores() - 1L
plan(multisession, workers = no_cores)

cleaned_df <- read_csv(here(analyses_dir, "cleaned.csv"))


stats_graph_and_methods <- function(exact_resp_req, keep_first_block,
                                    keep_last_three_blocks, cautious_regr){
  
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
    summarise(RT_stim_avg = mean(RT))
  
  finished_df <- cleaned_and_filtered_df %>%
    filter(str_detect(Task, "Predictive")) %>%
    left_join(neutral_timings) %>%
    mutate(
      RT_diff = RT - RT_stim_avg,
      Bias = coalesce(Block_Bias, Task_Side_Bias)
    ) %>%
    group_by(Task) %>%
    mutate(across(prev_RT, scale),
           unique_block = paste(Sub_Code, Block, sep = "_"))

  
  # Graph and Stats -------------------------------------------------------
    
  regress_both_tasks <- function(df, keys, dv){
    
    dv_and_condition <- paste(dv, first(keys), sep = "_")
    
    cautious_rand_effects <- case_when(
      str_detect(dv_and_condition, "Predictive_Blocks") ~
        "+ (1 | Block)",
      str_detect(dv_and_condition, "Predictive_Locations") ~
        "+ (1 | unique_block)"
    ) %>%
      rep(cautious_regr)

    if (dv == "RT_diff") {
      lm_reg <- if_else("Bias" %in% colnames(keys), "~ 1", "~ Bias") %>%
        paste(dv, ., "+ (1 | Sub_Code)", cautious_rand_effects) %>%
        lmer(df)
      
      row_append(list(lm_reg), dv_and_condition)
      
      tidy(lm_reg)
      
    } else if (dv == "RT") {
      "~ Congruency * Bias * prev_RT + (1 | Sub_Code)" %>%
        paste(dv, ., "+ (1 | RT_stim_avg)", cautious_rand_effects) %>%
        glmer(
          df,
          Gamma(link="identity"),
          glmerControl(optimizer = "bobyqa")
        ) %>%
        list %>%
        row_append(dv_and_condition)
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
    group_walk(~ regress_both_tasks(.x, .y, "RT"))# %>%
    # list %>%
    # row_append("stats_table")
    # stargazer

  
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
    mutate(across(everything(), list(sig_count = ~ str_count(., "\\*"))))
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
  mutate(future_pmap_dfr(., stats_graph_and_methods),
         map2_dfr(RT_Predictive_Blocks, "Blocks", get_sigs),
         map2_dfr(RT_Predictive_Locations, "Locations", get_sigs),
         across(starts_with("Blocks"), ~ ifelse(keep_last_three_blocks,         # Ignore cells where we're both controlling for participant but only including...
                                               ., NA_character_)))              # ...one block, so the predictive block condition no longer makes sense

results %>%
  count(across(c(starts_with("Blocks") & ends_with("count"),
                 Locations_Congruency__x__Bias_sig_count))) %>%
  drop_na() %>%
  nrow %>%
  equals(1L)


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

