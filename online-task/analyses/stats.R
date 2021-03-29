# Import Packages and Load Helper Functions and Data ----------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(gtools, DescTools, tidyverse, furrr, here, magrittr, lme4,
               lmerTest, broomExtra, cowplot, stargazer, png, scales, english)

analyses_dir <- here("online-task", "analyses")

no_cores <- availableCores() - 1L
plan(multisession, workers = no_cores)

cleaned_df <- read_csv(here(analyses_dir, "cleaned.csv"))


output_list_per_analysis_combo <- function(exact_resp_req, keep_first_block,
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
    dplyr::slice(-1L) %>%                                                              # remove the first trial of each block
    filter(Keep_Block) %>%
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
  
  cleaned_and_filtered_df %>%
    filter(Task == "Predictive_Blocks") %>%
    group_by(Congruency, Block_Bias) %>%
    summarise(mean_RT = mean(RT)) %>%
    pivot_wider(names_from = Congruency, values_from = mean_RT) %>%
    mutate(
      # across(where(is_numeric), ~ round(1000 * .)),
      pwalk(., function(Block_Bias, Congruent, Incongruent){
        Incongruent %>%
          subtract(Congruent) %>%
          multiply_by(1000L) %>%
          round %>%
          row_append(paste0(Block_Bias, "_congruency_effect"))
        # 
        # row_append(Incongruent - Congruent,
        #            paste0(Block_Bias, "_congruency_effect"))
      })
    )
  
  neutral_timings <- cleaned_and_filtered_df %>%
    filter(Task == "Neutral") %>%
    group_by(Dominant_Response) %>%
    summarise(baseline_RT = mean(RT), .groups = "drop") %>%
    ungroup()
  
  finished_df <- cleaned_and_filtered_df %>%
    filter(str_detect(Task, "Predictive")) %>%
    left_join(neutral_timings) %>%
    mutate(
      RT_diff = RT - baseline_RT,
      Bias = coalesce(Block_Bias, Task_Side_Bias)
    ) %>%
    group_by(Task) %>%
    mutate(across(prev_RT, scale),
           unique_block = paste(Sub_Code, Block, sep = "_")) %>%
    ungroup()

  
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
  
  finished_df %>%
    mutate(across(Bias, ~ paste("Mostly", Bias))) %>%
    group_by(Task, Congruency, Bias) %>%
    group_modify(~ regress_both_tasks(.x, .y, "RT_diff", "difference"))  %>%
    ungroup() %>%
    filter(effect == "fixed") %>%
    mutate(across(Task, ~if_else(. == "Predictive_Blocks", "LWPC", "CSPC")),
           across(p.value, ~if_else(. < .001, "< .001", paste(round(., 3L)))),
           across(c(estimate, std.error, statistic, df), ~paste(round(., 3L))),
           across(c(p.value, estimate, std.error),
                  ~str_remove(., "0(?=\\.)"))) %>%
    select(-c(effect, group, term)) %>%
    rename(
      Condition = Task,
      `$\\beta$` = estimate,
      `\\emph{SE}` = std.error,
      `\\emph{t}` = statistic,
      `\\emph{DF}` = df,
      `\\emph{p}` = p.value) %>%
    xtable::xtable(
      align = "llll|rrrrr",
      caption = stringr::str_wrap(
        "Linear mixed-effects model estimates of a congruency effect among
      each combination of condition, trial congruency, and typical block
      (in LWPC condition) or location (in CSPC condition) congruency,
      using difference-scored RTs"
      )) %>%
    list %>%
    list %>%
    row_append("table_1")
  
  "manuscript" %>%
    split_violin_builder %>%
    list %>%
    row_append("fig_2")

  # ggsave(split_violin_builder(),
  #        file = here("online-task", "analyses", "test.svg"),
  #        width = 10.2, height = 9)

  
  finished_df %>%
    group_by(Task) %>%
    group_walk(~ regress_both_tasks(.x, .y, "RT", "interaction"))
  
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
  
  n_incorrect_trials <- semi_trimmed_df %>%
    filter(Accuracy == FALSE) %>%
    nrow
  
  n_incorrect_trials %>%
    divide_by(nrow(semi_trimmed_df)) %>%
    percent(.1) %>%
    row_append("perc_wrong")
  
  semi_trimmed_df %>%
    filter(is.na(Accuracy)) %>%
    nrow %>%
    divide_by(nrow(semi_trimmed_df)) %>%
    percent(.1) %>%
    row_append("perc_NAcc")

  correct_trials <- filter(semi_trimmed_df, Accuracy)
  
  correct_trials %>%
    nrow %>%
    divide_by(n_incorrect_trials + nrow(correct_trials)) %>%
    percent(.1) %>%
    row_append("perc_correct")
  
  # Using the same word a second time
  
  first_word_trials <- filter(correct_trials, First_Word_Use)
  
  correct_trials %>%
    nrow %>%
    subtract(nrow(first_word_trials)) %>%
    divide_by(nrow(correct_trials)) %>%
    percent(.1) %>%
    row_append("perc_word_repeat")
  
  
  # Early trials
  
  first_word_trials %>%
    filter(RT < .3) %>%
    nrow %>%
    divide_by(nrow(first_word_trials)) %>%
    percent(.01) %>%
    row_append("perc_too_quick")
  
  first_word_trials %>%
    filter(RT < .3) %>%
    nrow %>%
    as.english %>%
    row_append("total_too_quick")

  
  # Slow trials
  
  first_word_trials %>%
    filter(!Timely_Response) %>%
    nrow %>%
    divide_by(nrow(first_word_trials)) %>%
    percent(.1) %>%
    row_append("perc_too_slow")
  
  
  # No RT detected
  
  first_word_trials %>%
    filter(is.na(RT)) %>%
    nrow %>%
    divide_by(nrow(first_word_trials)) %>%
    percent(.1) %>%
    row_append("perc_no_RT")
  
  
  # Percentage of trials preserved
  
  cleaned_and_filtered_df %>%
    nrow %>%
    divide_by(possible_tot_trials) %>%
    percent(.1) %>%
    row_append("perc_preserved")
  
  # Total trials preserved
  
  cleaned_and_filtered_df %>%
    nrow %>%
    row_append("total_preserved")
  
  
  
  
  # Neutral RT diff between dog and beaver
  
  # neutral_timings %>%
  #   pull(baseline_RT) %>%
  #   #  sd %>%
  #   hist()
  # row_append("neutral_rt_sd")
  
  dog_rt <- neutral_timings %>%
    filter(Dominant_Response == "dog") %>%
    pull(baseline_RT)
  
  neutral_timings %>%
    filter(Dominant_Response == "beaver") %>%
    pull(baseline_RT) %>%
    subtract(dog_rt) %>%
    multiply_by(1000L) %>%
    round %>%
    row_append("beaver_minus_dog")
    
  
  as_tibble_row(running_row)

}

make_term_more_interpretable <- function(term){
  term %>%
    str_replace_all("(In)*(C|c)ongruent:*", "__x__") %>%
    str_remove("__x__$")
}

get_sigs <- function(mixed_effect_model, col_suffix) {
  mixed_effect_model %>%
    tidy %>%
    filter(str_detect(
      term, "(Congruency.*Bias)|.*prev_RT")) %>%
    mutate(
      across(p.value, stars.pval),
      term =  make_term_more_interpretable(term) %>%
        paste(col_suffix, ., sep = "_")) %>%
    pivot_wider(id_cols = c(), names_from = term, values_from = p.value) %>%
    mutate(across(everything(), list(sigstars = ~ str_count(., "\\*"))))
}







# Outputs -----------------------------------------------------------------

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
  # filter(
  #   !exact_resp_req,
  #   keep_first_block,
  #   keep_last_three_blocks,
  #   cautious_regr
  # ) %>%
  mutate(future_pmap_dfr(., output_list_per_analysis_combo))

results_holds_up_diff_analysis_choices <- results %>%
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

results_ignoring_block_1 <- filter(
  results,
  !exact_resp_req,
  !keep_first_block,
  keep_last_three_blocks,
  cautious_regr
)

effect_stats <- function(condition, row_choice,
                         df = results_with_preferred_params){
  df %>%
    pull(condition) %>%
    pluck(1L) %>%
    tidy %>%
    mutate(
      across(c(estimate, std.error, statistic),
             ~ if_else(.^2 < .0001, round(., 3), round(., 2))),
      across(p.value,
             ~ case_when(
               . < .001 ~ "< .001",
               . < .01 ~ paste("=", round(., 3)),
               TRUE ~ paste("=", round(., 2)))
      ),
      across(p.value, ~ str_remove(., "0(?=\\.)"))) %>%
    mutate(term = make_term_more_interpretable(term)) %>%
    group_by(term) %>%
    transmute(effect_stats = paste0(
      "$\\beta$ = ", estimate,
      ", *SE* = ", std.error,
      ", z = ", statistic,
      ", *p* ", p.value
    )) %>%
    filter(term == row_choice) %>%
    pull(effect_stats)
}

table_1 <- results_with_preferred_params %>%
  pull(table_1) %>%
  pluck(1L, 1L)

figure_2 <- results_with_preferred_params %>%
  pull(fig_2) %>%
  pluck(1L)

table_2 <- stargazer(list(
  results_with_preferred_params %>%
    pull(Blocks_interaction) %>%
    pluck(1L),
  results_with_preferred_params %>%
    pull(Locations_interaction) %>%
    pluck(1L)),
  
  # type = "html",
  #note that the argument is "out" not "file"
  # out="star_descriptive.doc",
  
  type = "latex",
  keep.stat = c("n", "ll"),
  #single.row = TRUE,
  align = TRUE,
  title = str_wrap(
    "Estimates of raw RTs regressed in a generalized linear mixed-effects
     model onto trial congruency, block- or location-wise congruency,
     and previous trial RT"),
  header = FALSE,
  dep.var.caption  = "Raw Response Times",
  dep.var.labels   = "By condition",
  column.labels = c("LWPC", "CSPC"),
  model.numbers = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  covariate.labels = c("Trial congruency",
                       "Congruency bias (block or location)",
                       "Previous trial RT",
                       "Trial congruency X Congruency bias",
                       "Trial congruency X Previous trial RT",
                       "Congruency bias X Previous trial RT",
                       "Three-way interaction"))


# Tabulate Demographics ---------------------------------------------------

demographics <- cleaned_df %>%
  group_by(Sub_Code) %>%
  slice_sample() %>%
  ungroup()

age_mean <- demographics %>%
  pull(Age) %>%
  mean(na.rm = TRUE) %>%
  round(1L)

age_sd <- demographics %>%
  pull(Age) %>%
  sd(na.rm = TRUE) %>%
  round(1L)

total_males <- demographics %>%
  filter(Gender == "M") %>%
  nrow


# Assorted ----------------------------------------------------------------

num_drawings_used <- cleaned_df %>%
  pull(Dominant_Response) %>%
  n_distinct()

num_possible_drawings <- here("images", "IPNP", "IPNP_spreadsheet.csv") %>%
  read_csv %>%
  filter(Action_or_Object == "Object") %>%
  nrow

num_main_trials_per_participant <- cleaned_df %>%
  count(Sub_Code) %>%
  pull(n) %>%
  max

num_trials_per_block <- cleaned_df %>%
  count(Sub_Code, Block) %>%
  pull(n) %>%
  max

raw_mturk_df <- read_csv(here(analyses_dir, "raw_data", "raw_from_mturk.csv"))

num_practice_per_participant <- raw_mturk_df %>%
  filter(Block < 0) %>%
  count(Sub_Code) %>%
  pull(n) %>%
  max

non_matching_words <- raw_mturk_df %>%
  filter(Congruency == "Incongruent") %>%
  count(Sub_Code) %>%
  pull(n) %>%
  max



# Dimensions of the refitted IPNP images

pic_heights <- cleaned_df %>%
  pull(Image_Path) %>%
  unique() %>%
  map_dfr(function(x) {
    readPNG(here("images", x)) %>%
      dim %>%
      as_tibble_row(.name_repair = ~ make.names(c("height", "width")))
  }) %>%
  pull(height)

min_pic_height <- min(pic_heights)
max_pic_height <- max(pic_heights)

