# Import Packages and Load Helper Functions and Data ----------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(gtools, DescTools, tidyverse, furrr, here, magrittr, lme4,
               lmerTest, broomExtra, cowplot, stargazer, png, scales, brms,
               bayestestR, performance, rlang, insight, logspline, future,
               future.batchtools, tidybayes, glue)

pacman::p_install_gh("stan-dev/cmdstanr")

# install_cmdstan() # uncomment this line if this is the first time you've used cdmstanr on current computer

analyses_dir <- here("online-task", "analyses")
staging_dir <- here(analyses_dir, "staging")

cleaned_df <- read_csv(here(analyses_dir, "cleaned.csv"))


output_list_per_analysis_combo <- function(exact_resp_req, keep_first_block,
                                           keep_last_three_blocks, cautious_regr,
                                           preferred_params, rerun_find_best_brm,
                                           regenerate_bayes_power_analysis, platform){
  
  c(
    "final_wrangling",
    "methods_info",
    file.path("frequentist", "frequentist_stats"),
    file.path("bayes", "bayesian_stats")
  ) %>%
    paste0(".R") %>%
    here(staging_dir, .) %>%
    walk(~source(.x, local = TRUE))
  
  
  running_row <- list()
  
  row_append <- function(val, name){
    running_row <<- list_modify(running_row, !!name := val)
  }
  
  # Trim Cleaned Dataframe and Subtract out Neutral Condition -------------
  
  final_wrangling()
  
  
  # Frequentist Stats and Graph -------------------------------------------
  
  frequentist_stats()
  
  
  # Bayesian Stats and Graph ----------------------------------------------
  
  if (!preferred_params) {
    NULL
  } else {
    
    bayesian_stats()
    
    best_brm_model <- pick_brm_model(-1)
    
    posterior_description <- custom_describe_posterior(best_brm_model)
    
    brms::pp_check(best_brm_model)
    
  }    
  
  
  # Tabulate Methods Section Information ----------------------------------
  
  methods_info()
  
  
  
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
  mutate(
    preferred_params = if_else(
      !exact_resp_req &
        keep_first_block &
        keep_last_three_blocks &
        cautious_regr,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      "cluster" # either local or cluster
    ),
    # future_pmap_dfr(., output_list_per_analysis_combo)
    pmap_dfr(., output_list_per_analysis_combo)
  )

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

results_with_preferred_params <- filter(results, preferred_params)

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
      across(c(estimate, std.error), ~format_value(., 3)),
      across(statistic, ~format_value(., 2)),
      across(p.value, format_p),
      across(p.value, ~str_remove(., "p")),
      term = make_term_more_interpretable(term)
    ) %>%
    group_by(term) %>%
    transmute(effect_stats = paste0(
      "$\\beta$ = ", estimate,
      ", *SE* = ", std.error,
      ", z = ", statistic,
      ", *p*", p.value
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

