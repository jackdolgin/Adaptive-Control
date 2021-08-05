# Import Packages and Load Helper Functions and Data ----------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(gtools, DescTools, tidyverse, furrr, here, magrittr, lme4,
               lmerTest, broomExtra, cowplot, stargazer, png, scales, brms,
               bayestestR, performance, rlang, insight, logspline, future,
               future.batchtools, tidybayes, glue, effectsize, fs, datawizard)

pacman::p_install_gh("stan-dev/cmdstanr")

# install_cmdstan() # uncomment this line if this is the first time you've used cdmstanr on current computer

options(contrasts = c("contr.sum","contr.poly"))

no_cores <- availableCores() - 1L
plan(multisession, workers = no_cores)

analyses_dir <- here("online-task", "analyses")
staging_dir <- here(analyses_dir, "staging")
bayes_dir <- here(staging_dir, "bayes")

cleaned_df <- read_csv(here(analyses_dir, "cleaned.csv"), guess_max = 1e5)

glmer_optimzer <- "bobyqa"

# Global variables used for `bayesian_stats.R`

rerun_find_best_brm <- FALSE
regenerate_bayes_power_analysis <- FALSE
platform <- "local"
startup_iters <- 2500L
main_iters <- 2500L
total_chains <- 4L
looic_se_mult <- 2L
rope_power_min_range <- .05
rope_power_max_range <- .95
potential_dv_distributions <- list(
  list("ex-Gaussian", exgaussian()),
  list("skew normal", skew_normal()),
  list("shifted lognormal", shifted_lognormal())
) %>%
  transpose

output_list_per_analysis_combo <- function(exact_resp_req, keep_first_block,
                                           keep_last_three_blocks,
                                           cautious_regr, preferred_params){
  sources <- c(
    "final_wrangling",
    "methods_info",
    file.path("frequentist", "frequentist_stats"),
    file.path("bayes", "bayesian_stats")
  ) %>%
    paste0(".R") %>%
    here(staging_dir, .)
  
  for (i in seq(sources)) source(sources[i], T) # for some reason trying to do this with the `walk` function didn't work
  
  running_row <- list()
  
  row_append <- function(val, name){
    running_row <<- list_modify(running_row, !!name := val)
  }
  
  # Trim Cleaned Dataframe and Subtract out Neutral Condition -------------
  
  final_wrangling()
  
  
  # Frequentist Stats and Graph -------------------------------------------
  
  frequentist_stats()

  # Bayesian Stats and Graph ----------------------------------------------
  # runs bayesian models for only batch 1 in predictive locations, batch 1 in
  # predictive block, and batch 1 and 2 together in predictive locations

  if (preferred_params) {

    pwalk(
      list(
        c(1, 1, 2),
        c("Blocks", rep("Locations", 2)),
        c("Block", rep("unique_block", 2)),
        c(
          'normal(-.144, .15)',                                                 # based off Spinelli et al. 2019 exp 1b
          rep('normal(-.109, .15)'), 2                                          # based off Bugg et al. 2020 exp 2 inducer set
        ),
        c(
          'normal(.025, .1)',                                                   # based off Spinelli et al. 2019 exp 1b
          rep('normal(0, .1)'), 2                                               # based off Bugg et al. 2020 exp 2 inducer set
        ),
        c(
          'normal(-.024, .05)',                                                 # based off Spinelli et al. 2019 exp 1b
          rep('normal(-.027, .07)'), 2                                          # based off Bugg et al. 2020 exp 2 inducer set
        )
      ),
      bayesian_stats
    )

    # Tabulate Methods Section Information ----------------------------------

    methods_info()
  }
  
  as_tibble_row(running_row)
  
}

make_term_more_interpretable <- function(term){
  term %>%
    str_replace_all("1:?", "__x__") %>%
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
  mutate(
    preferred_params = if_else(
      !exact_resp_req &
        keep_first_block &
        keep_last_three_blocks &
        cautious_regr,
      TRUE,
      FALSE
    )
  ) %>%
  mutate(pmap_dfr(., output_list_per_analysis_combo))

results_hold_up_diff_analysis_choices <- results %>%
  mutate(map2_dfr(Blocks_interaction, "Blocks", get_sigs),
         map2_dfr(Locations_interaction, "Locations", get_sigs),
         across(starts_with("Blocks"), ~ ifelse(keep_last_three_blocks,         # Ignore cells where we're both controlling for participant but only including...
                                                ., NA_integer_))) %>%           # ...one block, so the predictive block condition no longer makes sense
  select(ends_with("sigstars") & !contains("prev_RT")) %>%
  mutate(across(everything(), ~RoundTo(., 3, ceiling))) %>%
  count(across(everything())) %>%
  drop_na %>%
  nrow %>%
  equals(1L)

results_with_preferred_params <- filter(results, preferred_params)

best_bayes_model_same_across_batch_and_task <- results_with_preferred_params %>%
  select(ends_with("model_num")) %>%
  as.numeric %>%
  n_distinct() %>%
  equals(1)

common_bayes_model_distr <- results_with_preferred_params %>%
  select(ends_with("fam")) %>%
  as.character %>%
  unique %>%
  str_replace("_", " ")

common_bayes_model_num <- results_with_preferred_params %>%
  select(ends_with("model_num")) %>%
  as.character %>%
  unique

best_bayes_equation <- reduce2(
  c(
    "~ 0 \\+ Intercept \\+",
    "prev_RT",
    "\\).?\\+",
    "Dominant_Response",
    "Sub_Code",
    "unique_block"
  ),
  c(
    "\\\\sim",
    "previousRT",
    "\\)\\\\\\\\\\\\\\text{+ }",
    "Unique Drawing",
    "Participant",
    "Unique Block \\\\text{ or} Block Number"
  ), 
  ~str_replace_all(..1, ..2, ..3),
  .init = pull(results_with_preferred_params, `2_batches_Locations_formulae`)
) %>%
  paste0("$$\\begin{aligned}", ., "\\end{aligned}$$")

results_ignoring_block_1 <- filter(
  results,
  !exact_resp_req,
  !keep_first_block,
  keep_last_three_blocks,
  cautious_regr
)

effect_stats <- function(condition, measure, row_choice, batch_num,
                         df = results_with_preferred_params){
    
    freq_description <- df %>%
      pull(glue("{condition}_{measure}")) %>%
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
      mutate(inline_freq_stats = paste0(
        "$\\upbeta$ = ", estimate,
        ", *SE* = ", std.error,
        ", z = ", statistic,
        ", *p*", p.value
      )) %>%
      filter(term == row_choice) %>%
      ungroup()
    
    if (identical(df, results_with_preferred_params) &
        identical(measure, "interaction")){
      
      df %>%
        pull(glue("{batch_num}_batches_{condition}_df")) %>%
        pluck(1L) %>%
        mutate(
          across(Parameter, ~ . %>%
                   str_remove("b_") %>%
                   str_replace_all("\\.", ":")),
          across(Parameter, ~if_else(. == "Intercept", "(Intercept)", .)),
          Parameter = make_term_more_interpretable(Parameter),
          inline_bayesian_stats = glue(
            "ROPE = [{format_value(ROPE_low)}, {format_value(ROPE_high)}],",
            " {format_rope(ROPE_Percentage)},",
            " {format_bf(BF)}"
          )
        ) %>%
        inner_join(freq_description, by = c("Parameter" = "term")) %>%
        transmute(inline_output = if_else(
          batch_num == 1L & condition == "Locations",
          inline_bayesian_stats,
          glue("{inline_freq_stats}, {inline_bayesian_stats}")
        )) %>%
        pull()
    
    } else{
      
      pull(freq_description, inline_freq_stats)
      
    }

# results_with_preferred_params %>% pull(Locations_interaction) %>% pluck(1L) %>% tidy %>%
  #   inner_join(read_csv("/Users/jackdolgin/Dropbox/Stages_of_Life/Grad_School/Repos/Adaptive-Control/online-task/analyses/staging/bayes/fits/1_batches_Locations/posterior_description_old_cause_has_shifted_lognormal.csv") %>%
  #                mutate(across(Parameter, ~ . %>%
  #                                str_remove("b_") %>%
  #                                str_replace_all("\\.", ":")))  %>%
  #                mutate(across(Parameter, ~if_else(. == "Intercept", "(Intercept)", .))), by = c("term" = "Parameter"))
  
  
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
    "Estimates of raw RTs regressed in a frequentist generalized linear
     mixed-effects model onto trial congruency, block- or location-wise
     congruency, and previous trial RT"),
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


# Bayesian Summaries ------------------------------------------------------

posterior_df_combined <-map_dfr(c("1_batches_Blocks", "2_batches_Locations"), ~{
  read_csv(here(bayes_dir, "fits", .x, "posterior_description.csv")) %>%
    mutate(Task_and_Batch = .x)
})



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

