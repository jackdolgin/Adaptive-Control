# Import Packages and Load Helper Functions and Data ----------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(gtools, DescTools, tidyverse, furrr, here, magrittr, lme4,
               lmerTest, broomExtra, cowplot, stargazer, png, scales, brms,
               bayestestR, performance, rlang, insight, logspline, future,
               cmdstanr, future.batchtools, tidybayes, glue, effectsize, fs,
               datawizard, bayesplot)

pacman::p_install_gh("stan-dev/cmdstanr")
pacman::p_install_gh("IndrajeetPatil/broomExtra")

# install_cmdstan() # uncomment this line if this is the first time you've used cdmstanr on current computer

options(contrasts = c("contr.treatment","contr.poly"))                          # This is already the default, just being explicit about it

bayesplot_theme_update(text = element_text(size = 7))

no_cores <- availableCores() - 1L
plan(multisession, workers = no_cores)

analyses_dir <- here("online-task", "analyses")
staging_dir <- here(analyses_dir, "staging")
bayes_dir <- here(staging_dir, "bayes")

cleaned_df <- read_csv(here(analyses_dir, "cleaned.csv"), guess_max = 1e5)

glmer_optimzer <- "bobyqa"

# Global variables used for `bayesian_stats.R`

key_interaction_param <- "Congruency.+Bias"
rerun_find_best_brm <- FALSE
regenerate_bayes_power_analysis <- FALSE
platform <- "local"
startup_iters <- 2500L
main_iters <- 2500L
total_chains <- 4L
rope_size <- .01                                                                # since we're working in shifted log-normal land
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
  
  row_append <- function(val, name, max_batch = ""){
    if (max_batch != "") name <- paste(name, max_batch, sep = "_")
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
        1000,
        750,                                                                    # weak intercept prior because I don't know how timing of online recordings will be
        0,
        c(
          330,                                                                  # based off Spinelli et al. 2019 exp 1b (using their code from https://osf.io/jnzgb)
          rep(120, 2)                                                           # based off Bugg et al. 2020 exp 2 inducer set + no counting
        ),
        0,
        c(
          0,                                                                    # based off Spinelli et al. 2019 exp 1b (using their code from https://osf.io/jnzgb)
          rep(10), 2                                                            # based off Bugg et al. 2020 exp 2 inducer set + no counting
        ),
        60,
        c(
          -90,                                                                  # based off Spinelli et al. 2019 exp 1b (using their code from https://osf.io/jnzgb)
          rep(-30, 2)                                                           # based off Bugg et al. 2020 exp 2 inducer set + no counting
        )
      ),
      bayesian_stats
    )
    

    # Tabulate Methods Section Information ----------------------------------

    methods_info()
  }
  
  as_tibble_row(running_row)
  
}

make_term_more_interpretable <- function(term, default_replacement = " x "){
  term %>%
    str_replace_all("Incongruent:?", default_replacement) %>%
    str_remove(paste0(default_replacement, "$"))
}

get_sigs <- function(mixed_effect_model, col_suffix) {
  mixed_effect_model %>%
    tidy %>%
    filter(str_detect(term, key_interaction_param)) %>%
    mutate(
      across(p.value, ~if_else(estimate > 0, 1 - .x, .x)),                      # we're testing the interaction term as a 1-sided test, hypothesizing that the estimate it's negative; if the estimate is instead positive, the p-value should be flipped
      across(p.value, stars.pval),
      term =  make_term_more_interpretable(term, "__x__") %>%
        paste(col_suffix, ., sep = "_")) %>%
    pivot_wider(id_cols = c(), names_from = term, values_from = p.value) %>%
    mutate(across(everything(), list(sigstars = ~ str_count(., "\\*"))))
}







# Outputs -----------------------------------------------------------------

# Uncomment to regenerate model fits, otherwise work with stored fits by
# reading in .rds file
# results <- c("exact_resp_req",
#              "keep_first_block",
#              "keep_last_three_blocks",
#              "cautious_regr"
# ) %>%
#   purrr::set_names(.) %>%
#   map(~ c(TRUE, FALSE)) %>%
#   expand.grid %>%
#   as_tibble %>%
#   filter(!(keep_first_block & cautious_regr & !keep_last_three_blocks),
#          or(keep_first_block, keep_last_three_blocks)) %>%
#   mutate(
#     preferred_params = if_else(
#       !exact_resp_req &
#         keep_first_block &
#         keep_last_three_blocks &
#         cautious_regr,
#       TRUE,
#       FALSE
#     )
#   ) %>%
#   mutate(pmap_dfr(., output_list_per_analysis_combo))

results <- read_rds("/Users/jackdolgin/Dropbox/Stages_of_Life/Grad_School/Repos/Adaptive-Control/results_variable_saved.rds")

results_hold_up_diff_analysis_choices <- results %>%
  filter(keep_last_three_blocks) %>%
  mutate(
    map2_dfr(Blocks_interaction, "Blocks", get_sigs),
    map2_dfr(Locations_interaction, "Locations", get_sigs)
    ) %>%
  select(matches(key_interaction_param) & ends_with("sigstars")) %>%
  mutate(across(everything(), ~RoundTo(., 3, ceiling))) %>%
  count(across(everything())) %>%
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
    "\\).?\\+",
    "Dominant_Response",
    "Sub_Code",
    "unique_block"
  ),
  c(
    "\\\\sim",
    "\\)\\\\\\\\\\\\\\text{+ }",
    "Unique Drawing",
    "Participant",
    "Unique Block \\\\text{ or } Block Number"
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
        across(c(estimate, std.error), ~format_value(., 1)),
        across(statistic, ~format_value(., 2)),
        across(p.value, format_p),
        across(p.value, ~str_remove(., "p")),
        term = make_term_more_interpretable(term, "__x__")
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
          Parameter = make_term_more_interpretable(Parameter, "__x__"),
          inline_bayesian_stats = glue(
            "ROPE = [{format_value(ROPE_low_raw)} ms, ",
            "{format_value(ROPE_high_raw)} ms],",
            " {format_rope(ROPE_Percentage)},",
            " {format_bf(exp(log_BF))}"
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

extract_rope <- function(task, low_or_high){
  results_with_preferred_params %>%
    pluck(glue("1_batches_{task}_df"), 1L) %>%
    pull(glue("ROPE_{low_or_high}_raw")) %>%
    median %>%
    round(2)
}

table_1 <- results_with_preferred_params %>%
  pull(table_1) %>%
  pluck(1L, 1L)

figure_2 <- results_with_preferred_params %>%
  pull(fig_2) %>%
  pluck(1L)

blocks_interaction_model_stargazer <- results_with_preferred_params %>%
  pull(Blocks_interaction) %>%
  pluck(1L)
locations_interaction_model_stargazer <- results_with_preferred_params %>%
  pull(Locations_interaction) %>%
  pluck(1L)

table_2 <- stargazer(
  blocks_interaction_model_stargazer,
  locations_interaction_model_stargazer,

  # type = "html",
  #note that the argument is "out" not "file"
  # out="star_descriptive.doc",

  type = "latex",
  keep.stat = c("n", "ll"),
  #single.row = TRUE,
  align = TRUE,
  title = str_wrap(
    "Raw RTs regressed in a frequentist generalized linear mixed-effects
    model over trial congruency, block- or location-wise congruency, and
    previous trial RT"),
  digits = 1,
  header = FALSE,
  dep.var.caption = "Regression Coefficients of Raw Response Times",
  dep.var.labels = "By condition",
  column.labels = c("LWPC", "CSPC"),
  model.numbers = FALSE,
  single.row = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  covariate.labels = c(
    "Trial congruency",
    "Congruency bias (block or location)",
    "Trial congruency X Congruency bias",
    "Intercept"
  )
)

supp_table_1 <- results_with_preferred_params %>%                               # model comparison is really just a synopsis of the different variables and families in each model,
  pull(`2_batches_Locations_model_comparison`) %>%                              # should be same whether I pulled `1_batches_Blocks_model_comparison`,
  pluck(1L)                                                                     # `1_batches_Locations_model_comparison`, or `2_batches_Locations_model_comparison`

supp_figure_blocks <- results_with_preferred_params %>%
  pull(`1_batches_Blocks_hist_and_trace_plots`) %>%
  pluck(1L)

supp_figure_locations <- results_with_preferred_params %>%
  pull(`1_batches_Locations_hist_and_trace_plots`) %>%
  pluck(1L)


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

