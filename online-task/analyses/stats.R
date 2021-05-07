# Import Packages and Load Helper Functions and Data ----------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(gtools, DescTools, tidyverse, furrr, here, magrittr, lme4,
               lmerTest, broomExtra, cowplot, stargazer, png, scales, brms,
               bayestestR, performance, rlang, insight, logspline, future,
               future.batchtools, tidybayes, glue)

analyses_dir <- here("online-task", "analyses")

cleaned_df <- read_csv(here(analyses_dir, "cleaned.csv"))


output_list_per_analysis_combo <- function(exact_resp_req, keep_first_block,
                                    keep_last_three_blocks, cautious_regr,
                                    preferred_params, rerun_find_best_brm,
                                    regenerate_bayes_power_analysis, platform){
  

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
    mutate(across(prev_RT, scale)) %>%
    ungroup() %>%
   mutate(
     unique_block = paste(Sub_Code, Block, sep = "_"),
     across(c(Congruency, Bias, Sub_Code, Dominant_Response, unique_block),
            as_factor),
     across(c(Congruency, Bias), ~fct_relevel(., sort))
   )
  
#  row_append(list(finished_df), "finished_df")

  possible_conditions <- finished_df %>%
    filter(Task == "Predictive_Locations") %>%
    count(Congruency, Bias) %>%
    nrow
  
  mean_condition_trials <- finished_df %>%
    filter(Task == "Predictive_Locations") %>%
    count(Sub_Code) %>%
    summarise(mean(n)) %>%
    pull %>%
    RoundTo(possible_conditions) %>%
    divide_by(possible_conditions)
     
  blocks_per_pcpt <- finished_df %>%
    pull(Block) %>%
    n_distinct()
  
  
  # Frequentist Stats and Graph -------------------------------------------
    
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
      
      fitted <- glmer(
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

  
  # Bayesian Stats and Graph ----------------------------------------------
  
  if (!preferred_params) {
    NULL
  } else {
    
    compare_bayes_mdls_name <- here(
      analyses_dir,
      "compare_bayesian_models.csv"
    )
    power_analyses_csv_name <- here(
      analyses_dir,
      "bayesian_power_by_sample_size.csv"           # might want to eventually write it to a different directory
    )
    
    pick_brm_model <- function(x){
      read_csv(compare_bayes_mdls_name) %>%
        filter(omit_prev_RT > x) %>%
        filter(LOOIC - min(LOOIC) < 2 * LOOIC_SE) %>%
        arrange(desc(omit_prev_RT), LOOIC) %>%
        slice_head() %>%
        pull(filename) %>%
        readRDS(here(analyses_dir, "bayesian_fits", .))
    }
    
    
    if (any(c(rerun_find_best_brm, regenerate_bayes_power_analysis))){
      if (platform == "local"){
        
        no_cores <- availableCores() - 1L
        plan(multisession, workers = no_cores)
        
      } else if (platform == "cluster"){
        
        plan(
          batchtools_slurm,
          workers = 4L,
          resources=list(
            job_name = "run_on_cluster",
            log_file = "run_on_cluster.log",
            queue = "medium",
            service = "short",
            ncpus = 4L,
            memory = '5g',
            walltime = "10:00:00"),
          template = here(analyses_dir, "template_slurm.tmpl")
        )
      }
      
      brm_with_custom_priors <- function(a_formula, df, familia, priors,
                                         sample_prior, to_be_seed, filename){
        brm(
          a_formula,
          df,
          familia,
          priors,
          sample_prior = sample_prior,
          save_pars = save_pars(all = T),
          iter = 5000L,
          cores = 4L,
          backend = if_else(platform == "local", "rstan", "cmdstanr"),
          silent = 2L,
          seed = to_be_seed,
          file = filename,
          file_refit = "on_change",
          refresh = 0L
        )
      }
      
      custom_describe_posterior <- function(amodel){
        amodel %>%
          describe_posterior(
            test = c("p_direction", "rope", "bf"),
            ci = 1,                                                               # as suggested by 10.3389/fpsyg.2019.02767
            rope_ci = 1,                                                          # ditto
            rope_range=c(-.005, Inf),
            parameters = "Congruency.*BiasIncongruent$"
          ) %>%
          as_tibble %>%
          mutate(across(
            ends_with(c("Median", "low",  "high")),
            family(amodel)$linkinv
          ))
      }
      
      if (rerun_find_best_brm){
        
        minimum_priors <- c(
          set_prior('normal(1.75, .25)', coef = "Intercept"),
          set_prior('normal(.275, .1)', coef='CongruencyIncongruent'),
          set_prior('normal(.012, .1)', coef='BiasIncongruent'),
          set_prior('normal(-.02, .07)', coef='CongruencyIncongruent:BiasIncongruent')
        )
        
        prev_RT_priors <- c(
          set_prior('normal(.04, .04)', coef='prev_RT'),
          set_prior('normal(.003, .02)', coef='CongruencyIncongruent:prev_RT'),
          set_prior('normal(0, .02)', coef='BiasIncongruent:prev_RT'),
          set_prior('normal(0, .02)', coef='CongruencyIncongruent:BiasIncongruent:prev_RT')
        )
        
        brm_models <- tibble(
          prior_col = list(
            minimum_priors,
            c(minimum_priors, prev_RT_priors),
            c(minimum_priors, prev_RT_priors)
          ),
          formulae = c(
            str_wrap("
          RT ~ 0 + Intercept + Congruency * Bias + (1 | Sub_Code)
          + (1 | Dominant_Response) + (1 | unique_block)
        ", 1e5),                                                                # set line long enough that new line symbol /n doesn't appear in the formula
            str_wrap("
          RT ~ 0 + Intercept + Congruency * Bias * prev_RT
          + (Congruency | Sub_Code) + (1 | Dominant_Response)
          + (1 | unique_block)
        ", 1e5),
            str_wrap("
          RT ~ 0 + Intercept + Congruency * Bias * prev_RT
          + (Congruency + Bias | Sub_Code)
          + (Congruency + Bias | Dominant_Response) + (1 | unique_block)
        ", 1e5)
          ),
          keeper = c(
            TRUE,
            platform != "local" %>%
              rep(2)
          )
        ) %>%
          mutate(omit_prev_RT = !str_detect(formulae, "prev_RT")) %>%
          tidyr::expand(
            nesting(
              prior_col,
              formulae,
              omit_prev_RT,
              keeper
            ),
            fam = list(
              exgaussian(),
              skew_normal(),
              shifted_lognormal()
            )
          ) %>%
          filter(keeper) %>%
          mutate(
            filename = glue("brm_fit_{row_number()}.rds"),
            models = future_pmap(
              .f = brm_with_custom_priors,
              .l = list(
                formulae,
                list(filter(finished_df, Task == "Predictive_Locations")),
                fam,
                prior_col,
                "yes",
                row_number(),
                here(
                  analyses_dir,
                  "bayesian_fits",
                  filename
                )
              )
            )) %>%
          walk(function(x){
            if (class(pluck(x, 1)) == "brmsfit"){
              all_models <<- list_modify(x, metrics = "LOOIC")
            }
          }) %>%
          mutate(
            exec("compare_performance", !!!all_models),
            across(fam, . %>% map_chr( ~{pluck(.x, 1)}))
          ) %>%
          select(-c(Name, prior_col, models)) %>%
          write_csv(compare_bayes_mdls_name)  # can have an if statement above about whether to let ppl calculate the stats on their own or just start off with the csv
        
      }
      
      
      brm_model_no_prev_RT <- pick_brm_model(0)
      
      
      if (regenerate_bayes_power_analysis) {
        
        seq(50, 100, 10) %>%
          future_map_dfr(function(sample_size){
            
            post_draws_simulated <- sample_size %>%
              seq %>%
              map_dfr(~{
                finished_df %>%
                  filter(Task == "Predictive_Locations") %>%
                  count(Congruency, Bias) %>%
                  mutate(across(n, ~round(. * mean_condition_trials / sum(n)))) %>%
                  uncount(n) %>%
                  sample_n(n()) %>%
                  mutate(n = blocks_per_pcpt) %>%
                  uncount(n) %>%
                  mutate(
                    Sub_Code = .x,
                    Dominant_Response = row_number(),
                    unique_block = row_number() %>%
                      multiply_by(blocks_per_pcpt) %>%
                      divide_by(n()) %>%
                      ceiling %>%
                      paste0(Sub_Code, "_", .)
                  )
              }) %>%
              mutate(across(everything(), as_factor)) %>%
              add_predicted_draws(
                brm_model_no_prev_RT,
                n=100,
                allow_new_levels=TRUE
              )
            
            post_draws_simulated %>%
              group_by(.draw) %>%
              group_modify(~{
                brm_with_custom_priors(
                  brm_model_no_prev_RT %>%
                    pluck(formula) %>%
                    as.character %>%
                    pluck(1) %>%
                    str_replace(".*(?= ~)", ".prediction"),
                  .x,
                  brm_model_no_prev_RT$family,
                  minimum_priors,
                  "yes",
                  1L,
                  NULL
                ) %>%
                  custom_describe_posterior()
              }) %>%
              ungroup() %>%
              summarise(
                across(ROPE_Percentage, ~ between(., .01, .99) %>%
                         not %>%
                         sum %>%
                         divide_by(n())),
                across(where(is.numeric), mean)
              ) %>%
              mutate(sample_size)
          }) %>%
          write_csv(power_analyses_csv_name)
      }
    }
    
    best_brm_model <- pick_brm_model(-1)
    
    posterior_description <- custom_describe_posterior(best_brm_model)
    
    brms::pp_check(best_brm_model)
    
  }    
  
  
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
    format_number %>%
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

