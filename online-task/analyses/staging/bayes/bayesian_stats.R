# add some libraries up here
# will also need to add the relevant packages

bayesian_stats <- function(){

  bayes_dir <- here(staging_dir, "bayes")
  
  compare_bayes_mdls_name <- here(
    bayes_dir,
    "compare_models.csv"
  )
  power_analyses_csv_name <- here(
    bayes_dir,
    "power_by_sample_size.csv"
  )
  
  pick_brm_model <- function(x){
    read_csv(compare_bayes_mdls_name) %>%
      filter(omit_prev_RT > x) %>%
      filter(LOOIC - min(LOOIC) < 2 * LOOIC_SE) %>%
      arrange(desc(omit_prev_RT), LOOIC) %>%
      slice_head() %>%
      pull(filename) %>%
      here(bayes_dir, "fits", .) %>%
      readRDS
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
          walltime = "48:00:00"),
        template = here(bayes_dir, "template_slurm.tmpl")
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
    
    minimum_priors <- c(
      set_prior('normal(1.75, .25)', coef = "Intercept"),
      set_prior('normal(.275, .1)', coef='CongruencyIncongruent'),
      set_prior('normal(.012, .1)', coef='BiasIncongruent'),
      set_prior('normal(-.02, .07)', coef='CongruencyIncongruent:BiasIncongruent')
    )
    
    if (rerun_find_best_brm){
      
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
          str_wrap("             # why are these models so much bigger than the other two?
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
                bayes_dir,
                "fits",
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
        write_csv(compare_bayes_mdls_name)
      
    }
    
    
    brm_model_no_prev_RT <- pick_brm_model(0)
    
    
    if (regenerate_bayes_power_analysis) {
      
      seq(50, 150, 10) %>%
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
            mutate(
              beyond_95 = ! between(ROPE_Percentage, .05, .95),
              beyond_97dot5 = ! between(ROPE_Percentage, .025, .975),
              beyond_99 = ! between(ROPE_Percentage, .01, .99)
            ) %>%
            summarise(across(where(is.numeric), mean)) %>%
            mutate(sample_size)
        }) %>%
        write_csv(power_analyses_csv_name)
    }
  }
}


