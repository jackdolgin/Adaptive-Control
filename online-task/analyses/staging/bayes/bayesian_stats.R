bayesian_stats <- function(max_batch, condition, rand_block_component,
                           prior1, prior2, prior3){
  
  key_interaction_param <- "Congruency1.Bias1$"
  
  key_interaction_ROPE_range <- c(0, Inf)
  
  df_with_batch_and_cond <- filter(
    finished_df,
    Task == glue("Predictive_{condition}"),
    Batch <= max_batch
  )
  
  batch_and_cond <- glue("{max_batch}_batches_{condition}")
  
  task_and_batch_dir <- here(bayes_dir, "fits", batch_and_cond)
  
  compare_bayes_mdls_name <- here(task_and_batch_dir, "compare_models.csv")
  
  posterior_descr_name <- here(task_and_batch_dir, "posterior_description.csv")
  
  power_analyses_csv_name <- here(task_and_batch_dir,
                                  "power_by_sample_size.csv")
  
  pick_brm_model <- function(x, format_to_return){
    
    best_model_row <- read_csv(compare_bayes_mdls_name) %>%
      filter(omit_prev_RT > x) %>%
      filter(LOOIC - min(LOOIC) < looic_se_mult * LOOIC_SE) %>%
      arrange(desc(omit_prev_RT), LOOIC) %>%
      slice_head()
    
    if (format_to_return == "row"){
      best_model_row
    } else{
      best_model_row %>%
        pull(filename) %>%
        here(task_and_batch_dir, .) %>%
        readRDS
    }
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
          ncpus = 4L,
          nodes = 1L,
          memory = '5g',
          walltime = "48:00:00"),
        template = here(bayes_dir, "template_slurm.tmpl")
      )
    }
    
    
    brm_with_custom_priors <- function(a_formula, df, familia, priors,
                                       sample_prior, to_be_seed, filename){
      fit_obj <- brm(
        a_formula,
        df,
        familia,
        priors,
        sample_prior = sample_prior,
        save_pars = save_pars(all = T),
        chains = total_chains,
        iter = startup_iters + main_iters,
        warmup = startup_iters,
        cores = 4L,
        backend = if_else(platform == "local", "rstan", "cmdstanr"),
        silent = 2L,
        seed = to_be_seed,
        file_refit = "on_change",
        refresh = 0L
      )
      
      if (is_null(filename)){
        fit_obj
      } else {
        write_rds(fit_obj, here(task_and_batch_dir, filename), "xz")
      }
    }
    
    custom_describe_posterior <- function(amodel, parameters_to_keep, r_range){
      amodel %>%
        describe_posterior(
          test = c("p_direction", "rope", "bf"),
          ci = 1,                                                               # as suggested by 10.3389/fpsyg.2019.02767
          rope_ci = 1,                                                          # ditto
          rope_range=r_range,
          parameters = parameters_to_keep
        ) %>%
        as_tibble %>%
        mutate(across(
          ends_with(c("Median", "low",  "high")),
          family(amodel)$linkinv
        ))
    }
    
    
    minimum_priors <- c(
      set_prior('normal(1, .75)', coef = "Intercept"),                          # weak prior because I don't know how timing of online recordings will be
      set_prior(prior1, coef = "Congruency1"),
      set_prior(prior2, coef = "Bias1"),
      set_prior(prior3, coef = "Congruency1:Bias1")
    )
    
    minimium_priors_shifted_logn <- c(
      set_prior('normal(0, log(1.75))', coef = "Intercept"),
      set_prior('normal(0, log(1.75))', coef = "Congruency1"),
      set_prior('normal(0, log(1.75))', coef = "Bias1"),
      set_prior('normal(0, log(1.75))', coef = "Congruency1:Bias1")
    )
    
    if (rerun_find_best_brm){
      
      prev_RT_priors <- c(
        set_prior('normal(.023, .03)', coef='prev_RT'),                         # based off Spinelli et al. 2019 exp 1b
        set_prior('normal(0, .02)', coef='Congruency1:prev_RT'),                # based off Spinelli et al. 2019 exp 1b
        set_prior('normal(0, .02)', coef='Bias1:prev_RT'),                      # based off Spinelli et al. 2019 exp 1b
        set_prior('normal(0, .02)', coef='Congruency1:Bias1:prev_RT')           # based off Spinelli et al. 2019 exp 1b
      )
      
      prev_RT_priors_shifted_logn <- c(
        set_prior('normal(0, log(1.5))', coef='prev_RT'),
        set_prior('normal(0, log(1.5))', coef='Congruency1:prev_RT'),
        set_prior('normal(0, log(1.5))', coef='Bias1:prev_RT'),
        set_prior('normal(0, log(1.5))', coef='Congruency1:Bias1:prev_RT')
      )
      
      tibble(
        formulae = c(
          paste0(
            "RT ~ 0 + Intercept + Congruency * Bias + (1 | Sub_Code)",
            "+ (1 | Dominant_Response) + (1 | ", rand_block_component, ")"
          ),
          paste0(                                                    # why are these models so much bigger than the other two? (at least when running for batch 1, locations task, haven't run this yet for other two iterations of the pwalk containing this `bayesian_stats` function)
            "RT ~ 0 + Intercept + Congruency * Bias * prev_RT",
            "+ (Congruency | Sub_Code) + (1 | Dominant_Response)",
            "+ (1 | ", rand_block_component, ")"
          ),
          paste0(
            "RT ~ 0 + Intercept + Congruency * Bias * prev_RT",
            "+ (Congruency + Bias | Sub_Code)",
            "+ (Congruency + Bias | Dominant_Response) ",
            "+ (1 | ", rand_block_component, ")"
          )
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
            formulae,
            omit_prev_RT,
            keeper
          ),
          fam = potential_dv_distributions[[2]]
        ) %>%
        filter(keeper) %>%
        rowwise() %>% 
        mutate(prior_col = case_when(
          pluck(fam, "family") == "shifted_lognormal" &
            str_detect(formulae, "prev_RT") ~
            list(c(minimium_priors_shifted_logn, prev_RT_priors_shifted_logn)),
          pluck(fam, "family") == "shifted_lognormal" ~
            list(minimium_priors_shifted_logn),
          str_detect(formulae, "prev_RT") ~
            list(c(minimum_priors, prev_RT_priors)),
          TRUE ~ list(minimum_priors))) %>%
        ungroup() %>%
        mutate(
          filename = glue("brm_fit_{row_number()}.rds"),
          models = future_pmap(
            .f = brm_with_custom_priors,
            .l = list(
              formulae,
              list(df_with_batch_and_cond),
              fam,
              prior_col,
              "yes",
              row_number(),
              filename
            ),
            .options = furrr_options(seed = TRUE)
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
        mutate(model_num = row_number()) %>%
        write_csv(compare_bayes_mdls_name)

      # pick_brm_model(-1) %>%
      #   custom_describe_posterior(NULL) %>%
      #   write_csv(posterior_descr_name)

        # check_prior()      
      best_brm_model <- pick_brm_model(-1, "raw")
      
      model_param_names <- best_brm_model %>%
        find_parameters("fixed", "conditional") %>%
        pluck("conditional") %>%
        paste0("$")
      
      pmap_dfr(
        list(
          list(best_brm_model),
          model_param_names,
          list(
            c(-Inf, 0),
            c(0, Inf),
            c(0, Inf),
            c(-Inf, 0),
            key_interaction_ROPE_range,
            c(-Inf, 0),
            "default",
            "default"
          )
        ),
        custom_describe_posterior
      ) %>%
        write_csv(posterior_descr_name)
      
      # posterior_description <- custom_describe_posterior(best_brm_model)
      # brms::pp_check(best_brm_model)
            
    }
    
    inside_the_rope <- read_csv(posterior_descr_name) %>%
      filter(str_detect(Parameter, regex(key_interaction_param))) %>%
      pull(ROPE_Percentage) %>%
      between(.05, .95)
    
    # run the power analysis only if we've specified we want to regenerate it and if the bayesian model isn't already significant
    if (regenerate_bayes_power_analysis & inside_the_rope &
        (max_batch == 2 | condition == "Locations")){
      
      best_brm_model_no_prev_RT <- pick_brm_model(0, "raw")
      
      power_priors <-
        if (best_brm_model_no_prev_RT$family$family == "shifted_lognormal"){
          minimium_priors_shifted_logn
        } else{
          minimum_priors
        }
      
      
      300 %>%
        future_map_dfr(function(samp_size){
          
          options(contrasts = c("contr.sum","contr.poly"))
          
          post_draws_simulated <- samp_size %>%
            seq %>%
            map_dfr(~{
              df_with_batch_and_cond %>%
                count(Congruency, Bias) %>%
                mutate(across(
                  n, ~round(. * mean_condition_trials / sum(n)))
                ) %>%
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
              best_brm_model_no_prev_RT,
              n=120,
              allow_new_levels=TRUE
            )
          
          post_draws_simulated %>%
            group_by(.draw) %>%
            group_modify(~{
              brm_with_custom_priors(
                best_brm_model_no_prev_RT %>%
                  pluck(formula) %>%
                  as.character %>%
                  pluck(1) %>%
                  str_replace(".*(?= ~)", ".prediction"),
                .x,
                best_brm_model_no_prev_RT$family,
                power_priors,
                "yes",
                1L,
                NULL
              ) %>%
                custom_describe_posterior(
                  key_interaction_param,
                  key_interaction_ROPE_range
                )
            }) %>%
            ungroup() %>%
            mutate(
              beyond_95 = ! between(
                ROPE_Percentage, rope_power_min_range, rope_power_max_range
              ),
              beyond_97dot5 = ! between(ROPE_Percentage, .025, .975),
              beyond_99 = ! between(ROPE_Percentage, .01, .99),
              power_sample_size = samp_size
            )
        }, .options = furrr_options(seed = TRUE)) %>%
        write_csv(power_analyses_csv_name)
    }
  }
  
  read_csv(posterior_descr_name) %>%
    list() %>%
    row_append(glue("{batch_and_cond}_df"))
  
  walk(c("formulae", "fam", "model_num"), ~{
    pick_brm_model(-1, "row") %>%
      pull(glue("{.x}")) %>%
      row_append(glue("{batch_and_cond}_{.x}"))
  })
  
  if (power_analyses_csv_name %in% dir_ls(task_and_batch_dir)){

    walk(c("formulae", "fam", "model_num"), function(col_name){
      pick_brm_model(0, "row") %>%
        pull(glue("{col_name}")) %>%
        row_append(glue("{batch_and_cond}_{col_name}_simplified"))
    })
    
    read_csv(power_analyses_csv_name) %>%
      group_by(power_sample_size) %>%
      summarise(
        n_simulations = n(),
        across(where(is_logical), mean)
      ) %>%
      mutate(across(everything(), ~walk2(., cur_column(), ~{
        row_append(.x, glue("{batch_and_cond}_{.y}"))
      })))
  }
  

  # 
  # key_interaction_stats <- read_csv(posterior_descr_name) %>%
  #   filter(str_detect(Parameter, regex(key_interaction_param)))
  # 
  # walk(c("ROPE_Percentage", "BF"), ~{
  #   key_interaction_stats %>%
  #     pull(glue("{.x}")) %>%
  #     row_append(glue("{.x}_{batch_and_cond}"))
  # })
  
  # try(
  #     read_csv(power_analyses_csv_name) %>%
  #       group_by(power_sample_size) %>%
  #       summarise(across(where(is_logical), mean)) %>%
  #       mutate(across(everything(), ~walk2(., cur_column(), ~{
  #         row_append(.x, glue("{batch_and_cond}_{.y}"))
  #       }))),
  #   TRUE
  # )
  
}


