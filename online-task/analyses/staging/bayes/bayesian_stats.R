bayesian_stats <- function(max_batch, condition, rand_block_component,
                           prior_intercept_mean, prior_intercept_sd,
                           prior_congruency_mean, prior_congruency_sd,
                           prior_bias_mean, prior_bias_sd,
                           prior_interaction_mean, prior_interaction_sd){
  
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
    
    model_rows <- read_csv(compare_bayes_mdls_name)
    
    if (x == "stick_to_simplest") model_rows <- slice_min(model_rows,
                                                          nchar(formulae))
    
    best_model_row <- slice_min(model_rows, LOOIC)
    
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
        backend = "cmdstanr",
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
          rope_ci = 1,                                                          # suggested by 10.3389/fpsyg.2019.02767
          rope_range= r_range,
          parameters = parameters_to_keep
        ) %>%
        as_tibble# %>%
        # mutate(across(
        #   ends_with(c("Median", "low",  "high")),
        #   family(amodel)$linkinv
        # ))
    }
    
    set_df_of_priors <- function(fam){
      if (fam$family %in% c("exgaussian", "skew_normal")){
        addend <- 0
        denom <- 1
        link_func <- ""
      } else if (fam$family == "shifted_lognormal"){
        addend <- 1
        denom <- prior_intercept_mean
        link_func <- "log"
      }
      
      pmap_dfr(
        list(
          c(
            prior_intercept_mean,
            c(
              prior_congruency_mean,
              prior_bias_mean,
              prior_interaction_mean
            ) %>%
              divide_by(denom) %>%
              add(addend)
          ),
          c(
            prior_intercept_sd,
            c(
              prior_congruency_sd,
              prior_bias_sd,
              prior_interaction_sd
            ) %>%
              divide_by(denom) %>%
              add(addend)
          ),
          link_func,
          c("Intercept", rep("b", 3)),
          c(
            "",
            "CongruencyIncongruent",
            "BiasIncongruent",
            "CongruencyIncongruent:BiasIncongruent"
          )
        ),
        ~{
          "set_prior('normal({..3}({..1}), {..3}({..2}))', class = '{..4}', coef = '{..5}')" %>%
            glue %>%
            parse_expr %>%
            eval
        }
      )
    }
    
    if (rerun_find_best_brm){
      
      rm(all_models)                                                            # remove this variable since it's a running list of model fits, and we don't want the list to spill over from a different `max_batch` and `condition` pwalk combination run from `staging.R`
      
      c(
        paste0(
          "(1 | Sub_Code) + (1 | Dominant_Response) ",
          "+ (1 | ", rand_block_component, ")"
        ),
        paste0(
          "(Congruency | Sub_Code) + (1 | Dominant_Response) ",
          "+ (1 | ", rand_block_component, ")"
        ),
        paste0(
          "(Congruency + Bias | Sub_Code) ",
          "+ (Congruency + Bias | Sub_Code) ",
          "+ (Congruency + Bias | Dominant_Response) ",
          "+ (1 | ", rand_block_component, ")"
        )
      ) %>%
        paste0("RT ~ 0 + Intercept + Congruency * Bias + ", .) %>%
        as_tibble_col("formulae") %>%
        bind_cols(keeper = c(TRUE, rep(platform != "local", 2))) %>%
        tidyr::expand(
          nesting(formulae, keeper),
          fam = potential_dv_distributions[[2]]
        ) %>%
        filter(keeper) %>%
        mutate(prior_col = map(fam, set_df_of_priors)) %>%
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
            all_models <<- list_modify(x)
          }
        }) %>%
        bind_cols(compare_performance(all_models)) %>%
        mutate(across(fam, . %>% map_chr( ~{pluck(.x, 1)}))) %>%
        select(-c(Name, prior_col, models)) %>%
        mutate(model_num = row_number()) %>%
        write_csv(compare_bayes_mdls_name)

      # pick_brm_model("all_models") %>%
      #   custom_describe_posterior(NULL) %>%
      #   write_csv(posterior_descr_name)

        # check_prior()      
      best_brm_model <- pick_brm_model("all_models", "raw")
      
      model_param_names <- best_brm_model %>%
        find_parameters("fixed", "conditional") %>%
        pluck("conditional") %>%
        paste0("$")
      
      pmap_dfr(
        list(
          list(best_brm_model),
          model_param_names,
          c(-rope_size, rope_size) %>%
            list %>%
            rep(4)
        ),
        custom_describe_posterior
      )
        write_csv(posterior_descr_name)
      
      # posterior_description <- custom_describe_posterior(best_brm_model)
      # brms::pp_check(best_brm_model)
            
    }
    
    inside_the_rope <- read_csv(posterior_descr_name) %>%
      filter(str_detect(Parameter, regex(key_interaction_param))) %>%
      pull(ROPE_Percentage) %>%
      between(.05, .95)
    
    # run the power analysis only if we've specified we want to regenerate it and if the bayesian model isn't already significant
    if (regenerate_bayes_power_analysis & inside_the_rope & max_batch == 2 | condition == "Locations"){
      
      best_simple_brm_model <- pick_brm_model("stick_to_simplest", "raw")
      
      300 %>%
        future_map_dfr(function(samp_size){
          
          options(contrasts = c("contr.treatment","contr.poly"))                # This is already the default, just being explicit about it
          
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
              best_simple_brm_model,
              ndraws = 120,
              allow_new_levels=TRUE
            )
          
          post_draws_simulated %>%
            group_by(.draw) %>%
            group_modify(~{
              brm_with_custom_priors(
                best_simple_brm_model %>%
                  pluck(formula) %>%
                  as.character %>%
                  pluck(1) %>%
                  str_replace(".*(?= ~)", ".prediction"),
                .x,
                best_simple_brm_model$family,
                set_df_of_priors(best_simple_brm_model$family) %>%
                  filter(class != "Intercept"),
                "yes",
                1L,
                NULL
              ) %>%
                custom_describe_posterior(
                  key_interaction_param,
                  c(-rope_size, rope_size)
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
  
  
  read_csv(compare_bayes_mdls_name) %>%
    mutate(`Random Effects` = map_chr(formulae, ~{
      reduce2(
        c(
          "Dominant_Response",
          "Sub_Code",
          "unique_block"
        ),
        c(
          "Unique Drawing",
          "Participant",
          "Unique Block or Block Number"
        ), 
        ~str_replace_all(..1, ..2, ..3),
        .init = str_remove(.x, "RT ~ 0 \\+ Intercept \\+ Congruency \\* Bias \\+ ")
      )
    })) %>%
    mutate(across(model_num, as.character)) %>%
    left_join(
      potential_dv_distributions %>%
        as_tibble(.name_repair = ~ c("Family", "fam")) %>%
        mutate(across(everything(), ~map_chr(.x, ~pluck(.x, 1))))
    ) %>%
    select(model_num, Family, `Random Effects`) %>%
    rename(Model = model_num) %>%
    mutate(across(Family, str_to_title)) %>%
    list() %>%
    row_append(glue("{batch_and_cond}_model_comparison"))
  
  map(c("hist", "trace"), ~{
    exec(
      glue("mcmc_{.x}"),
      pick_brm_model("all_models", "raw"),
      regex_pars = "^(b|sigma|ndt).*",
      facet_args = list(
        ncol = 1,
        strip.position = "left",
        labeller = labeller(
          .default = function(term){
            make_term_more_interpretable(term) %>%
              str_remove("b_") %>%
              str_to_title()
          }
        )
      )
    )
  }) %>%
    reduce(plot_grid) %>%
    list() %>%
    row_append(glue("{batch_and_cond}_hist_and_trace_plots"))
  
  read_csv(posterior_descr_name) %>%
    mutate(                                                                     # this mutation back-transforms the ROPE range,...
      Raw_intercept = exp(max(Median)),                                         # ... assuming we're working with a shifted_lognormal...
      across(                                                                   # ... distribution and hence the units are in log
        c("ROPE_low", "ROPE_high"),
        ~ . * Raw_intercept, .names = "{.col}_raw")
    ) %>%
    list() %>%
    row_append(glue("{batch_and_cond}_df"))
  
  walk(c("formulae", "fam", "model_num"), ~{
    pick_brm_model("all_models", "row") %>%
      pull(glue("{.x}")) %>%
      row_append(glue("{batch_and_cond}_{.x}"))
  })
  
  if (power_analyses_csv_name %in% dir_ls(task_and_batch_dir)){

    walk(c("formulae", "fam", "model_num"), function(col_name){
      pick_brm_model("stick_to_simplest", "row") %>%
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


