frequentist_stats <- function(){
  
  source(
    here(staging_dir, "frequentist", "graph_builders.R"),
    local = TRUE
  )
  
  regress_both_tasks <- function(df, keys, dv, analysis_type){
    
    condition <- str_extract(first(keys), "(?<=_).*")
    
    cautious_rand_effects <- case_when(
      condition == "Blocks" ~ "+ (1 | Block)",
      condition == "Locations" ~ "+ (1 | unique_block)"
    ) %>%
      rep(cautious_regr)
    
    if (analysis_type == "difference") {
      options(contrasts = c("contr.treatment","contr.poly"))
      equation <- if_else("Bias" %in% colnames(keys), "~ 1", "~ Bias") %>%      # should check this again to make sure (1 | Dominant_Response) shouldn't makes its way in here
        paste(dv, ., cautious_rand_effects)
      
    } else if (analysis_type == "interaction") {
      options(contrasts = c("contr.sum","contr.poly"))                          # apparently interactions do better with contr.sum
      equation <- "~ Congruency * Bias * prev_RT" %>%
        paste(dv, ., "+ (1 | Dominant_Response)", cautious_rand_effects)
    }
    
    if (!(condition == "Locations" & cautious_regr & grepl("RT", dv))){         # so that unique_block and Sub_Code aren't redundant random effects if these three parts of the if statement are all true (would only be redundant in such a case); check if i grepl should or shouldn't positively detect RT_diff
      equation <- paste(equation, "+ (1 | Sub_Code)")
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
}