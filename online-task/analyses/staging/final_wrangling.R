final_wrangling <- function(){
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
}