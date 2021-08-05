methods_info <- function(){
  
  possible_tot_trials <- semi_trimmed_df %>%
    tidyr::expand(Sub_Code, Trial) %>%
    nrow %T>%
    row_append("possible_tot_trials")
  
  
  # Percent of non- full screen trials
  
  possible_tot_trials %>%
    subtract(nrow(semi_trimmed_df)) %>%
    divide_by(possible_tot_trials) %>%
    percent(.1) %>%
    row_append("perc_non_full_screen")
  
  
  # Percent of manually corrected trials
  
  here(analyses_dir, "transcribe", "corrected_responses.csv") %>%
    read_csv %>%
    nrow %>%
    divide_by(possible_tot_trials) %>%
    percent(.1) %>%
    row_append("perc_manually_corrected")
  
  
  # Response Accuracies
  
  n_incorrect_trials <- semi_trimmed_df %>%
    filter(Accuracy == FALSE) %>%
    nrow
  
  n_incorrect_trials %>%
    divide_by(nrow(semi_trimmed_df)) %>%
    percent(.1) %>%
    row_append("perc_wrong")
  
  semi_trimmed_df %>%
    filter(is.na(transcript)) %>%
    nrow %>%
    divide_by(nrow(semi_trimmed_df)) %>%
    percent(.1) %>%
    row_append("perc_blank")
  
  semi_trimmed_df %>%
    filter(
      is.na(Accuracy),
      !is.na(transcript)
    ) %>%
    nrow %>%
    divide_by(nrow(semi_trimmed_df)) %>%
    percent(.1) %>%
    row_append("perc_nonsensical")
  
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
    filter(!Slow_Enough) %>%
    nrow %>%
    divide_by(nrow(first_word_trials)) %>%
    percent(.01) %>%
    row_append("perc_too_fast")
  
  
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
  
}