methods_info <- function(){
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
  
}