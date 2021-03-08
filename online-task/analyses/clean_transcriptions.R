# Import Packages and Set Up Directories ----------------------------------

# install.packages("devtools")

if (!require(devtools)) install.packages("pacman")
pacman::p_load(DescTools, tidyverse, furrr, here)

no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

analyses_dir <- here("online-task", "analyses")
transcript_dir <- here(analyses_dir, "transcribe")


# Clean speech-to-text data -----------------------------------------------

here(transcript_dir, "transcriptions_merged.csv") %>%
  read_csv %>%
  group_by(across(-c(preciseStartTime, preciseEndTime, transcript,              # So that each trial only takes up 1 row
                     confidence))) %>%                                          # Also some nations' English lack a confidence rating in Google's API
  summarise(across(transcript, ~paste0(., collapse = "")),
            across(preciseStartTime, min),
            across(preciseEndTime, max),
            across(confidence, mean)) %>%
  ungroup() %>%
  left_join(read_csv(here(transcript_dir, "corrected_responses.csv")),
            by=c("Sub_Code", "Trial")) %>%
  mutate(
    transcript = coalesce(transcript.y, transcript.x),
    across(confidence, ~if_else(is.na(transcript.y), ., NA_real_)),
    across(c(Dominant_Response, Label, Synonyms, transcript),
           ~str_remove_all(., " ") %>% tolower),
    Timely_Response = if_else(Stim_Onset + preciseStartTime <= Stim_Offset,
                              TRUE, FALSE)
  ) %>%
  mutate(Accuracy = future_pmap_lgl(., function(
    Synonyms, transcript, Label, Congruency, ...){
    
    Synonyms <- StrSplit(Synonyms, ",")
    Synonyms_and_plurals <- c(Synonyms,
                              paste0("a", Synonyms),
                              paste0(Synonyms, "s"),
                              paste0(Synonyms, "es"))
    
    transcript_beginning <- str_sub(transcript, end = str_count(Label))
    
    case_when(
      transcript_beginning == Label & Congruency == "Incongruent" ~ FALSE,
      transcript %in% Synonyms_and_plurals ~ TRUE,
      TRUE ~ NA)})
  ) %>%
  arrange(Sub_Code, Trial) %>%
  select(-c(transcript.x, transcript.y)) %>%
  group_by(Sub_Code, transcript) %>%
  mutate(First_Word_Use = (row_number() == 1)) %>%                              # If someone says the same word for multiple trials, only keep the first trial they said that word and filter out the other times
  write_csv(here(analyses_dir, "cleaned.csv"))
