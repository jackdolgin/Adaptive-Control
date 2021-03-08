# Import Packages and Set Up Directories ----------------------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(googleLanguageR, googleCloudStorageR, sound, tuneR, fs,
               DescTools, tidyverse, furrr, here, magrittr)
pacman::p_load_gh("LiKao/VoiceExperiment")

no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

# May take a few minutes to run the first time you're authenticating
here("Authentication_File.json") %T>%
  gl_auth %T>%
  gcs_auth

audio_dir <- here("online-task", "analyses", "raw_data", "audio")
transcribe_dir <- here("online-task", "analyses", "transcribe")

pcpts_with_all_audio_saved <- dir_ls(path(audio_dir, "full")) %>%
  str_extract("(?<=full/)[[:alnum:]]*") %>%
  as_tibble_col(column_name = "Sub_Code") %>%
  count(Sub_Code) %>%
  filter(n == 4) %>%
  select(Sub_Code) %>%
  filter(! Sub_Code %in% c(                                                     # Remove these participants from the analyzed data
    "TeIzC5Si",                                                                 # second time participating
    "ZaNEATXg",                                                                 # second time participating
    "nYFDenHD",                                                                 # second time participating
    "9z9Be9lj",                                                                 # second time participating
    "9Ka1PGgd",                                                                 # second time participating
    "sTPZJGOH",                                                                 # second time participating
    "5g1a5F9S",                                                                 # second time participating
    "FPiMSr70",                                                                 # second time participating
    "1ADP4oEP",                                                                 # second time participating
    "rd5Rou3C",                                                                 # third time participating
    "gt3skyFD",                                                                 # error issue reading their voice data
    "ZDRIrjxA",                                                                 # error issue reading their voice data
    "nE0in8wT",                                                                 # error issue reading their voice data
    "sJiC0naK",                                                                 # voice was consistently low, difficult to analyze
    "qiAx7h9D",                                                                 # didn't take the task seriously
    "OWNpYNOV",                                                                 # poor command of English
    "tOGmbtQk",                                                                 # too many blank responses, and they didn't seem focused
    "ugpDTAuK",                                                                 # TV on in the background
    "lgPVM78L"                                                                  # child making noise in background and distracting the speaker
  ))                                                                

recording_durations <- path(audio_dir, "full") %>%
  dir_ls(glob = "*.wav") %>%
  as.character %>%
  as_tibble_col("Full_Audio_Path") %>%
  mutate(
    Recording_Duration = future_map_dbl(Full_Audio_Path, ~sound::duration(.x))  # Takes a couple minutes to run
  )


# Import Data and Run through Google Speech-to-Text -----------------------

predictive_context <- function(mydf, either_block_or_side, relevant_task){
  mydf %>%
    group_by(Sub_Code, !!either_block_or_side) %>%
    mutate(!!paste0(quo_name(either_block_or_side), "_Bias") := 
             case_when(
               Task != relevant_task ~ NA_character_,
               sum(Congruency == "Congruent") / n() > .5 ~ "Congruent",
               TRUE ~ "Incongruent")
    )
}

list(
  here("online-task", "analyses", "raw_data", "raw_from_mturk.csv"),
  here("images", "IPNP", "IPNP_spreadsheet_synonyms.csv"),
  here("online-task", "analyses", "raw_data", "demographics.csv")
) %>%
  map(read_csv) %>%
  reduce(left_join) %>%
  right_join(pcpts_with_all_audio_saved) %>%
  filter(Block > 0) %>%
  mutate(Full_Audio_Path = map2_chr(Sub_Code, Block, function(x, y) {
    path(audio_dir, "full") %>%
      dir_ls(regexp = paste0(x, "_.*_", y), fail = FALSE)
  })) %>%
  left_join(recording_durations) %>%
  group_by(Sub_Code, Block) %>%
  mutate(
    across(Nationality, ~if_else(is.na(.) | . == "O", "en-US", .)),
    Time_Skipped = max(Stim_Offset) %>%
      subtract(min(Trial_Start)) %>%
      divide_by(1000) %>%
      subtract(Recording_Duration),
    across(c(Fix_Onset, Fix_Offset, Stim_Onset, Stim_Offset, Trial_Start),
           . %>%
             subtract(min(Trial_Start)) %>%
             divide_by(1000) %>%
             subtract(Time_Skipped)),
    End_recording = 
      if_else(
        lead(Fix_Offset) > Stim_Onset + 58 | row_number() == n(),
        Stim_Onset + 58,
        lead(Fix_Offset, default = last(Fix_Offset) + 58)),
    across(c(Audio_Connected, Audio_Permitted, Full_Screen),
           list(lead, lag), .names = "{.col}_{.fn}")
  ) %>%
  filter(across(
    starts_with(c("Audio_Connected", "Audio_Permitted", "Full_Screen")),
    ~is.na(.) | . == 1
  )) %>%
  predictive_context(quo(Block), "Predictive_Blocks") %>%
  predictive_context(quo(Task_Side), "Predictive_Locations") %>%
  ungroup() %>%
  mutate(across(Synonyms,
                ~if_else(Dominant_Response == Label,
                         paste(Label, ., sep = ", "),
                         paste(Dominant_Response, Label, ., sep = ", "))),
         across(Synonyms, ~str_remove(., " NA$"))) %>%
  write_csv(here(transcribe_dir, "light_clean_from_mturk.csv"))

light_clean_from_mturk <- here(transcribe_dir, "light_clean_from_mturk.csv") %>%
  read_csv

i_max <- light_clean_from_mturk %>%
  nrow %>%
  RoundTo(100, ceiling) %>%
  divide_by(100)

for (loop_count in 0:(i_max - 1)){
  while_count <- 0
  while (while_count < 5){
    tryCatch({
      light_clean_from_mturk %>%
        filter(row_number() > (loop_count * 100 ),
               row_number() <= (loop_count * 100) + 100) %>%
        pmap_df(function(Stim_Onset, Trial, End_recording,
                         Sub_Code, Synonyms, Nationality,
                         Full_Audio_Path, ...){
          spliced_audio_dir <- path(audio_dir, "spliced", Sub_Code) %T>%
            dir_create %>%
            path(paste0("Trial_", Trial, ".wav"))
          
          readWave(Full_Audio_Path, from = Stim_Onset,
                   to = End_recording, units = "seconds") %T>%
            writeWave(spliced_audio_dir, extensible = FALSE)
          
          transcribed_list <- gl_speech(spliced_audio_dir,
                                        languageCode = Nationality,
                                        speechContexts =
                                          list(phrases = strsplit(Synonyms,
                                                                  ',') %>%
                                                 unlist %>%
                                                 trimws))
          
          Sys.sleep(1)
          
          precise_timing <- spliced_audio_dir %>%
            read.wav(filter = list(high = 4000)) %>%
            onsets
          
          if(length(transcribed_list$timings) > 0){
            df <- transcribed_list %>%
              pluck('transcript') %>%
              select(transcript, confidence) %>%
              mutate(across(transcript, trimws))
          } else {
            df <- tibble(transcript = NA, confidence = NA)
          }
          
          mutate(df,
                 Sub_Code,
                 Trial,
                 preciseStartTime = pluck(precise_timing, first, "start"),
                 preciseEndTime = pluck(precise_timing, last, "end"))
        }) %>%
        write_csv(here(transcribe_dir,
                       "chunked",
                       paste0("transcription_pt", loop_count + 1,  ".csv")))
      
      break
    }, error=function(e) {         # test this line just for kick's sake
      while_count <- while_count + 1
    }
    )
  }
}

path(transcribe_dir, "chunked") %>%
  dir_ls %>%
  map_df(read_csv) %>%
  right_join(light_clean_from_mturk) %>%
  write_csv(here(transcribe_dir, "transcriptions_merged.csv"))

