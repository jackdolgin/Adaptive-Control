# Import Packages and Set Up Directories ----------------------------------

# install.packages("devtools")
if (!require(devtools)) install.packages("pacman")
pacman::p_load(googleLanguageR, googleCloudStorageR, sound, tuneR, fs,
               DescTools, tidyverse, furrr, here, magrittr)
pacman::p_load_gh("LiKao/VoiceExperiment")

no_cores <- availableCores() - 1L
plan(multisession, workers = no_cores)

raw_dir <- here("online-task", "analyses", "raw_data")
audio_dir <- here(raw_dir, "audio")

pcpts_with_all_audio_saved <- path(audio_dir, "full") %>%
  dir_ls(regexp = "_[[:alnum:]].wav") %>%                                       # Ignore audio from practice trial
  str_extract("(?<=full/)[[:alnum:]]*") %>%
  as_tibble_col(column_name = "Sub_Code") %>%
  count(Sub_Code) %>%
  filter(n == 4L) %>%
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
    "wXzdYKqh",                                                                 # second time participating
    "6Ii4dBYv",                                                                 # second time participating
    "ONZH07rh",                                                                 # second time participating
    "A9Dz1vG4",                                                                 # second time participating
    "o69A0VsK",                                                                 # second time participating
    "LOS9PDfF",                                                                 # second time participating
    "1e1VFWjI",                                                                 # second time participating
    "57263529",                                                                 # second time participating
    "ANrKJmFe",                                                                 # second time participating
    "hiYUAiLF",                                                                 # second time participating
    "4YmuWpvJ",                                                                 # second time participating
    "FpgJwbjL",                                                                 # second time participating
    "HaMibOwc",                                                                 # second time participating
    "pGjEGy6F",                                                                 # second time participating
    "rd5Rou3C",                                                                 # third time participating
    "gt3skyFD",                                                                 # error issue reading their voice data
    "ZDRIrjxA",                                                                 # error issue reading their voice data
    "nE0in8wT",                                                                 # error issue reading their voice data
    "70m2f9pC",                                                                 # error issue reading their voice data
    "sJiC0naK",                                                                 # voice was consistently low, difficult to analyze
    "qiAx7h9D",                                                                 # didn't take the task seriously
    "RFhUrH7P",                                                                 # didn't follow the instructions
    "OWNpYNOV",                                                                 # poor command of English
    "tOGmbtQk",                                                                 # too many blank responses, and they didn't seem focused
    "ugpDTAuK",                                                                 # TV on in the background
    "5pu4UnTu",                                                                 # TV on in the background
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
  here(raw_dir, "raw_from_mturk.csv"),
  here("images", "IPNP", "IPNP_spreadsheet_synonyms.csv"),
  here(raw_dir, "demographics.csv")
) %>%
  map(read_csv) %>%
  reduce(left_join) %>%
  right_join(pcpts_with_all_audio_saved) %>%
  filter(Block > 0L) %>%
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
      divide_by(1000L) %>%
      subtract(Recording_Duration),
    across(c(Fix_Onset, Fix_Offset, Stim_Onset, Stim_Offset, Trial_Start),
           . %>%
             subtract(min(Trial_Start)) %>%
             divide_by(1000L) %>%
             subtract(Time_Skipped)),
    End_recording = 
      if_else(
        lead(Fix_Offset) > Stim_Onset + 58L | row_number() == n(),
        Stim_Onset + 58L,
        lead(Fix_Offset, default = last(Fix_Offset) + 58L)),
    across(c(Audio_Connected, Audio_Permitted, Full_Screen),
           list(lead, lag), .names = "{.col}_{.fn}")
  ) %>%
  filter(across(
    starts_with(c("Audio_Connected", "Audio_Permitted", "Full_Screen")),
    ~is.na(.) | . == 1L
  )) %>%
  predictive_context(quo(Block), "Predictive_Blocks") %>%
  predictive_context(quo(Task_Side), "Predictive_Locations") %>%
  ungroup() %>%
  mutate(across(Synonyms,
                ~if_else(Dominant_Response == Label,
                         paste(Label, ., sep = ", "),
                         paste(Dominant_Response, Label, ., sep = ", "))),
         across(Synonyms, ~str_remove(., " NA$"))) %>%
  mutate(pmap_dfr(., insistently(function(Stim_Onset, Trial, End_recording,
                                          Sub_Code, Synonyms, Nationality,
                                          Full_Audio_Path, ...){
    
    spliced_audio_dir <- path(audio_dir, "spliced", Sub_Code) %T>%
      dir_create %>%
      path(paste0("Trial_", Trial, ".wav"))
    
    readWave(Full_Audio_Path, from = Stim_Onset,
             to = End_recording, units = "seconds") %T>%
      writeWave(spliced_audio_dir, extensible = FALSE)
    
    here("Authentication_File.json") %T>%                                       # May take a few minutes to run the first time you're authenticating
      gl_auth %>%
      gcs_auth
    
    transcribed_list <- gl_speech(
      spliced_audio_dir,
      languageCode = Nationality,
      speechContexts =
        list(phrases = strsplit(Synonyms, ',') %>%
               unlist %>%
               trimws
        )
    )
    
    Sys.sleep(1)
    
    precise_timing <- spliced_audio_dir %>%
      read.wav(filter = list(high = 4000L)) %>%
      onsets
    
    preciseStartTime <- pluck(precise_timing, first, "start")
    preciseEndTime <- pluck(precise_timing, last, "end")
    
    if(length(transcribed_list$timings) > 0L){
      df <- transcribed_list %>%
        pluck('transcript') %>%
        mutate(
          across(transcript, trimws),
          across(confidence, as.numeric)                                        # though, some nations' English lack a confidence rating in Google's API
        ) %>%
        summarise(
          across(transcript, ~paste0(., collapse = "")),
          across(confidence, mean)
        )
    } else {
      df <- tibble(transcript = NA_character_, confidence = NA_real_)
    }
    
    mutate(
      df,
      preciseStartTime = if_else(
        is.null(preciseStartTime), NA_real_, preciseStartTime),
      preciseEndTime = if_else(
        is.null(preciseEndTime), NA_real_, preciseEndTime)
    )
    
  }, rate = rate_delay(5)))) %>%
  write_csv(here(
    "online-task",
    "analyses",
    "transcribe",
    "lightly_cleaned_and_transcribed.csv"
  ))
