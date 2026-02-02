library(tidyverse)
library(zoo)
library(here)
library(gazer)

process_subject_data <- function(subj_id) {
  gaze_file <- here("data", paste0("Topics_Pupil_NoVWP_20Jan26-", subj_id, "-1-GazeData.txt"))
  
  gaze_data <- read_tsv(
    gaze_file, 
    col_types = cols(
      PupilDiameterLeftEye = col_double(),
      PupilDiameterRightEye = col_double(),
      PupilValidityLeftEye = col_integer(),  # Force to integer
      PupilValidityRightEye = col_integer(), # Force to integer
      RTTime = col_double(),
      .default = col_guess() 
    )
  )
  
  processed <- gaze_data |>
    mutate(
      L = ifelse(PupilValidityLeftEye == 1, PupilDiameterLeftEye, NA),
      R = ifelse(PupilValidityRightEye == 1, PupilDiameterRightEye, NA),
      PupilAvg = (L + R) / 2,
      PupilAvg = coalesce(PupilAvg, L, R)
    )
  
  # DE-SPIKING
  processed <- processed |>
    mutate(PupilAvg = zoo::rollmedian(PupilAvg, k = 5, fill = NA))
  
  # EXTEND BLINKS
  processed <- processed |>
    mutate(is_blink = ifelse(is.na(PupilAvg), 1L, 0L)) |> 
    as.data.frame() |> 
    extend_blinks(fillback = 100, fillforward = 100, hz = 60) |> 
    as_tibble()
  
  # smoothing
  processed <- processed |>
    mutate(
      PupilAvg = na.approx(PupilAvg, maxgap = 15, na.rm = FALSE),
      PupilAvg = rollmean(PupilAvg, k = 5, fill = NA)
    )
  
  # ready slide as baseline
  baseline_val <- processed |>
    filter(CurrentObject == "Ready") |>
    summarise(mean_b = mean(PupilAvg, na.rm = TRUE)) |>
    pull(mean_b)
  
  # Fallback if 'Ready' is missing
  if(is.na(baseline_val) || length(baseline_val) == 0) {
    baseline_val <- processed |>
      filter(CurrentObject == "SegmentationPlayCollectSlide") |>
      filter(RTTime < (min(RTTime) + 2000)) |>
      summarise(mean_b = mean(PupilAvg, na.rm = TRUE)) |>
      pull(mean_b)
  }
  
  processed <- processed |>
    mutate(
      PupilChange = PupilAvg - baseline_val, 
      SubjectID = subj_id
    )
  
  return(processed)
}


subject_list <- c("998", "997", "996", "995") 

all_pupil_data <- map_df(subject_list, process_subject_data)

write_csv(all_pupil_data, here("data", "combined_cleaned_pupil_data.csv"))

all_pupil_data <- read_csv("data/combined_cleaned_pupil_data.csv")
