library(tidyverse)
library(zoo)
library(here)
library(gazer)

process_subject_data <- function(subj_id) {
  gaze_file <- here("data", paste0("Topics_Pupil_NoVWP_20Jan26-", subj_id, "-1-GazeData.txt"))
  
  # 1. Force strict types to fix "parsing issues" and "precision" errors
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
  
  # 2. Average and identify blinks
  processed <- gaze_data |>
    mutate(
      L = ifelse(PupilValidityLeftEye == 1, PupilDiameterLeftEye, NA),
      R = ifelse(PupilValidityRightEye == 1, PupilDiameterRightEye, NA),
      PupilAvg = (L + R) / 2,
      PupilAvg = coalesce(PupilAvg, L, R)
    )
  
  # 3. ROBUST DE-SPIKING
  # Using a median filter as a first pass to remove the 'eyeball roll' spikes
  processed <- processed |>
    mutate(PupilAvg = zoo::rollmedian(PupilAvg, k = 5, fill = NA))
  
  # 4. EXTEND BLINKS (Fixing the precision error)
  # gazeR's extend_blinks often requires the blink column to be an integer (0 or 1)
  processed <- processed |>
    mutate(is_blink = ifelse(is.na(PupilAvg), 1L, 0L)) |> # Note the 'L' for integer
    as.data.frame() |> # gazer sometimes prefers base data.frames over tibbles
    extend_blinks(fillback = 100, fillforward = 100, hz = 60) |> 
    as_tibble()
  
  # 5. INTERPOLATION & SMOOTHING
  processed <- processed |>
    mutate(
      # Linear interpolation for gaps up to 250ms (15 samples at 60Hz)
      PupilAvg = na.approx(PupilAvg, maxgap = 15, na.rm = FALSE),
      # Smooth the resulting signal to remove jitter
      PupilAvg = rollmean(PupilAvg, k = 5, fill = NA)
    )
  
  # 6. BASELINE: "Ready" slide
  # Identifying the resting state right before Mozart starts
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
