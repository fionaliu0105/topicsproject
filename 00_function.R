library(tidyverse)
library(zoo)
library(here)

process_subject_data <- function(subj_id) {
  gaze_file <- here("data", paste0("Topics_Pupil_NoVWP_20Jan26-", subj_id, "-1-GazeData.txt"))
  seg_file  <- here("data", paste0("Segmentation_Subject", subj_id, "1.txt"))
  
  gaze_data <- read_tsv(
    gaze_file, 
    col_types = cols(
      PupilDiameterLeftEye = col_double(),
      PupilDiameterRightEye = col_double(),
      PupilValidityLeftEye = col_double(),
      PupilValidityRightEye = col_double(),
      .default = col_guess() # Let R guess the rest
    )
  )
  
  seg_data <- read_csv(
    seg_file,
    skip = 2,
    col_names = c("ResponseID", "Timestamp"),
    col_types = cols()
  ) |> 
    filter(!is.na(suppressWarnings(as.numeric(ResponseID))))
  
  processed <- gaze_data |>
    mutate(
      L = suppressWarnings(as.numeric(PupilDiameterLeftEye)),
      R = suppressWarnings(as.numeric(PupilDiameterRightEye)),
      
      L = ifelse(PupilValidityLeftEye == 1, L, NA),
      R = ifelse(PupilValidityRightEye == 1, R, NA),
      
      PupilAvg = (L + R) / 2,
      PupilAvg = coalesce(PupilAvg, L, R),
      
      PupilAvg = na.approx(PupilAvg, maxgap = 10, na.rm = FALSE)
    )
  
  baseline_val <- processed |>
    filter(CurrentObject == "SliderInstructions") |>
    summarise(mean_b = mean(PupilAvg, na.rm = TRUE)) |>
    pull(mean_b)
  
  processed <- processed |>
    mutate(PupilChange = PupilAvg - baseline_val, SubjectID = subj_id)
  
  return(processed)
}