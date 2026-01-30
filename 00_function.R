library(tidyverse)
library(zoo)
library(here)

process_subject_data <- function(subj_id) {
  gaze_file <- here("data", paste0("Topics_Pupil_NoVWP_20Jan26-", subj_id, "-1-GazeData.txt"))
  seg_file  <- here("data", paste0("Segmentation_Subject", subj_id, "1.txt"))
  
  gaze_data <- read_tsv(gaze_file, col_types = cols())
  
  seg_data <- read_csv(
    seg_file,
    skip = 2,
    col_names = c("ResponseID", "Timestamp"),
    col_types = cols()
  ) |> 
    filter(!is.na(suppressWarnings(as.numeric(ResponseID))))
  
  processed <- gaze_data |>
    mutate(
      PupilDiameterLeftEye = ifelse(PupilValidityLeftEye == 1, PupilDiameterLeftEye, NA),
      PupilDiameterRightEye = ifelse(PupilValidityRightEye == 1, PupilDiameterRightEye, NA),
      
      PupilAvg = (PupilDiameterLeftEye + PupilDiameterRightEye) / 2,
      
      PupilAvg = coalesce(PupilAvg, PupilDiameterLeftEye, PupilDiameterRightEye),
      
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