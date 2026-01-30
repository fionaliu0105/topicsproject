library(tidyverse)
library(zoo)

process_subject_data <- function(subj_id) {
  gaze_file <- paste0("Topics_Pupil_NoVWP_20Jan26-", subj_id, "-1-GazeData.txt")
  seg_file  <- paste0("Segmentation_Subject", subj_id, "1.txt")
  
  gaze_data <- read_tsv(gaze_file, col_types = cols())
  seg_data <- read_csv(
    seg_file,
    skip = 2,
    col_names = c("ResponseID", "Timestamp"),
    col_types = cols()
  ) |> 
    filter(!is.na(as.numeric(ResponseID)))
  
  processed <- gaze_data |>
    mutate(
      PupilDiameterLeftEye = ifelse(PupilValidityLeftEye == 1, PupilDiameterLeftEye, NA),
      PupilDiameterRightEye = ifelse(PupilValidityRightEye == 1, PupilDiameterRightEye, NA),
      PupilAvg = rowMeans(
        select(., PupilDiameterLeftEye, PupilDiameterRightEye),
        na.rm = TRUE
      ),
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
