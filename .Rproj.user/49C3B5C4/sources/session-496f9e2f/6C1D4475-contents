# Pivot data to have one column per subject for the pupil time-course
isc_matrix <- analysis_ts |>
  select(Time_sec, SubjectID, Pupil) |>
  pivot_wider(names_from = SubjectID, values_from = Pupil) |>
  select(-Time_sec) |>
  cor(use = "complete.obs")

# ISC matrix
library(ggcorrplot)
ggcorrplot(isc_matrix, lab = TRUE, title = "Intersubject Correlation (Pupil)")