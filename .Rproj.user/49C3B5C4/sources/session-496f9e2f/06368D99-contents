# Summarize by second to reduce noise and align tasks
analysis_ts <- all_participants_data |>
  filter(CurrentObject == "SegmentationPlayCollectSlide") |>
  mutate(Time_sec = round((RTTime - min(RTTime)) / 1000)) |>
  group_by(SubjectID, Time_sec) |>
  summarise(
    Pupil = mean(PupilChange, na.rm = TRUE),
    Tension = mean(CursorX, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate correlation (consider a 1-2 second lag for the pupil)
cor_results <- analysis_ts |>
  group_by(SubjectID) |>
  summarise(correlation = cor(Pupil, Tension, use = "complete.obs"))

print(cor_results)
