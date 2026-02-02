all_pupil_data <- read_csv("data/combined_cleaned_pupil_data.csv")

# 1. Align gaze data and fix the SubjectID type
listening_gaze <- all_pupil_data |>
  filter(CurrentObject == "SegmentationPlayCollectSlide") |>
  group_by(SubjectID) |>
  # Crop to the exposition (~120s) as requested
  mutate(Music_Time = RTTime - min(RTTime)) |> 
  filter(Music_Time <= 120000) |> 
  # Convert SubjectID to character to match segments
  mutate(SubjectID = as.character(SubjectID)) |> 
  ungroup()

# 2. Slice the 'Top-Half' Epochs
analyzed_epochs <- listening_gaze |>
  # Join using the now-matching character types
  inner_join(all_segments |> mutate(SubjectID = as.character(SubjectID)), 
             by = "SubjectID") |>
  mutate(Time_Rel = Music_Time - Timestamp) |>
  # 2s before/after click to control for tonic drift
  filter(Time_Rel >= -2000 & Time_Rel <= 2000) |>
  arrange(SubjectID, Timestamp, Time_Rel) |>
  # Local Baseline (2s before click) as the delta for pupil change
  group_by(SubjectID, ClickID) |>
  mutate(
    Local_Base = mean(PupilChange[Time_Rel >= -2000 & Time_Rel <= 0], na.rm = TRUE),
    Pupil_Event = PupilChange - Local_Base
  ) |>
  ungroup()

erpr_summary <- analyzed_epochs |>
  group_by(Time_Rel) |>
  summarise(
    Mean_Pupil = mean(Pupil_Event, na.rm = TRUE),
    SE = sd(Pupil_Event, na.rm = TRUE) / sqrt(n())
  )


ggplot(erpr_summary, aes(x = Time_Rel, y = Mean_Pupil)) +
  geom_ribbon(aes(ymin = Mean_Pupil - SE, ymax = Mean_Pupil + SE), alpha = 0.2, fill = "darkred") +
  geom_line(color = "steelblue", size = 1) +
  geom_smooth(color = "darkred") + 
  theme_minimal() +
  labs(title = "K. 332: Grand Average Pupil Response to New Ideas",
       subtitle = "Averaged across 4 participants; Time 0 = button press",
       x = "Relative Time (ms)", y = "Pupil Change (Local Baseline)")

erpr_summary |> 
  filter(Time_Rel < 0) |> 
  summarize(average = mean(Mean_Pupil))

erpr_summary |> 
  filter(Time_Rel > 0) |> 
  summarize(average = mean(Mean_Pupil))

