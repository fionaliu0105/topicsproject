library(tidyverse)
library(zoo)

## Gaussian Humps ----
# This creates the clusters of segmentation clicks your advisor requested
ggplot(all_segments, aes(x = Timestamp)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  geom_rug(aes(color = SubjectID)) + 
  theme_minimal() +
  labs(title = "K. 332: Inter-subject Segmentation Agreement",
       subtitle = "Clusters represent strong consensus on musical boundaries",
       x = "Time in Song (ms)", y = "Density (Agreement Strength)")

## do participants show peak after the click? ----

## eda ----
eda_baseline <- all_participants_data |>
  group_by(SubjectID, CurrentObject) |>
  filter(CurrentObject %in% c("SliderInstructions", "SegmentationPlayCollectSlide")) |>
  summarise(mean_raw = mean(PupilAvg, na.rm = TRUE)) |>
  pivot_wider(names_from = CurrentObject, values_from = mean_raw)

eda_baseline

ggplot(all_participants_data, aes(x = PupilChange, fill = SubjectID)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~SubjectID) +
  theme_minimal() +
  labs(title = "Distribution of Pupil Changes by Participant",
       x = "Change from Baseline (mm)", y = "Density")

all_participants_data |>
  filter(CurrentObject %in% c("SliderInstructions", "SegmentationPlayCollectSlide")) |>
  group_by(SubjectID, CurrentObject) |>
  summarise(Raw_Avg = mean(PupilAvg, na.rm = TRUE)) |>
  ggplot(aes(x = CurrentObject, y = Raw_Avg, group = SubjectID, color = SubjectID)) +
  geom_point() +
  geom_line() +
  labs(title = "Baseline vs. Task: Raw Pupil Size",
       y = "Raw Diameter (mm)", subtitle = "A steep drop here confirms a Luminance Artifact")

all_participants_data |>
  filter(CurrentObject == "SegmentationPlayCollectSlide") |>
  mutate(IsMissing = is.na(PupilAvg)) |>
  ggplot(aes(x = RTTime, y = SubjectID, fill = IsMissing)) +
  geom_tile() +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red")) +
  theme_minimal() +
  labs(title = "Missing Data Map (Red indicates blinks/lost signal)",
       x = "Time (ms)", y = "Subject")

ggplot(all_participants_data |> filter(CurrentObject == "SegmentationPlayCollectSlide"), 
       aes(x = CursorX, fill = SubjectID)) +
  geom_histogram(bins = 30) +
  facet_wrap(~SubjectID) +
  labs(title = "How Participants Used the Tension Slider")
