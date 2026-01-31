library(tidyverse)
library(zoo)
library(here)
library(gazer)

# 1. Load all segmentation files
load_segments <- function(subj_id) {
  read_csv(here("data", paste0("Segmentation_Subject", subj_id, "1.txt")), skip = 2, 
           col_names = c("ClickID", "Timestamp"), col_types = cols()) |>
    filter(!is.na(suppressWarnings(as.numeric(ClickID)))) |>
    mutate(SubjectID = subj_id)
}
all_segments <- map_df(subject_list, load_segments)
write_csv(all_segments, here("data", "combined_cleaned_segments_data.csv"))


# 2. Plot the 'Gaussian Humps' (Density)
ggplot(all_segments, aes(x = Timestamp)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  geom_vline(aes(xintercept = Timestamp, color = SubjectID), alpha = 0.3) +
  theme_minimal() +
  labs(title = "K. 332 Segmentation Agreement (Clusters)",
       subtitle = "Humps indicate where multiple participants heard a 'new idea'",
       x = "Time in Song (ms)", y = "Density of Clicks")

analyzed_epochs <- all_participants_data |>
  filter(CurrentObject == "SegmentationPlayCollectSlide") |>
  inner_join(all_segments, by = "SubjectID") |>
  mutate(Time_Rel = RTTime - Timestamp) |>
  filter(Time_Rel >= -2000 & Time_Rel <= 2000) |>
  arrange(SubjectID, Timestamp, Time_Rel) |>
  group_by(SubjectID, ClickID) |>
  mutate(
    Local_Base = mean(PupilChange[Time_Rel >= -2000 & Time_Rel <= 0], na.rm = TRUE),
    Pupil_Event = PupilChange - Local_Base
  ) |>
  ungroup()

# Plot a few random epochs to check for order
sliced_epochs |>
  filter(SubjectID == "998", ClickID %in% 1:3) |>
  ggplot(aes(x = Time_Rel, y = Pupil_Epoch, color = factor(ClickID))) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ClickID) +
  theme_minimal() +
  labs(title = "Sanity Check: First 3 Epochs for Subject 998",
       subtitle = "Data should start at -2000 and end at 2000 ms")