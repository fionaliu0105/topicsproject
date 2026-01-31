
music_data <- all_participants_data |> 
  filter(CurrentObject == "SegmentationPlayCollectSlide") |>
  group_by(SubjectID) |>
  mutate(Time_ms = RTTime - min(RTTime)) |>
  ungroup()

grand_avg <- music_data |>
  group_by(Time_ms) |>
  summarise(
    Mean_Pupil = mean(PupilChange, na.rm = TRUE),
    SE_Pupil = sd(PupilChange, na.rm = TRUE) / sqrt(n())
  )

all_participants_data |>
  filter(CurrentObject == "SegmentationPlayCollectSlide") |>
  group_by(RTTime) |> 
  summarise(Grand_Mean = mean(PupilChange, na.rm = TRUE)) |>
  ggplot(aes(x = RTTime, y = Grand_Mean)) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  labs(title = "Group Average Pupil Response to K. 332",
       y = "Mean Dilation (mm)", x = "Time (ms)")

# subject 998
seg_998 <- read_csv(here("data", "Segmentation_Subject9981.txt"), skip = 2, 
                    col_names = c("ID", "Timestamp")) |>
  filter(!is.na(as.numeric(ID)))

ggplot(grand_avg, aes(x = Time_ms, y = Mean_Pupil)) +
  geom_line() +
  geom_vline(data = seg_998, aes(xintercept = Timestamp), 
             color = "blue", linetype = "dashed", alpha = 0.6) +
  labs(title = "Pupil Dilation vs. Segmentation Events",
       caption = "Dashed lines indicate participant-reported 'New Ideas'")
