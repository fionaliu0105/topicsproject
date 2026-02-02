
# Define 2-second bins to find where clicks overlap
boundary_bins <- all_segments |>
  mutate(Bin = round(Timestamp / 2000) * 2000) |> 
  group_by(Bin) |>
  summarise(Agreement_Count = n_distinct(SubjectID)) |>
  mutate(Agreement_Level = ifelse(Agreement_Count >= 3, "High", "Low"))

final_analysis <- analyzed_epochs |>
  mutate(Bin = round(Timestamp / 2000) * 2000) |>
  left_join(boundary_bins, by = "Bin") |>
  filter(Time_Rel > 0) |> 
  group_by(SubjectID, ClickID, Agreement_Level) |>
  summarise(Peak_Dilation = max(Pupil_Event, na.rm = TRUE), .groups = "drop")

t_test <- t.test(Peak_Dilation ~ Agreement_Level, data = final_analysis)
print(t_test)

final_analysis |> 
  group_by(Agreement_Level) |> 
  summarise(Mean_Peak = mean(Peak_Dilation), SD = sd(Peak_Dilation))
