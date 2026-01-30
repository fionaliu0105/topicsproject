library(tidyverse)
library(zoo)


subject_list <- c("998", "997", "996", "995") 

all_participants_data <- map_df(subject_list, process_subject_data)

write_csv(all_participants_data, "Combined_Cleaned_Pupil_Data.csv")