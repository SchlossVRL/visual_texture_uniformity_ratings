#clear environment 
rm(list=ls())

#libraries
library(tidyverse)
library(skimr)


#preprocessing
#set path to data
data_path <- ("data")

#list files in directory
csv_files <- list.files(data_path, pattern = "*.csv", full.names = TRUE)

#load files into list
data_list <- lapply(csv_files, read.csv)

#combine into single data frame
data <- do.call(rbind, data_list)

#peak
glimpse(data)

#data_copy <- data

#clean
data <- data %>% 
  mutate(
    practiceTrial = ifelse(practiceTrial == "", NA, TRUE)) %>% 
  filter(trial_type == "html-slider-response") %>% # getting trials with that trial type
  filter(is.na(practiceTrial)) %>% #remove practice trials
  select(subject_id, image_path, response) 

#save output csv
write.csv(data, "uniformity_ratings_clean.csv", row.names = FALSE)
