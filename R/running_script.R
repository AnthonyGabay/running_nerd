library(tidyverse)

polar <- read_csv("C:\\Users\\gabayas\\Downloads\\Anthony_Gabay_2021-02-18_12-27-48.csv") %>%
  dplyr::rename(time = Sport, 
                HR = Date,
                pace = Duration,
                distance = contains('Max Speed')) %>%
  dplyr::select(time, HR, pace, distance) %>%
  filter(!grepl("RUNNING", time)) %>%
  filter(!grepl("Time", time)) %>%
  mutate(time = lubridate::period_to_seconds(lubridate::hms(time))) %>%
  mutate(pace_seconds = lubridate::period_to_seconds(lubridate::ms(pace))) %>%
  mutate(distance = as.numeric(distance))