# 01_Inspect global seabirds data

# Packages ----
  library(tidyverse)
  library(furrr)
  library(lubridate)

# Load and combine csv file ----
  in_dir <- c("/Volumes/Extreme Pro/GST_Data/raw") # set input directory
  files <- dir(in_dir, full.names = TRUE) # get file names
  splist <- basename(files) # create string of file name
  dat <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)
  # all_dat <- walk(lapply(files, function(x) read.csv(x, stringsAsFactors = F)), rbind)
  all_df <- do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = F)))

  # summarise data ----
  all_nogls <- all_df[all_df$device!='GLS', ]
  all
  all_nogls <- all_nogls %>% 
    mutate(Year = year(date_gmt), yday = yday(date_time))
  
  # locs per day 
  loc_per_day <- all_nogls %>% group_by(scientific_name, track_id, Year, yday) %>% summarise(lpd = n())
  
  hist(loc_per_day$lpd)
  
  # number of tracking days 
  n_days <- loc_per_day %>% group_by(scientific_name, track_id, Year) %>% summarise(ndays = n())

  hist(n_days$ndays)
