# 01_Inspect global seabirds data

# Packages ----
  library(tidyverse)
  library(furrr)
  library(lubridate)
  library(dlookr)

# Load and combine csv file ----
  in_dir <- c("/Volumes/Extreme Pro/GST_Data/raw") # set input directory
  files <- dir(in_dir, full.names = TRUE) # get file names
  splist <- basename(files) # create string of file name
  dat <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)
  # all_dat <- walk(lapply(files, function(x) read.csv(x, stringsAsFactors = F)), rbind)
  all_df <- do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = F)))

  # summarise data ----
  all_nogls <- all_df[all_df$device!='GLS', ]
  all_nogls <- all_nogls %>% 
    mutate(Year = year(date_gmt), yday = yday(date_time))
  
  all_nogls <- all_nogls%>%
    separate(scientific_name, into = c("genus", "species"), sep = " ", remove = FALSE)
  
  # locs per day 
  loc_per_day <- all_nogls %>% group_by(genus, species, scientific_name, track_id, Year, yday) %>% summarise(lpd = n()) 
  
  min(loc_per_day$lpd) # min loc per day = 1
  one_pd <- loc_per_day %>% filter(lpd <= 1) %>% group_by(genus, species, scientific_name) %>% summarise() # 31 species
  hist(loc_per_day$lpd)
  
  # binning data
  loc_per_day$bins <- binning(loc_per_day$lpd, nbins = 5, type = "quantile")
  loc_per_day$bins
  #     [1,2]     (2,5]     (5,7]    (7,12] (12,1081] 
  #     15337     20729     10631     14532     13293 
  
  two_pd <- loc_per_day %>% filter(lpd >= 2)
  two_pd$bins <- binning(two_pd$lpd, nbins = 5, type = "quantile")
  two_pd$bins
  #  [2,3]     (3,5]     (5,8]    (8,12] (12,1081] 
  #  15305     13351     15068     10095     13293 
  
  n_two_pd <- two_pd %>% group_by(genus, species, scientific_name, track_id, Year, yday) %>% summarise()
  
  ggplot(data = n_two_pd, aes(y = scientific_name, x = yday, color = scientific_name)) + geom_jitter() + theme_gray(base_size = 14) # year round data distribution
  
  ndays_two_pd <- n_two_pd %>% group_by(genus, species, scientific_name, track_id, Year) %>% summarise(ndays = n())
  
  ggplot(data = ndays_two_pd, aes(x = species, y = ndays, color = scientific_name)) + geom_violin() + geom_point() + facet_wrap(vars(genus), scales = "free") + theme_gray(base_size = 14) + theme(axis.text.x = element_text(angle = -90))
  
  # number of tracking days per individual
  n_days <- loc_per_day %>% group_by(genus, species, scientific_name, track_id, Year) %>% summarise(ndays = n())

  hist(n_days$ndays)

  ggplot(data = n_days, aes(x = species, y = ndays, color = scientific_name)) + geom_violin() + geom_jitter() + facet_wrap(vars(genus), scales = "free") + theme(axis.text.x = element_text(size=12, angle = -90))
  
  # unique tracking days per species (n-th day of the year)
  utd <- all_nogls %>% group_by(genus, species, scientific_name, Year, yday) %>% summarise()
  
  ggplot(data = utd, aes(y = scientific_name, x = yday, color = scientific_name)) + geom_jitter() + theme_gray(base_size = 14)
  
  # unique tracking year
  uty <- utd %>% group_by(genus, species, scientific_name, Year) %>% summarise(nday = n())
  
  ggplot(data = uty, aes(x = Year, y = nday, color = scientific_name)) + geom_point() + facet_wrap(vars(scientific_name), scales = "free") +  theme_gray(base_size = 14)
  ## visualising number of unique track year and no. of unique track days in each year

  n_uty <- uty %>% group_by(genus, species, scientific_name, Year) %>% group_by(genus, scientific_name) %>% summarise(nyear = n())
  