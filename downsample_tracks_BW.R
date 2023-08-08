## Downsampling high-res data to two points per day ## 

pacman::p_load(lubridate, tidyverse)

source('C:/Users/Martim Bill/Documents/R/source_scripts/downsample_fxn.r')


### READ DATA: lapply instead of loop ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SoI <- read.csv('data/Calonectris borealis_Madeira_GLS_0607.csv')

spnames <- gsub(" ", "_", unique(SoI$scientific_name))
files <- list.files('data/', pattern = spnames[spnames=='Calonectris diomedea'], full.names = T)

TD <- do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = F))) # read multiple files, combine into one dataframe

TD <- read.csv("data/extra_data/GLS_Chizé_Birdlife_190211/ynal/Thalassarche_carteri_Amsterdam_GLS_all_ids.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TD$date_time <- as.POSIXct(strptime(paste(TD$date_gmt, TD$time_gmt, sep=' '), "%Y-%m-%d %H:%M:%S"), tz="GMT")# date-time object
# check daily distribution of points
TD <- TD %>% group_by(track_id) %>% arrange(track_id, date_time) %>% ungroup()
hist(hour(TD$date_time),12)

## Subset out only PTT and GPS data
TDhires <- TD[TD$device!='GLS', ]
TDgls <- TD[TD$device=='GLS', ]

library(fuzzyjoin)
## inspect time difference between locs, look for missing days
TDhires <- TDhires %>%
  group_by(track_id) %>%
  mutate(time_diff = as.numeric(difftime(date_time, lag(date_time), units = "days"))) %>%
  ungroup()

timelag <- TDhires %>% 
  filter(time_diff >= 1) # check missing days in data

## find time difference from noon/ midnight
downsamp2 <- function(dat) {
  test <- dat %>%
    mutate(dt_noon = as.numeric(difftime(date_time, as.POSIXct(strptime(paste(date_gmt, "12:00:00", sep=' '), "%Y-%m-%d %H:%M:%S"), tz="GMT"), units = "secs"))) %>%
    mutate(dt_midnight = as.numeric(difftime(date_time, as.POSIXct(strptime(paste(date_gmt, "23:59:59", sep=' '), "%Y-%m-%d %H:%M:%S"), tz="GMT"), units = "secs"))) %>% 
    arrange(track_id, date_gmt)
  
  test <- test %>%
    mutate(dt_noon = case_when(dt_noon < 0 ~ dt_noon*-1, TRUE ~ dt_noon), dt_midnight = case_when(dt_midnight < 0 ~ dt_midnight*-1, TRUE ~ dt_midnight))
  
  test_noon <- test %>% group_by(track_id, date_gmt) %>% filter(dt_noon == min(dt_noon)) %>% ungroup %>% 
    group_by(track_id) %>% mutate(time_diff = as.numeric(difftime(date_time, lag(date_time), units = "days"))) 
  
  test_midn <- test %>% group_by(track_id, date_gmt) %>% filter(dt_midnight == min(dt_midnight)) %>% ungroup %>% 
    group_by(track_id) %>% mutate(time_diff = as.numeric(difftime(date_time, lag(date_time), units = "days"))) 
  
  
  downsampled <- bind_rows(test_noon, test_midn) 
  downsampled <- downsampled %>% arrange(track_id, date_gmt) %>% group_by(track_id) %>% mutate(time_diff = as.numeric(difftime(date_time, lag(date_time), units = "hours"))) %>% ungroup
  
  return(downsampled)
}

species_ds <- downsamp2(TDhires)

## check if there duplicates, resulted from single data from same day, so min(dt_noon) = min(dt_midnight)
downsampled <- species_ds %>% select(-time_diff, -dt_noon, -dt_midnight)
unique(downsampled)

hist(species_ds$time_diff)

ggplot(data = species_ds, )

## RECOMBINE: recombine GLS data to downsampled data

TDdown <- rbind.data.frame(downsampled, TD[TD$device=='GLS', ])

## SUMMARIZE: check the track-wise number of points per day ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

downsampled$yday <- yday(downsampled$date_time)
pnts.a.day <- downsampled %>% group_by(track_id, yday) %>% summarise(pnts.a.day = n())

hist(pnts.a.day$pnts.a.day)

TDdown$yday <- yday(TDdown$date_time)
pnts.a.day <- TDdown %>% group_by(track_id, yday) %>% summarise(pnts.a.day = n())

hist(pnts.a.day$pnts.a.day)

## SAVE 
saveRDS(TDdown, 'data_out/downsampled/Calonectris_borealis_Madeira.rds')

# saveRDS(TDdown, 'data_test/downsampled/SCSH_Malta.Balearic.Chafarinas_test.rds')
