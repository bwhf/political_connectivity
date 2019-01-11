## Downsampling high-res data to two points per day ## 

pacman::p_load(lubridate, dplyr)

source('C:/Users/Martim Bill/Documents/R/source_scripts/downsample_fxn.r')


### READ DATA: lapply instead of loop ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SoI <- read.csv('data_summaries_FINAL/species_globpop_coverage.csv')

fnames <- gsub(" ", "_", unique(SoI$species))
files <- list.files('data/all_spp', pattern = fnames[fnames=='Calonectris_borealis'], full.names = T)

TD <- do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = F))) # read multiple files, combine into one dataframe


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TD$date_time <- as.POSIXct(strptime(paste(TD$date_gmt, TD$time_gmt, sep=' '), "%Y-%m-%d %H:%M:%S"), tz="GMT")# date-time object
# check daily distribution of points
hist(hour(TD$date_time),12)

## Subset out only PTT and GPS data
TDhires <- TD[TD$device!='GLS', ]

## DOWNSAMPLE: retain times as close to a 12hour interval centered on noon and midnight as possible given the data ~~~~~~~~~~~~~~~~~~~~~~~~~~
before <- Sys.time()
downsample_list <- lapply(split(TDhires, TDhires$dataset_id), function(x) SubSamp(x, 12))
Sys.time() - before

downsampled <- do.call('bind_rows', downsample_list)
# check daily distribution of points AFTER downsampling
hist(hour(downsampled$date_time),12)


## RECOMBINE: recombine GLS data to downsampled data

TDdown <- rbind.data.frame(downsampled, TD[TD$device=='GLS', ])


## SUMMARIZE: check the track-wise number of points per day ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

downsampled$yday <- yday(downsampled$date_time)
pnts.a.day <- downsampled %>% group_by(track_id, yday) %>% summarize(pnts.a.day = n())

hist(pnts.a.day$pnts.a.day)

TDdown$yday <- yday(TDdown$date_time)
pnts.a.day <- TDdown %>% group_by(track_id, yday) %>% summarize(pnts.a.day = n())

hist(pnts.a.day$pnts.a.day)
