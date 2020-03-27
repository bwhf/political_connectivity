# Create table summarizing data per population for each species

pacman::p_load(stringr, dplyr, lubridate)

# folder <- 'data_test/all_TD/'
# folder <- 'data_test/no_eqnx_downsampled/' # run this for pre-month_filter summary
# folder <- 'data_test/month_filtered/'        # run this for pre-landlocked
folder <- 'data/analysis/oveez/'                 # run this for fully-filtered summary

files <- list.files(folder)

spp_in_repo <- unique(do.call("rbind", stringr::str_split(list.files(folder), pattern="_") )[, 1])

sum_list <- vector("list", length(spp_in_repo))
track_dur_list <- vector("list", length(spp_in_repo))

## summary by species, site, and device type ## 

for(i in 1:length(spp_in_repo)){
  print(i)
  one <- spp_in_repo[i]
  
  sp_files <- files[str_detect(files, pattern=one)]
  
  TD <- ungroup(do.call("rbind", lapply( sp_files, function(x) readRDS(paste0(folder, x)) )))  # for after filtering summary
  # TD <- ungroup(do.call("rbind", lapply( sp_files, function(x) as.data.frame(data.table::fread(paste0(folder, x), stringsAsFactors = F)) )))  # for before filtering summary
  
  TD$date_time <- as.POSIXct(paste(TD$date_gmt, TD$time_gmt))
  TD$yday <- yday(TD$date_time)
  TD$track_id <- as.character(TD$track_id)
  
  # summarize track durations - if track shorter than 24h, summarized as == 1 day (b/c effectively one point a day)
  track_dur <- TD %>% group_by(scientific_name, site_name, device, track_id) %>% summarise(
    start = min(date_time),
    end   = max(date_time),
    dur   = difftime(end, start, units = "days") # continuous duration
    # dur = n_distinct(yday(date_time))  # number of tracking days (i.e. interval duration)
  ) %>% mutate(
    dur   = ifelse(dur < 1, 1, dur)
  )
  
  avg_dur <- track_dur %>% group_by(scientific_name, site_name, device) %>% summarise(
    n_tracks = n_distinct(track_id),
    sum_track_days = sum(dur),
    mn_dur = median(dur),
    mx_dur = max(dur)
  )
  
  sp_sum <- TD %>% group_by(scientific_name, site_name) %>% summarise(
    ydays_missed = 366 - n_distinct(yday),
    mn_yr = min(year(date_time)),
    mx_yr = max(year(date_time)),
    n_yrs = n_distinct(year(date_time))
  ) %>% left_join( avg_dur, by = c("scientific_name", "site_name") )
  
  sum_list[[i]] <- as.data.frame((sp_sum))
  
  track_dur_list[[i]] <- track_dur
  
}  

sum_table <- do.call("rbind", sum_list)

## SAVE ##
data.table::fwrite(sum_table, "data/analysis/summary_tables/track_summary_sp_site_device.csv")


dur_table <- do.call("rbind", track_dur_list)

# histogram of track durations
png("figures/supplement/track_dur_hist.png")
hist(dur_table$dur, 20, main=NULL, xlab="Track duration (days)")
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## summary by species and site ## 

folder <- 'data/analysis/oveez/'                 # run this for fully-filtered summary


sum_list <- vector("list", length(spp_in_repo))


for(i in 1:length(spp_in_repo)){
  print(i)
  one <- spp_in_repo[i]
  
  sp_files <- files[str_detect(files, pattern=one)]
  
  TD <- ungroup(do.call("rbind", lapply( sp_files, function(x) readRDS(paste0(folder, x)) )))  # for after filtering summary
  # TD <- ungroup(do.call("rbind", lapply( sp_files, function(x) as.data.frame(data.table::fread(paste0(folder, x), stringsAsFactors = F)) )))  # for before filtering summary
  
  TD$date_time <- as.POSIXct(paste(TD$date_gmt, TD$time_gmt))
  TD$yday <- yday(TD$date_time)
  
  # summarize track durations - if track shorter than 24h, summarized as == 1 day (b/c effectively one point a day)
  track_dur <- TD %>% group_by(scientific_name, site_name, track_id) %>% summarise(
    start = min(date_time),
    end   = max(date_time), 
    # dur   = difftime(end, start, units = "days"), # continuous duration
    dur = n_distinct(yday(date_time))  # number of tracking days (i.e. interval duration)
  ) %>% mutate(
    dur   = ifelse(dur < 1, 1, dur)
  )
  
  avg_dur <- track_dur %>% group_by(scientific_name, site_name) %>% summarise(
    n_tracks = n_distinct(track_id),
    sum_track_days = sum(dur),
    mn_dur = median(dur),
    mx_dur = max(dur)
  )
  
  sp_sum <- TD %>% group_by(scientific_name, site_name) %>% summarise(
    ydays_missed = 366 - n_distinct(yday),
    mn_yr = min(year(date_time)),
    mx_yr = max(year(date_time)),
    n_yrs = n_distinct(year(date_time))
  ) %>% left_join( avg_dur, by = c("scientific_name", "site_name") )
  
  sum_list[[i]] <- as.data.frame((sp_sum))
  
}  

sum_table <- do.call("rbind", sum_list)

data.table::fwrite(sum_table, "data/analysis/summary_tables/track_summary_sp_site.csv")
