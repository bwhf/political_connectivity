## Function to take political jurisdiction-overlain tracking data, and calculate the bird-days each individiual (track/bird) spends in each jur-country

# This function takes fully filtered, and classified tracking data and calculates the time spent by an individual in different jurisdictions on a monthly basis, relative to the tracked sample from a population (i.e. site_name). That is, the sample-weighted time spent by each individual in different jurisdictions for each month tracked. 

# This function adds the following:
# * adj_site_name: the site name to be used when calculating population-level time spent
# * site_name:     identifies breeding site for all species

# inFolder:  source folder containing filtered and overlain tracking data (.rds)
# outFolder: output folder in which to save summary tables of time spent  (.rds)
# over: are tracking points overlaid on EEZs or RFMO areas?
# lookup:    a lookup table for changing site names (in case certain times of year the 'population' considered changes)
# by:        "month" or "season" season is phenological period.


birddays <- function(inFolder, outFolder, over = c("EEZ", "RFMO"), lookup=NULL, by=NULL, files=NULL) {
  
  pacman::p_load(tidyverse, scales, data.table, stringr)
  
  if(is.null(by)){
    stop("Must choose: run this 'by' season, or month.")
  }
  
  if(is.null(files)){
    files <- list.files(inFolder)
  }
  
  # all species
  spp <- unique(do.call("rbind", str_split(files, n=4, pattern="_"))[, 1])
  
  

  for(i in 1:length(spp)){
    print(i)
    sp <- spp[i]            # species

    sp_files <- str_subset(files, pattern = fixed(sp))

    # load all data_sets for a species/site, and combine into one data.frame
    TD <- do.call( "rbind", lapply( sp_files, function(x) as.data.frame(readRDS(paste(inFolder, x, sep = ""))) ))

    ## change site_name based on phenological periods (for certain species only) ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # * adj_site_name is name to be used when calculating population-level time spent, 
    # * site_name identifies breeding site for all species
    if(!is.null(lookup) & !any(is.na(TD$breed_status)) ){
      TD <- left_join(TD, lookup, by=c("breed_status","scientific_name", "site_name"))
    } else {
    # print("no site lookup table provided, sites (i.e. populations) will be defaults!!")
    TD$adj_site_name <- TD$site_name
      }
    
    sp <- unique(TD$scientific_name)
    sname <- unique(TD$site_name)
    
    TD$month     <- lubridate::month(TD$date_time)
    TD$yday      <- lubridate::yday(TD$date_time)
    TD$month_dur <- Hmisc::monthDays(TD$date_time) # month duration
    
    if(by == "season"){
    ## By breeding/non-breeding season ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    TD_ndays <- TD %>% dplyr::group_by(breed_status, adj_site_name, track_id) %>% summarise(
      scientific_name  = first(scientific_name),
      site_name = first(site_name),
      dsi       = n_distinct(yday)  # number of tracking days per season for each track/individual
    )
      # calculate simple weights from the proportion of the day spent in each jurisdiction, based on number of jurs per day
      weights <- TD %>% group_by(breed_status, track_id, yday) %>% summarise(
        n_jur     = n_distinct(jur), # number of jurisdictions visited by each individual on each day tracked 
        pp_jur    = 1/n_jur          # basic proportion of time spent by a bird in each jur
      )
      # calculate daily weights (prop. of day spent) for each jur-yday combo
      weighted <- TD_weights %>% group_by(breed_status, adj_site_name, track_id, yday, jur) %>% summarise(
        day_wei = first(pp_jur))
      
      weighted <- merge(weighted, TD_ndays) # combine jur, daily weights, and dmi (monthly) weight
    
    # Make bird-days dataset
    brdy <- weighted %>% group_by(breed_status, jur, track_id, adj_site_name) %>% summarise(
      scientific_name  = first(scientific_name),
      site_name        = first(site_name),
      dsi    = first(dsi),
      dsei   = sum(day_wei), # dsei = day-season-jur-individual
      ppt    = dsei/dsi     # proportion of time (days.in.jur[x]/tot.days.tracked.in.season[y])
    )
    
    ## n tracks per season
    nTracks <- brdy %>% group_by(breed_status, adj_site_name) %>% summarise(n_tracks = n_distinct(track_id))
    # print(nTracks)
    
    brdy <- merge(brdy, nTracks)
    
    # calculate the contribution of each individual to the sample/population's 'story'
    brdy$ppts <- brdy$ppt*(1 / brdy$n_tracks) #MB# individual's relative time spent * (1/number of birds tracked per month)
    
    brdy <- brdy %>% mutate(
      ppts  = ppt * (1 / n_tracks)
    )
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Save
    # ## 2 ##
    brdyfname <- paste(sp, "_season_brdy.rds", sep="")
    
    saveRDS(brdy, paste(outFolder, brdyfname, sep="") )

    
    } else if(by == "month"){
    
    ## By month ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    TD_ndays <- TD %>% dplyr::group_by(month, adj_site_name, track_id) %>% summarise(
      scientific_name  = first(scientific_name),
      site_name = first(site_name),
      dmi       = n_distinct(yday),  # number of tracking days per month for each track/individual
      month_dur = first(month_dur),  # n of days in month
      month_wei = dmi / month_dur,   # weight of track based on n of tracked days in a month
      dmi2      = n_distinct(yday) * month_wei   # dmi2 = day-month-individual -> number of days of tracking data per month per track adjusted for the proportion of days in a month covered by the track
    )
    
    if(over == "RFMO"){
      TD <- TD %>% mutate( jur = ifelse(jur == "otherRFMO" | sovereign == "EEZ", "EEZ", jur) )
    }
    
    # calculate simple weights from the proportion of the day spent in each jur, based on number of jurisdictions per day
    weights <- TD %>% group_by(month, track_id, yday) %>% summarise(
      n_jur     = n_distinct(jur), # number of jurs visited by each individual on each day tracked 
      pp_jur    = 1/n_jur          # basic proportion of time spent by a bird in each jur
    )
    TD_weights <- merge(TD, weights) # merge weights to Tracking Data
    
    # calculate daily weights (prop. of day spent) for each jur-yday combo
    weighted <- TD_weights %>% group_by(month, adj_site_name, track_id, yday, jur) %>% summarise(
      day_wei = first(pp_jur))
    
    weighted <- merge(weighted, TD_ndays) # combine jur, daily weights, and dmi (monthly) weight

    # Make bird-days dataset
    brdy <- weighted %>% group_by(month, adj_site_name, jur, track_id) %>% summarise(
      scientific_name  = first(scientific_name),
      site_name = first(site_name),
      dmi    = first(dmi),
      dmi2   = first(dmi2),
      dmei   = sum(day_wei), # dmei = day-month-jur-individual
      dmei2  = sum(day_wei*month_wei),  # dmei, adjusted both for daily (multiple jurs per day) and monthly weights (track duration)  # ppt = proportion of time (days.in.jur[x]/tot.days.trckd.in.month[y])
      ppt    = dmei/dmi,     # proportion of time (days.in.jur[x]/tot.days.in.month[y])
      ppt2   = dmei2/dmi2,   
      ppt3   = dmei/first(month_dur) # prop. of time spent, assuming dmi = month_dur (i.e. track time represenst month)
      
    )
    
    ## n tracks per month
    nTracks <- brdy %>% group_by(month, adj_site_name) %>% summarise(n_tracks = n_distinct(track_id))
    # print(nTracks)
    # nTracks <- TD %>% group_by(month, adj_site_name) %>% summarise(n_tracks = first(n_tracks_month))
    
    brdy <- merge(brdy, nTracks)
    
    # calculate the contribution of each individual to the sample/population's 'story'
    brdy <- brdy %>% mutate(
      ppts  = ppt * (1 / n_tracks),
      ppts2 = ppt2 * ( 1 / n_tracks),
      ppts3 = ppt3 * ( 1 / n_tracks)
    )
    
    if(over == "RFMO"){
      brdy <- brdy %>% filter(jur != "EEZ" & jur != "otherRFMO") # remove calculations for other areas besides focal RFMO
    }

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Save
    # ## 2 ##
    brdyfname <- paste(sp, "_month_brdy.rds", sep="")
    
    saveRDS(brdy, paste(outFolder, brdyfname, sep="") )
    }
    
  }
  
}
