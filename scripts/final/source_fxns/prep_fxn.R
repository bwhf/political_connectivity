######### Function to do the following: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1 Remove extraneous columns
#2 Filter to only first 365 days for each track
#3 Filter out high latitude fixes (i.e. error fixes which may be missed by equinox filter)
#4 Standaridize filenames and dataset_ids

# inFolder <- "data/all_TD/"
# outFolder <- "data_test/all_TD"
# 
# standard <- data.table::fread("data/standard_site_names.csv")


prep <- function(inFolder, standard, files=NULL, outFolder){
  
  pacman::p_load(stringr, data.table, dplyr)
  
  files <- list.files(inFolder)
  
  report.list <- vector(mode = "list", length = length(files))
  
  for(i in 1:length(files)){
    print(i)
    
    # 1 # Remove unnecessary columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    one <- as.data.frame(data.table::fread(paste(inFolder, files, sep="")[i], integer64 = "character"))
    
    one <- one %>% dplyr::select(
      dataset_id, scientific_name, common_name, site_name, colony_name, lat_colony, lon_colony, device, track_id, bird_id, breed_stage, breed_status, equinox, date_gmt, time_gmt, latitude, longitude
    )    
    
    
    # 2 # Retain only first 365 days of tracking (per track)   ~~~~~~~~~~~~
    
    if(is.null(one$date_time)){
      one$date_time <- lubridate::parse_date_time(paste(one$date_gmt, one$time_gmt, sep=' '), "ymd_HMS", tz = "UTC")
    }
    one$year <- year(one$date_time)
    one$yday <- yday(one$date_time)
    
    # by track, retain only first 365 days of data
    xx <- lapply(split(one, one$track_id), function(x) {
      
      one <- x
      
      first.cal.yr <- dplyr::filter( x, year == sort(unique(x$year))[1] ) %>% arrange(track_id, date_time)
      startyday <- min(unique(first.cal.yr$yday))
      
      second.cal.yr <- dplyr::filter( x, year == sort(unique(x$year))[2] )
      second.cal.yr <- dplyr::filter( second.cal.yr, yday < startyday) %>% arrange(track_id, date_time)
      
      newone <- rbind.data.frame(first.cal.yr, second.cal.yr) %>% arrange(track_id, date_time)
      
      newone$yday <- NULL
      newone$year <- NULL
      
      return(newone)
    }
      
    )  
    
    xxx <- do.call("rbind.data.frame", xx)
    
    
    # 3 # Filter out extreme latitude points ##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    newone <- dplyr::filter(xxx, latitude < 65 & latitude > -75)
    # print(paste( (nrow(newTD) - nrow(newTDX)), "of", nrow(newTD), "removed,", round((nrow(newTDX) / nrow(newTD) * 100) - 100), "%"))
    
    report <- data.frame(
      "scientific_name" = rep(newone$scientific_name), 
      "site_name" = rep(newone$site_name), 
      "dataset_id" = rep(newone$dataset_id), 
      "n_pnts_rmvd" = (nrow(xxx) - nrow(newone)), 
      "perc_pnts_rmvd" = abs( (nrow(newone) / nrow(xxx) * 100) - 100 ),
      stringsAsFactors = FALSE
    )
    
    report.list[[i]] <- report
    
    # 4 # Fill track_id with bird_id if NA  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    newone$bird_id <- with(newone, ifelse((is.na(bird_id) | bird_id == "NA"), track_id, bird_id))
    
    
    # 5 # Standardize file names  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    file <- files[i]
    
    site      <- strsplit(file, "_")[[1]][2] # name of site in file name 
    name2     <- standard$other_name_1
    name3     <- standard$other_name_2
    name4     <- standard$other_name_3
    whichname <- which(data.frame((site == standard$final_name), (site == standard$acap_pop_islgrp), (site == name2), (site == name3), (site == name4))==TRUE, arr.ind=T)[1] # get row where name matches one of options
    
    final_name <- standard$final_name[whichname]
    
    if(is.na(final_name)){
      stop("Site name not found in `data/standard_site_names.csv` table")
    }
    
    site_name <- newone$site_name[1] # name of site in data 
    
    if( site != final_name | site_name != final_name ){ 
      # does site_name in data equal new name? If not, make it so!
      
      if(nchar(first(as.character(newone$dataset_id))) > 4){ # if an extra_data set, change the dataset_id to reflect standard site_name
        newone$dataset_id <- sub(site, final_name, newone$dataset_id)
        print(first(newone$dataset_id))
        newone$site_name <- rep(final_name)
        newfile <- sub(site, final_name, file)
        data.table::fwrite(newone, paste(outFolder, file, sep=""))
        # file.remove(paste(inFolder, file, sep="")) # remove old file
        
      } else {
        newone$site_name <- rep(final_name)
        newfile <- sub(site, final_name, file)
        # file.remove(paste(inFolder, file, sep="")) # remove old file
        data.table::fwrite(newone, paste(outFolder, newfile, sep=""))
        
      }
      
    } else {
      data.table::fwrite(newone, paste(outFolder, file, sep=""))
    }
    
  }
  
  return(do.call("rbind", report.list))
  
}
