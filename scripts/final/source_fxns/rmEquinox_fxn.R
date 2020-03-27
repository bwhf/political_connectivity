## Filter out equinox days for GLS data ##

# inFolder:  path to folder containing data sets with GLS data, and "GLS" somewhere in the file name to ID it as such
# outFolder: path to folder where filtered data ought to be saved (as .rsd files)
# files:    (optional) specific filenames w/in inFolder to load (for filtering)


rmEquinox <- function(inFolder, outFolder, files=NULL) {
  
  pacman::p_load(dplyr, lubridate)
  
  files <- list.files(inFolder, full.names = T)
  
  for(i in 1:length(files)){
    
    one <- data.table::fread(files[i], integer64 = "character")
    
    one$yday <- lubridate::yday(one$date_time)
    
    # get info for file name
    sp     <- first(one$scientific_name)
    site   <- first(one$site_name)
    print(paste(i, ": ", sp, "  ", site, sep=""))
    device <- first(one$device)
    data_set_id <- first(one$dataset_id)
    
    if(nchar(data_set_id) < 5) { # rigamarole for different data set ids (BirdLife/otherwise)
      thePath <- paste(outFolder, sp, "_", site, "_", device, "_", data_set_id, ".rds", sep="")
    } else { thePath <- paste(outFolder, data_set_id, ".rds", sep="") }
    
    if( stringr::str_detect(files[i], pattern="GLS") ){ # if it's a GLS data set, run filter
      
      # one$date_time <- lubridate::parse_date_time(paste(one$date_gmt, one$time_gmt, sep=' '), "ymd_HMS", tz = "UTC")
      
      if(any(!is.na(one$equinox)) & any(one$equinox == "SST" )){ # don't filter data with SST-derived lats
        saveRDS(one, thePath)
        
      } else {
        
        one.f <- one %>% dplyr::filter(
          (yday < (80 - 21) | yday > (80 + 7)) & (yday < (264 - 7) | yday > (264 + 21))
        ) # filter out days 21 days prior to and 7 days after ~March20th, and 7 days prior to and 21 days following ~Sept.20th
        # one.f$yday <- NULL
        
        saveRDS(one.f, thePath)
        
      }
      
    } else {   # if not, just save it as .rds
      # one <- data.table::fread(files[i], integer64 = "character")
      data_set_id <- one$dataset_id[1]
      
      saveRDS(one, thePath)
      
    }
    
  }
  
  
}
