# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to read in multiple .csv files for a target species and downsample them to 12 hour intervals 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# species argument should be scientific name (e.g. "Thalassarche steadi")
# inFolder is path to folder where csv files are stored
# hists = TRUE if you want to see a before and after histograme of fixes per day
# saveit, if TRUE an Rdata file will be saved for each 'sitename' within the species
# outFolder: if saveit=T, specify the path where the Rdata files should be saves (MUST end in "/")
# interval: between points time interval (h) to set data to

downsample <- function(species, inFolder, outFolder, interval=24, redo=TRUE, hists=TRUE, saveit=TRUE, GLS=FALSE) {
  
  pacman::p_load(lubridate, tools)
  
  source("C:/Users/Martim Bill/Documents/R/source_scripts/SubSamp_fxn.r") # call workhorse SubSamp() fxn
  
  files <- list.files(inFolder, pattern = species, full.names = T)
  
  if(GLS == FALSE){ # optionally  exclude GLS data sets from downsampling
    files <- grep(pattern = "GLS", files, value=T, invert = T)
  }
  
  if(redo == FALSE){  # don't redo datasets which already have been done previously
  
    datasets2skip <- do.call("rbind", str_split(
      str_subset(list.files(out), pattern = species), pattern = fixed(".")))[, 1]
    
    if(length(datasets2skip) > 0){
      datasets2rmv <- grep(paste(datasets2skip, collapse="|"), files)
      files <- files[-datasets2rmv]
    }
  
    if(length(files) == 0){
      warning(paste("No new data sets to downsample for:", species, sep=" "))
    }
  } 
  print(
    paste(length(files), "files to process", sep = " ")
  )
  
  # for(i in 1:nrow(sppnames))  { ## loop over all species 
  
  # files <- list.files('data/all_spp', pattern = sppnames[sppnames==species], full.names = T) ## select one species

  if(length(files)>0) { # if there are files to downsample, do so!
      if(file_ext(files[1])=="csv"){
        TD <- do.call("rbind", lapply(files, function(x) as.data.frame(data.table::fread(x, integer64 = "character"))) ) # read all of one species' files, combine into DF
      } else if(file_ext(files[1])=="rds"){
        TD <- do.call("rbind", lapply(files, function(x) readRDS(x)) ) # read all of one species' files, combine into DF
      }
    
    #### Meld Scopoli's islands into Island Group ####
    if(any(c('Calonectris diomedea', 'Calonectris leucomelas') == species)){
      TD$site_name <- ifelse(TD$site_name=="Filfla", "Malta", TD$site_name)
      TD$site_name <- ifelse(TD$site_name=="Gozo", "Malta", TD$site_name)
      TD$site_name <- ifelse(TD$site_name=="Sanriku Coast", "Iwate", TD$site_name)
      
    }
    
    TD$date_time <- lubridate::parse_date_time(paste(TD$date_gmt, TD$time_gmt, sep=' '), "ymd_HMS", tz = "UTC")
    TD <- TD %>% group_by(track_id) %>% arrange(date_time) # sort by time, w/in individual tracks
    
    TDhires <- TD
    
    downsample_list <- lapply(split(TDhires, TDhires$dataset_id), function(x) SubSamp(x, interval) ) # downsample each dataset 
    downsampled <- do.call('bind_rows', downsample_list)

    rm(TDhires)
    rm(downsample_list)
    TDdown <- downsampled
    
    ###
    if(hists==TRUE){
      par(mfrow=c(1,2))
      TD$yday <- yday(TD$date_time)
      pnts.a.day <- TD %>% group_by(track_id, yday) %>% summarise(pnts.a.day = n())
      
      hist(pnts.a.day$pnts.a.day)
      
      TDdown$yday <- yday(TDdown$date_time)
      pnts.a.day <- TDdown %>% group_by(track_id, yday) %>% summarise(pnts.a.day = n())
      
      hist(pnts.a.day$pnts.a.day)
    }
    
    if(saveit == TRUE){
      for(s in 1:n_distinct(TDdown$dataset_id)) { ## subset by site and save (this also combines data sets w/in a site to one data set)
        onesite  <- TDdown[TDdown$dataset_id==unique(TDdown$dataset_id)[s], ]
        species  <- first(onesite$scientific_name)
        site     <- first(onesite$site_name)
        data_set <- first(onesite$dataset_id)
        device   <-  first(onesite$device)
        
        onesite$yday <- NULL
        onesite$year <- NULL
        
        if(nchar(data_set) < 5) {
          saveRDS(onesite, 
            paste(
              paste(outFolder,  species, "_", site, "_", sep=""), 
              paste(device, data_set, sep="_"), ".rds", sep="")
          )
        } else { 
          saveRDS(onesite, 
            paste(
              paste(outFolder,  data_set, sep=""), ".rds", sep=""
            )
          )
        }
  
      }
    } else {
      TD.list <- split(TDdown, TDdown$track_id)
      return(TD.list)
    }
  }
}
