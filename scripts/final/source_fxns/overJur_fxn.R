## Function to overlay points on a spatial dataset of country/EEZ borders

# inFolder:  folder containing files of tracking data sets (.rds)
# files:     (optional) file names for specific files to be used w/in inFolder
# over_which: "EEZ" means overlay TD with EEZ spatial data set (only works with VLIZ Marine Regions EEZ-land union data set), "RFMO" means overlay with an RFMO spatial data set.
# spatial:   spatial dataset of EEZ or RFMO borders (.shp)
# outFolder: folder in which to save output (.rds)
# assign: assign the Falklands/Malvinas and South Georgia/Georgia del Sur to either the "UK", or "ARG"

overJur <- function(inFolder, files=NULL, over_which = c("EEZ", "RFMO"), spatial, filter_landlocked=FALSE, assign=NULL, outFolder){
  
  pacman::p_load(sp, raster)
  
  if(is.null(files)){ # if files not provided, find files in inFolder
    files <- list.files(inFolder)
  } else {
    files <- files
  }
  
  # assign Disputed areas to either UK or Argentina (if assign is set, if not, assumed to RFMO analysis)
  if(is.null(assign)){spatial <- spatial}
  else if(assign == "UK"){
    sovereign <- "United Kingdom"  
  } else if(assign == "ARG"){
    sovereign <- "Argentina"  
  }
  
  if(!is.null(assign)){
    spatial@data$Svrgn_f <- ifelse(spatial@data$Cntry_f=="Falkland Islands" | spatial@data$Cntry_f=="South Georgia & the South Sandwich Islands", sovereign, spatial@data$Svrgn_f)
  }
  
  wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing
  
  words <- do.call("rbind", str_split(files, n=4, "_"))
  spp_sites <- unique(paste(words[,1], words[,2], sep="_"))
  
  for(i in 1:length(spp_sites)){
    print(paste(i, spp_sites[i]))
    
    spsi_files <- str_subset(files, pattern = fixed(spp_sites[i]))
    
    TD.list <- lapply(spsi_files, function(x) readRDS(paste(inFolder, x, sep="")))
    names(TD.list) <- spsi_files
    
    TD.list <- lapply(TD.list, function(x) {  # filter out rows with NA in either LAT or LON columns
      x$latitude <- as.numeric(x$latitude)
      x$longitude <- as.numeric(x$longitude)
      NAs2Rmv <- x[-which(is.na(x$latitude) | is.na(x$longitude)), ]
      if(nrow(NAs2Rmv) > 0){
      x <- x[-which(is.na(x$latitude) | is.na(x$longitude)), ]
      return(x)
      } else { return(x) }
    }
      )
    
    TDsp.list <- lapply(TD.list, function(x) SpatialPointsDataFrame(data=x, SpatialPoints(
      data.frame(x$longitude, x$latitude),
      proj4string = wgs84
    ))
    )
    
    # overlay points with unioned eez-countries dataset (1 step)
    if(over_which == "EEZ"){
      TD.list <- lapply(TDsp.list, function(x, i) {
        oveez <- over(x, spatial)
        
        x$eez_name <- as.character(oveez$Cntry_f) #MB# add overlay results (EEZ) to SPntsDF
        x$jur      <- as.character(oveez$Svrgn_f)
        
        x$landlocked <- oveez$landlocked # add whether landlocked to points
        
        x$jur[is.na(x$jur)] <- "High seas" #MB# NAs to 0s (high seas)
        
        x <- x@data
        
        if(filter_landlocked == TRUE){
          x <- x %>% dplyr::filter(landlocked == 0 | jur == "High seas") # REMOVE points falling in landlocked countries
        }

        return(x)
        }
      )
    } else if (over_which == "RFMO") {
      # see link for dealing with inland points: https://gis.stackexchange.com/questions/225102/calculate-distance-between-points-and-nearest-polygon-in-r
      TD.list <- lapply(TDsp.list, function(x, i) {
        ovrfmo <- sp::over(x, spatial)
        
        x$jur <- as.character(ovrfmo$RFB) #MB# add overlay results (EEZ) to SPntsDF
        
        x$jur[is.na(x$jur)] <- "otherRFMO" #MB# NAs to 0s (high seas)
        
        x <- x@data
        
        return(x)
        }
      )  
    }
    
    for(i in 1:length(TDsp.list)){
      oneTD <- TD.list[[i]]
      
      fname <- names(TDsp.list)[[i]]
      thePath   <- paste0(outFolder, fname)
      
      ## Save overlain tracking data ##
      saveRDS(oneTD, thePath)
    }
    
    
  }
}


