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

  if(over_which == "EEZ"){
    ## Make adjustments to sovereignty information in EEZ data set ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SOVEREIGN <- spatial@data$SOVEREIGN1
    
    multisover <- apply(cbind(spatial@data$SOVEREIGN1, spatial@data$SOVEREIGN2, spatial@data$SOVEREIGN3), 1, 
      function(x) paste(x[!is.na(x)], collapse = "/"))
    SOVEREIGN <- ifelse(spatial@data$POL_TYPE == "Joint regime (EEZ)", paste("Joint regime", multisover), SOVEREIGN)
    SOVEREIGN <- ifelse(spatial@data$POL_TYPE == "Overlapping claim", paste("Disputed", multisover), SOVEREIGN)
    SOVEREIGN <- ifelse(spatial@data$UNION == "Overlapping claim South China Sea", "Disputed CHN/VNM/PHL/TWN/MAL", SOVEREIGN)
    SOVEREIGN <- ifelse(spatial@data$UNION == "Chagos Archipelago", "Disputed Mauritius/United Kingdom", SOVEREIGN)
    SOVEREIGN <- ifelse(SOVEREIGN == "Republic of Mauritius", "Mauritius", SOVEREIGN)
    # assign Disputed breeding areas to one or other countries claiming sovereignty (if assign is set, if not, assumed to RFMO analysis)
    if(is.null(assign) | assign == "A"){
      sovereign <- c("United Kingdom", "Spain")
    } else if(assign == "B"){
      sovereign <- c("Argentina", "Morocco")  
    }
    
    SOVEREIGN <- ifelse(spatial@data$UNION=="Falkland Islands" | spatial@data$UNION=="South Georgia & the South Sandwich Islands", sovereign[1], SOVEREIGN)
    spatial@data$SOVEREIGN <- ifelse(spatial@data$UNION=="Chafarinas Islands" , sovereign[2], SOVEREIGN)
  }
  
  ## Load tracking data ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing
  
  words <- do.call("rbind", str_split(files, n=4, "_"))
  spp_sites <- unique(paste(words[,1], words[,2], sep="_"))
  
  for(i in 1:length(spp_sites)){
    print(paste(i, spp_sites[i]))
    
    spsi_files <- str_subset(files, pattern = fixed(spp_sites[i]))
    
    TD.list <- lapply(spsi_files, function(x) readRDS(paste(inFolder, x, sep="")))
    names(TD.list) <- spsi_files
    
    TDsp.list <- lapply(TD.list, function(x) SpatialPointsDataFrame(data=x, SpatialPoints(
      data.frame(x$longitude, x$latitude),
      proj4string = wgs84
    ))
    )
    
    # overlay points with unioned eez-countries dataset (1 step)
    if(over_which == "EEZ"){
      
      TD.list <- lapply(TDsp.list, function(x, i) {
        oveez <- over(x, spatial)
        
        x$eez_name <- as.character(oveez$UNION) #MB# add overlay results (EEZ) to SPntsDF
        x$jur      <- as.character(oveez$SOVEREIGN)
        x$landlocked <- ifelse(as.character(oveez$POL_TYPE)=="Landlocked country", TRUE, FALSE)

        x$jur[is.na(x$jur)] <- "High seas" #MB# NAs to 0s (high seas)
        x <- x@data
        x <- x %>% group_by(month) %>% summarise(n_tracks_month = n_distinct(track_id)) %>% left_join(x, by="month") # monthly sample size
        
        if(filter_landlocked == TRUE){
          x <- x %>% dplyr::filter(landlocked == FALSE | jur == "High seas") # REMOVE points falling in landlocked countries
        }

        return(x)
        }
      )
    } else if (over_which == "RFMO") {
      # see link for dealing with inland points: https://gis.stackexchange.com/questions/225102/calculate-distance-between-points-and-nearest-polygon-in-r
      
      TD.list <- lapply(TDsp.list, function(x, i) {
        x$sovereign <- if_else(x$jur=="High seas", "High seas", "EEZ")  # identifier column of whether point is in high seas or EEZ
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


