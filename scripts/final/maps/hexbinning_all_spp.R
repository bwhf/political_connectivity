#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Binning tracking data over a global hexogonal grid surface # 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 21-Nov-19
#----------------------------------------------------------------------------#

pacman::p_load(dggridR, sf, tidyverse)

## Choose whether to use high threshold or low threshold data (i.e. >1 bird per month) ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# thresh <- "high"
thresh <- "low"

if(thresh == "high"){
  master <- "data/analysis/bird_thresh/"
  master_figs <- "figures/bird_thresh/"
} else {
  master <- "data/analysis/"
  master_figs <- "figures/"
}


## Choose which country to assign Falklands/S.Georgia breeders too ~~~~~~~~~~
assign <- "A"
# assign <- "B"

# use popData sheet to associate site_name in TD to an origin country (ONLY GOOD FOR MEASURES OF VISITATION)
popData <- read.csv('data/population_estimates.csv', stringsAsFactors = F)

## change assignment of birds breeding in disputed areas 
if(assign == "A"){
  folder <- paste0(master, "data/analysis/glob_count/")
} else if(assign == "B"){
  popData$jurisdiction <- ifelse(popData$site_name == "Falkland Islands (Islas Malvinas)" | popData$site_name == "South Georgia (Islas Georgias del Sur)", "Argentina", 
    ifelse(popData$site_name == "Chafarinas", "Morocco", popData$jurisdiction))
  popData$origin <- ifelse(popData$site_name == "Falkland Islands (Islas Malvinas)" | popData$site_name == "South Georgia (Islas Georgias del Sur)", "Argentina", ifelse(popData$site_name == "Chafarinas", "Morocco", popData$jurisdiction))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
##### Create a grid, and loop through species-sites to bin data per cell (i.e. richness, points)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``

# Construct a global grid with cells approximately X km across 
dggs <- dgconstruct(spacing=100, metric=TRUE, resround='nearest') # spacing unit  CLS (km)

# LOOP through spp-sites
inFolder <- paste0(master, 'oveez/')
files <- list.files(inFolder)
files

words <- do.call("rbind", str_split(files, n=4, "_"))
spp_sites <- unique(paste(words[,1], words[,2], sep="_"))

cellcnt_list <- list()

Nspp_sites <- length(spp_sites)


visit_rich = FALSE  # should richness be true richness, or visiting richness?  (visit. rich. filters out points within origin EEZs)
by_month   = FALSE # should richness be calculated for each month (for making monthly maps)


for(i in 1:Nspp_sites){
  print(paste(i, spp_sites[i]))
  
  spsi_files <- str_subset(files, pattern = fixed(spp_sites[i]))
  
  TD.list <- lapply(spsi_files, function(x) as.data.frame(readRDS(paste(inFolder, x, sep=""))) )
  names(TD.list) <- spsi_files
  
  TD <- do.call("rbind", TD.list)
  
  if(visit_rich == TRUE) {
    # use popData sheet to associate site_name in TD to an origin country, and add this as a column (ONLY GOOD FOR MEASURES OF VISITATION)
    PDsite     <- popData[popData$standard_site_name %in% unique(TD$site_name), ]
    if( nrow(PDsite) != 0 ){ 
      TD$origin <- rep(PDsite[PDsite$scientific_name %in% unique(TD$scientific_name), ]$origin)
 } else { next }

    TD$origin  <- rep(PDsite[PDsite$scientific_name %in% unique(TD$scientific_name), ]$origin)
    
    TD <- TD[TD$origin != TD$jur, ] # filter out all points for this population w/in it's origin EEZ (based on jurisdiction) (more conservative than deleting cells)
  }
  
  #Get the corresponding hexcells for each bird fix (lat-long pair)
  TD$cell <- dgGEO_to_SEQNUM(dggs, TD$longitude, TD$latitude)$seqnum
  
  cellcnt   <- TD %>% group_by(cell, month) %>% summarise(
    scientific_name = first(scientific_name),
    fixcount    = n(),
    richness    = 1
  )
  
  # cellcnt <- cellcnt %>% filter(fixcount > 1) # optional filter on grids with low fixcounts (i.e. 1 point only)
  
  cellcnt_list[[i]] <- cellcnt
}


cellcnt_df <- do.call("rbind", cellcnt_list)



if(by_month == TRUE){
  # across species, month-by-month
  cellcnt_sum <- cellcnt_df %>% group_by(cell, scientific_name, month) %>% summarise(
    fixcount = sum(na.omit(fixcount)),
    richness = 1
  ) %>% group_by(cell, month) %>% summarise(
    fixcount = sum(na.omit(fixcount)),
    richness = sum(na.omit(richness))
  )
  
  saveRDS(cellcnt_sum, paste0(master, "glob_hexgrid/cellcnt_sum_452km_trich_bymonth.rds"))
  
} else{
  ## summarize stats per cell (binning) ##
  # by species
  cellcnt_sp_sum <- cellcnt_df %>% group_by(cell, scientific_name) %>% summarise(
    fixcount = sum(na.omit(fixcount)),
    richness = 1
  )
  
  # across species
  cellcnt_sum <- cellcnt_sp_sum %>% group_by(cell) %>% summarise(
    fixcount = sum(na.omit(fixcount)),
    richness = sum(na.omit(richness))
  )
  
  # grid <- dgcellstogrid(dggs, cells = as.numeric(cellcnt_sum$cell), frame=TRUE, wrapcells=TRUE) # get only cells which contained fixes
  grid <- dgcellstogrid(dggs, cells = as.numeric(cellcnt_sum$cell))
  grid <- grid %>% rename("cell" = "seqnum")
  grid <- merge(grid, cellcnt_sum, by.x="cell")
  
  if(visit_rich==TRUE){
    
    if(assign=="A"){
      saveRDS(grid, paste0(master, "glob_hexgrid/global_hexgrid_95km_vrich.rds"))
    } else if(assign=="B"){
      saveRDS(grid, paste0(master, "glob_hexgrid/glob_hexgrid/global_hexgrid_95km_vrich.rds"))
    }
  } else {
    if(assign=="A"){
      saveRDS(grid, paste0(master, "glob_hexgrid/global_hexgrid_95km_trich.rds"))
    } else if(assign=="B"){
      saveRDS(grid, paste0(master, "sovereign_B_assign/glob_hexgrid/global_hexgrid_95km_trich.rds"))
    }
  }
  
}
