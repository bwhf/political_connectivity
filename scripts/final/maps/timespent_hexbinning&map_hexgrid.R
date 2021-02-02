#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create maps of tracking data binned into hex.grid, and re-centering geodata to pacific perspective # 
# Time spent # 
#*** projection transformations in this script only work with R<4.0 and lwgeom versions <2.0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-------------------------------------------------------------------------------------------------------

pacman::p_load(sf, sp, dggridR, tidyverse, lubridate, lwgeom, viridis)

## Choose whether to use high threshold or low threshold data (i.e. >1 bird per month
# thresh <- "high"
thresh  <- "low"

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

popData <- read.csv('data/population_estimates.csv', stringsAsFactors = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(assign == "A"){
  # inFolder <- paste0(master, 'noeqx_dnsmpl/')
  inFolder <- paste0(master, 'month_filtered/')
} else if(assign == "B"){
  inFolder <- paste0(master, 'sovereign_B_assign/month_filtered/')
  # re-assign birds on disputed islands to Argentina
  popData$jurisdiction <- ifelse(popData$site_name == "Falkland Islands (Islas Malvinas)" | popData$site_name == "South Georgia (Islas Georgias del Sur)", "Argentina", 
    ifelse(popData$site_name == "Chafarinas", "Morocco", popData$jurisdiction))
  popData$origin <- ifelse(popData$site_name == "Falkland Islands (Islas Malvinas)" | popData$site_name == "South Georgia (Islas Georgias del Sur)", "Argentina", ifelse(popData$site_name == "Chafarinas", "Morocco", popData$jurisdiction))
}

files <- list.files(inFolder)
files

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

spatial <- dgconstruct(spacing=360, metric=TRUE, resround='nearest')

wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing

# load data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
spp <- unique(do.call("rbind", str_split(files, n=4, pattern="_"))[, 1])

cntsum.list <- vector(mode="list", length(spp))

for( i in 1:length(spp)){
  print(i)
  sp <- spp[i]            # species
  sp_files <- str_subset(files, pattern = fixed(sp))
  
  # load all data_sets for a species/site, and combine into one data.frame
  TD <- do.call( "rbind", lapply( sp_files, function(x) as.data.frame(readRDS(paste(inFolder, x, sep = ""))) ))
  
  # hexgrid: get cell name for each fix ~~~~~~~~~~~~~~~~~
  TD$cell <- dgGEO_to_SEQNUM(spatial, TD$longitude, TD$latitude)$seqnum
  
  # Summarize monthly time spent per cell ~~~~~~~~~~~~~~~~
  sname <- unique(TD$site_name)
  TD$month     <- lubridate::month(TD$date_time)
  TD$yday      <- lubridate::yday(TD$date_time)
  TD_ndays <- TD %>% dplyr::group_by(month, site_name, track_id) %>% summarise(
    scientific_name  = first(scientific_name),
    dmi       = n_distinct(yday)  # number of tracking days per month for each track/individual
  )
  
  # calculate simple weights from the proportion of the day spent in each jur, based on number of jurisdictions per day
  weights <- TD %>% group_by(month, track_id, yday) %>% summarise(
    n_cell     = n_distinct(cell), # number of jurs visited by each individual on each day tracked
    pp_cell    = 1/n_cell          # basic proportion of time spent by a bird in each jur
  )
  
  TD_weights <- merge(TD, weights) # merge weights to Tracking Data
  # calculate daily weights (prop. of day spent) for each jur-yday combo
  
  weighted <- TD_weights %>% group_by(month, site_name, track_id, yday, cell) %>% summarise(
    day_wei = first(pp_cell))
  
  weighted <- merge(weighted, TD_ndays) # combine jur, daily weights, and dmi (monthly) weight
  # Make bird-days dataset
  brdy <- weighted %>% group_by(month, site_name, cell, track_id) %>% summarise(
    scientific_name  = first(scientific_name),
    dmi    = first(dmi),
    dmei   = sum(day_wei), # dmei = day-month-jur-individual
    ppt    = dmei/dmi     # proportion of time (days.in.jur[x]/tot.days.in.month[y])
  )
  
  ## n tracks per month
  nTracks <- brdy %>% group_by(month, cell) %>% summarise(n_tracks = n_distinct(track_id))

  brdy <- merge(brdy, nTracks)
    
  # calculate the contribution of each individual to the sample/population's 'story'
  brdy <- brdy %>% mutate(
    ppts  = ppt * (1 / n_tracks)
  )
  # popTime ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  brdysum <- brdy %>% group_by(site_name, cell, month) %>% summarise(
    scientific_name = first(scientific_name),
    n_tracks  = first(n_tracks),
    pop_time  = sum(ppts), # sum of time spent across individuals (within month, jur, population)
    samp_est  = pop_time*first(n_tracks) # est. of number of sample birds in jur at any given time (in each month)
  )
  
  PDsp      <- popData[popData$scientific_name %in% unique(brdysum$scientific_name), ]
  PDsp.site <- PDsp[PDsp$standard_site_name %in% unique(brdysum$site_name), ]      # HERE, if adj_site_names > 1, problem
  
  # combine population data and time spent/visitation summary
  cntsum <- merge(
    brdysum, PDsp.site[c("standard_site_name", "pop_estimate_IND", "global_pop_estimate_IND", "origin")],
    by.x = c("site_name"), by.y = c("standard_site_name"),
    all = TRUE
  )
  
  # normal prop. based on prop.time.spent * population_size
  cntsum$tot_atatime  <- cntsum$pop_time * cntsum$pop_estimate_IND
  
  # globcnt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cntsum$globprop <- cntsum$tot_atatime/cntsum$global_pop_estimate_IND # proportion of global population in jur in time period (month of season)
  cntsum.list[[i]] <- cntsum
  
}

cntsum_allTD <- do.call("rbind", cntsum.list)

# summarize (bin) by cell
cellcnt_sum <- cntsum_allTD %>% group_by(cell) %>% summarise(
  timespent = sum(na.omit(tot_atatime)) / 12   # either x/12 for BIRDYEAR or just sum for BIRDMONTH
)

grid <- dgcellstogrid(spatial, cells = as.numeric(cellcnt_sum$cell), frame=TRUE, wrapcells=TRUE) # get only cells which contained fixes
grid <- merge(grid, cellcnt_sum, by.x="cell")

# saveRDS(grid, paste0(master, "glob_hexgrid/global_hexgrid_452km_timespent.rds"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Make map ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("C:/Users/Martim Bill/Documents/R/source_scripts/recenter_map_fxn.r") # mapdata re-centering function

if(!exists("grid")){ grid <- readRDS(paste0(master, "glob_hexgrid/global_hexgrid_452km_timespent.rds")) }


# recentering and projection objects
shift <- -153
central_meridian <- 360 + shift
proj <- sprintf("+proj=kav7 +lon_0=%i", central_meridian)
# proj <- sprintf("+proj=kav7 +lon_0=%i", shift)


# Convert dggrid object (from 'hexbinning.r') to SF object (several steps) ~~~~~~~~~~~~
# hexgrid to list of Polygons
polylist <- lapply(split(grid, grid$cell), function(x) { 
  apoly <- Polygon(cbind(x$long, x$lat))
  apoly <- Polygons(list(apoly), ID = as.numeric(first(x$cell)))
}
)

# get attribute data for each cell
polydata <- grid %>% group_by(cell) %>% summarise(
  timespent = first(timespent)
) %>%  arrange( cell ) %>% as.data.frame()

rownames(polydata) <- polydata$cell

# convert to SPolyDF (attaching data)
grid.sp <- SpatialPolygonsDataFrame( SpatialPolygons(polylist, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), data = polydata, match.ID = T)

## sp to sf (and make valid)
grid.sf <- st_as_sf(grid.sp)
# all(st_is_valid(grid.sf)) # check validity 
grid.sf <- st_make_valid(grid.sf)
# all(st_is_valid(grid.sf)) # check validity 

## Save or Read in grid of data (in WGS84) ~~~~~~~~~~

# st_write(grid.sf, paste0(master, "global_grids/timespent_grid.shp"), delete_layer =T)

# IF GRID ALREADY EXISTS: Read in  ~~~~~~~~~~

if(!exists("grid.sf")){ grid.sf <- st_read(paste0(master, "global_grids/timespent_grid.shp")) }

# decide recentering and projection objects ~~~~~~~~~~~~~~~~~~~~~~
shift <- -153
central_meridian <- 360 + shift
proj <- sprintf("+proj=kav7 +lon_0=%i", central_meridian)
# proj <- sprintf("+proj=kav7 +lon_0=%i", shift)



# Countries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") # load country dataset
world <- rmapshaper::ms_simplify(x, keep = 0.99)

# EEZs ~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~

x <- st_as_sf( raster::shapefile("C:/Users/Martim Bill/Documents/geodata/world_EEZ_v11/eez_boundaries_v11_0_360.shp") ) # just EEZ data (latest version (2019))
x <- st_as_sf( raster::shapefile("spatial_data/shapefiles_EEZ_countries/union_countries_EEZs/EEZ_Land_v3_202030.shp") ) # EEZ-land union data (latest version (2019))
eez_cnt <- rmapshaper::ms_simplify(x, keep = .01) # simplify dataset to make smaller
# all(st_is_valid(eez_cnt))

# create background polygon for globe ~~~~~~~~~
lats <- c(90:-90, -90:90, 90)
maxlong <- 360 + (shift + 180)
minlong <- shift + 180
longs <- c(rep(c(maxlong, minlong ), each = 181), maxlong )


# turn into correctly projected sf collection
outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% st_sf()

# re-center data sets ~~~~~~~~~~~~ 
# re_eez <- recentre(eez_cnt, shift) %>% group_by(Line_ID) %>% summarize()          # for just EEZ lines data set (seam removal works)
re_eez <- recentre(eez_cnt, shift) %>% group_by(MRGID_EEZ) %>% summarize()          # seam removeal doesn't work for eez-land union

re_grid <- recentre(grid.sf, shift) %>% group_by(cell) %>% summarise(  # recenter grid! 
  timespent = first(timespent)
)
re_world <- recentre(world,  clon = shift) %>% group_by(sovereignt) %>% summarize() # recenter and remove old seam

### project datasets
# re_world_wt <- st_transform_proj(re_world, crs = proj1, use_gdal = FALSE)
re_world_prj   <- lwgeom::st_transform_proj(st_segmentize(st_buffer(re_world, -0.01), 15000), proj)
re_eez_prj     <- st_transform_proj(re_eez, crs = proj, use_gdal = FALSE)
re_grid_prj   <- st_transform_proj(re_grid, crs = proj, use_gdal = FALSE)
outline_prj   <- lwgeom::st_transform_proj(outline, crs = proj, use_gdal = FALSE)



## Map
m3 <- ggplot() +
  cowplot::theme_nothing() +
  geom_sf(data = outline_prj, color = NA, fill = "black") +
  # geom_sf(data = outline_prj, color = NA, fill = "white") +
  geom_sf(data = re_grid_prj, aes(fill=timespent/1000000), color=NA) +
  scale_fill_viridis(
    option ="inferno",
    trans  ="sqrt",
    # trans  ="log2",
    breaks = scales::trans_breaks("sqrt", function(x) x ^ 2),
    # breaks = scales::trans_breaks("log2", function(x) 2 ^ x),
    labels = function(x) round(x, 1)
    ) +
  # scale_fill_continuous_sequential(
  #   palette = "Reds 3",
  #   trans  ="sqrt",
  #   breaks = scales::trans_breaks("sqrt", function(x) x ^ 2),
  #   labels = function(x) round(x, 1)
  #   ) + # single hue color palette
  geom_sf(data = re_eez_prj, fill=NA,  color="grey50", size=.9) +        # EEZ borders
  geom_sf(data = re_world_prj, fill="grey40", color="grey40") +
  # geom_sf(data = re_world_wt, fill="grey55", color="grey25") +                # country polygons
  # geom_sf(data = re_world_wt, fill="grey85", color="grey85") +                # country polygons
  guides(
    fill = guide_colorbar(
      title="Bird years (millions)",
      title.position="top",
      barwidth  = 8,
      barheight = 1.5,
      ticks = T,
      ticks.linewidth = 2)
    ) +
  theme(
    plot.margin=unit(c(0,0,0,0),"cm"),
    # legend.position="bottom",
    legend.direction = "horizontal",
    # legend.position=c(0.01, 0),
    legend.position=c(0.80, 0),   # legend bottom right
    legend.justification = "left",
    legend.title=element_text(size=17),
    legend.text = element_text(size = 16)
  ) +
  coord_sf(datum = NA) 
# dev.new()
# m3

ggsave( "C:/Users/Martim Bill/Desktop/test/plotB11.png", plot = m3, width=30, height=20, units="cm", dpi=250)


## SAVE ##
if(assign == "A"){
  ggsave(paste0(master_figs, "maps/birdYEAR_global_kav7_pacific_infernoX.png"),
    width=30, height=20, units="cm", dpi=250)
} else if(assign=="B"){
  ggsave(paste0(master_figs, "figures/sovereign_B_assign/maps/birdYEAR_global_kav7_pacific_infernoX.png"),
    width=30, height=20, units="cm", dpi=250)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Animate monthly distribution #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("C:/Users/Martim Bill/Documents/R/source_scripts/recenter_map_fxn.r") # mapdata re-centering function

# recentering and projection objects
shift <- -153
central_meridian <- 360 + shift
proj <- sprintf("+proj=kav7 +lon_0=%i", central_meridian)
# proj <- sprintf("+proj=kav7 +lon_0=%i", shift)



# Countries ~~~~~~~~~~~~ 
x <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") # load country dataset
world <- rmapshaper::ms_simplify(x, keep = 0.99)

# EEZs ~~~~~~~~~~~~ 
x <- st_as_sf( raster::shapefile("data_test/geodata/World_EEZ_v10_20180221_HR_0_360/World_EEZ_boundaries_v10_2018_0_360.shp") ) # just EEZ data (latest version (2018))

eez_cnt <- rmapshaper::ms_simplify(x, keep = .01) # simplify dataset to make smaller

# create background polygon for globe ~~~~~~~~~
lats <- c(90:-90, -90:90, 90)
maxlong <- 360 + (shift + 180)
minlong <- shift + 180
longs <- c(rep(c(maxlong, minlong ), each = 181), maxlong )


# turn into correctly projected sf collection
outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% st_sf()

# re-center data sets ~~~~~~~~~~~~ 
re_eez <- recentre(eez_cnt, shift) %>% group_by(Line_ID) %>% summarize()    
re_world <- recentre(world,  clon = shift) %>% group_by(sovereignt) %>% summarize() # recenter and remove old seam

### project datasets
# re_world_wt <- st_transform_proj(re_world, crs = proj1, use_gdal = FALSE)
# re_world_wt   <- lwgeom::st_transform_proj(st_segmentize(st_buffer(re_world, -0.01), 15000), proj)
# re_eez_wt     <- st_transform_proj(re_eez, crs = proj, use_gdal = FALSE)
# re_world_wt <- lwgeom::st_transform_proj(st_segmentize(st_buffer(re_eez, -0.01), 15000), "+proj=kav7 +lon_0=-153")
# outline_prj   <- lwgeom::st_transform_proj(outline, crs = proj, use_gdal = FALSE)

re_world_prj   <- lwgeom::st_transform_proj(st_segmentize(st_buffer(re_world, -0.01), 15000), proj)
re_eez_prj     <- st_transform_proj(re_eez, crs = proj, use_gdal = FALSE)
outline_prj   <- lwgeom::st_transform_proj(outline, crs = proj, use_gdal = FALSE)




cellcnt_sum <- cntsum_allTD %>% group_by(month, cell) %>% summarise(
  timespent = sum(na.omit(tot_atatime)) #/ 12   # either x/12 for BIRDYEAR or just sum for BIRDMONTH
)

# thescale <- scales::pretty_breaks(n=3)(min(cellcnt_sum$richness):max(cellcnt_sum$richness))
thescale <- scales::trans_breaks("sqrt", function(x) x ^ 2)(min(cellcnt_sum$timespent):max(cellcnt_sum$timespent)) / 1000000


limits <- c(min(cellcnt_sum$timespent), 5*ceiling(max(cellcnt_sum$timespent)/5)) # plot limits, rounding up to nearest 5

plist <- list()

for(i in 1:12){
  print(i)
  onemonth <- dplyr::filter(cellcnt_sum, month == i)
  
  grid <- dgcellstogrid(spatial, cells = as.numeric(onemonth$cell), frame=TRUE, wrapcells=TRUE) # get only cells which contained fixes
  grid <- merge(grid, onemonth, by.x="cell")
  
  
  # Convert dggrid object (from 'hexbinning.r') to SF object (several steps) ~~~~~~~~~~~~
  # hexgrid to list of Polygons
  polylist <- lapply(split(grid, grid$cell), function(x) { 
    apoly <- Polygon(cbind(x$long, x$lat))
    apoly <- Polygons(list(apoly), ID = as.numeric(first(x$cell)))
  }
  )
  
  # get attribute data for each cell
  polydata <- grid %>% group_by(cell) %>% summarise(
    month     = first(month),
    timespent = first(timespent)
  ) %>%  arrange( cell ) %>% as.data.frame()
  
  rownames(polydata) <- polydata$cell
  
  # convert to SPolyDF (attaching data)
  grid.sp <- SpatialPolygonsDataFrame( SpatialPolygons(polylist, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), data = polydata, match.ID = T)
  
  ## sp to sf (and make valid)
  grid.sf <- st_as_sf(grid.sp)
  # all(st_is_valid(grid.sf)) # check validity 
  grid.sf <- st_make_valid(grid.sf)
  # all(st_is_valid(grid.sf)) # check validity 
  
  re_grid <- recentre(grid.sf, shift) %>% group_by(cell) %>% summarise(  # recenter grid! 
    timespent = first(timespent)
  )
  # re_grid_wt    <- st_transform_proj(re_grid, crs = proj, use_gdal = FALSE)
  re_grid_prj   <- st_transform_proj(re_grid, crs = proj, use_gdal = FALSE)
  
  p <- ggplot() +
    cowplot::theme_nothing() +
    geom_sf(data = outline_prj, color = NA, fill = "black") +
    geom_sf(data = re_grid_prj, aes(fill=timespent/1000000), color=NA) +
    scale_fill_viridis(
      option ="inferno",
      limits = c(0, 11),
      trans  ="sqrt",
      breaks = scales::trans_breaks("sqrt", function(x) x ^ 2, n=4),
      # breaks = thescale
      labels = function(x) round(x, 1)
    ) +
    geom_sf(data = re_eez_prj, fill="grey",  color="grey50") +        # EEZ borders
    geom_sf(data = re_world_prj, fill="grey30", color="grey30") +
    guides( 
      fill = guide_colorbar(
        title="Bird months (millions)",
        title.position="top",
        barwidth  = 8,
        barheight = 1.5)
    ) + 
    theme(
      plot.margin=unit(c(0,0,0,0),"cm"),
      # legend.position="bottom",
      legend.direction = "horizontal",
      legend.position=c(0.1, 0)
    ) +
    coord_sf(datum = NA) 
  p <- cowplot::plot_grid(p, label_size = 30, labels = month.abb[i])
  p
  
  plist[[i]] <- p

  # ggsave(sprintf(paste0(master_figs, "test/global_maps/animate/", i,"_timespent_%s.png"), month.abb[i]),
  #   plot = p, width=30, height=20, units="cm", dpi=250)
  
}

