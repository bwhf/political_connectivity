#===================================================================================
## Make a global map of sampled colony/island groups and tracking data ## 
# Centered on prime meridian
#===================================================================================
pacman::p_load(dplyr, sf, ggplot2, lwgeom)

## Load colony locations data ## --------------------------------------------
cols <- as.data.frame(data.table::fread( "data/colony_locations.csv"))

cols$lon_colony <- as.numeric(cols$lon_colony)
cols <- cols[!is.na(cols$lat_colony) == T, ]
cols_sf <- st_as_sf(cols, coords = c("lon_colony", "lat_colony"), crs = 4326)

## Load EEZ-country union dataset ## ------------------------------------------
x <- st_as_sf( raster::shapefile("spatial_data/shapefiles_EEZ_countries/union_countries_EEZs/EEZ_Land_v3_202030.shp") ) # EEZ-land union data (latest version (2019))
eez_cnt <- rmapshaper::ms_simplify(x, keep = .001) # simplify dataset to make smaller

## Load Country (land) polygons ##  --------------------------------------------
x <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") # load country dataset
world <- rmapshaper::ms_simplify(x, keep = 0.99)

## load tracking data ## --------------------------------------------
inFolder <- 'data/analysis/oveez/'
files <- list.files(inFolder)

allTD_list <- lapply(files, function(x) readRDS(paste0(inFolder, x)) )

TD_sf <- do.call("rbind", allTD_list) %>% st_as_sf( 
  coords = c("longitude", "latitude"), 
  crs = 4326)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Greenwich-centered version

# choose projection type
proj <- "+proj=wintri"  # Winkel Tripel
proj <- "+proj=robin"    # Robinson
proj <- "+proj=kav7"       # Kavrayskiy VII

## create background ocean polygon to ensure the edge of globe is smooth
# vectors of latitudes and longitudes that go once around the globe in 1-degree steps
lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

# turn into correctly projected sf collection
outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>% st_transform_proj(crs = proj) # project


## project each dataset ## 
world_prj  <- lwgeom::st_transform_proj(world, crs = proj)
cols_prj   <- lwgeom::st_transform_proj(cols_sf, crs = proj)
eez_prj    <- lwgeom::st_transform_proj(eez_cnt, crs = proj)
TD_prj     <- lwgeom::st_transform_proj(TD_sf, crs = proj)


## Plot ## 

p <- ggplot() +
  geom_sf(data = outline, fill = "#56B4E950", size = 0.5/.pt) +
  geom_sf(data = TD_prj, mapping=aes(), color = "#332288", alpha = 0.03, show.legend = "point") +
  geom_sf(data = world_prj, fill="grey15", color=alpha("white", 0.2)) +
  geom_sf(data = cols_prj, color = "red", size=3.5, pch=21, fill=alpha("white", 0.3), stroke = 1) +
  theme(panel.background = element_rect(fill = "white")) +
  coord_sf(datum = NA)
p
