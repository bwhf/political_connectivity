#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create maps of tracking data binned into hex.grid, and re-centering geodata to pacific perspective #
# Richness and fixcount #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-------------------------------------------------------------------------------------------------------

pacman::p_load(dggridR, sf, tidyverse, viridis, cowplot, lwgeom, colorspace)

source("scripts/final/source_fxns/recenter_map_fxn.r") # mapdata re-centering function

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### Variations ----------------------------------------------------######

## Choose whether to use high threshold or low threshold data (i.e. >1 bird per month) ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# thresh <- "high"
thresh  <- "low"

if(thresh == "high"){
  master <- "data/analysis/bird_thresh/"
  master_figs <- "figures/bird_thresh/"
} else {
  master <- "data/analysis/"
  master_figs <- "figures/"
}

#### UK or Argentina assignment of Falklands/S.Georgia breeding birds ? ####
assign <- "UK"
# assign <- "ARG"

#### visiting or true richness grid? ####

visit_rich = FALSE


if(assign=="UK"){
  if(visit_rich == TRUE){
    grid <- readRDS(paste0(master, "glob_hexgrid/global_hexgrid_452km_vrich.rds"))  # load grid excluding points w/in origin countries (visit rich.)
  } else {
    grid <- readRDS(paste0(master, "glob_hexgrid/global_hexgrid_452km_trich.rds"))  # load grid with all points (true richness/fixcount)
  }
} else if(assign=="ARG"){
  if(visit_rich == TRUE){
    grid <- readRDS(paste0(master, "ARG_assign/glob_hexgrid/global_hexgrid_452km_vrich.rds"))  # load grid excluding points w/in origin countries (visit rich.)
  } else {
    grid <- readRDS(paste0(master,"ARG_assign/glob_hexgrid/global_hexgrid_452km_trich.rds"))  # load grid with all points (true richness/fixcount)
  }
}


#### Re-center hexgrid ####
shift <- -153 # Degrees from prime meridian to shift map
central_meridian <- 360 + shift
proj <- sprintf("+proj=kav7 +lon_0=%i", central_meridian)  # set projection (w/ new meridian) (Kavraisky VII https://en.wikipedia.org/wiki/Kavrayskiy_VII_projection)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get hexgrid in into sf (via sp)

## Convert dgrid object (from 'hexbinning.r') to SpatialPolygons object (several steps)
# hexgrid to list of Polygons
polylist <- lapply(split(grid, grid$cell), function(x) { 
  apoly <- Polygon(cbind(x$long, x$lat))
  apoly <- Polygons(list(apoly), ID = as.numeric(first(x$cell)))
}
)

# get attribute data for each cell
polydata <- grid %>% group_by(cell) %>% summarise(
  fixcount = first(fixcount),
  richness = first(richness)
) %>%  arrange( cell ) %>% as.data.frame()

rownames(polydata) <- polydata$cell

# convert to SPolyDF (attaching data)
grid.sp <- SpatialPolygonsDataFrame( SpatialPolygons(polylist, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), data = polydata, match.ID = T)

#~~~~~~~~~~~~
## sp to sf (and make valid)

grid.sf <- st_as_sf(grid.sp)
# all(st_is_valid(grid.sf)) # check validity 
grid.sf <- st_make_valid(grid.sf)
# all(st_is_valid(grid.sf)) # check validity 

re_grid <- recentre(grid.sf, shift) %>% group_by(cell) %>% summarise(  # recenter grid! 
  richness = first(richness),
  fixcount = first(fixcount)
)

# ggplot() + geom_sf(data=re_grid) # check


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Re-center world polygon data ####

x <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") # load country dataset
world <- rmapshaper::ms_simplify(x, keep = 0.99)

re_world <- recentre(world, shift) %>% group_by(sovereignt) %>% summarize() # recenter and remove old seam
# re_world <- recentre(world, 207) #%>% group_by(sovereignt) %>% summarize() # recenter and remove old seam


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Re-center EEZ boundaries data ####

# eez_cnt <- st_as_sf( raster::shapefile("data_test/shapefiles_EEZ_countries/union_countries_EEZs/EEZ_land_v2_201410.shp" ) )  # EEZ-land union data doesn't work
x <- st_as_sf( raster::shapefile("data_test/geodata/World_EEZ_v10_20180221_HR_0_360/World_EEZ_boundaries_v10_2018_0_360.shp") ) # just EEZ data (latest version (2018))

eez_cnt <- rmapshaper::ms_simplify(x, keep = .01) # simplify dataset to make smaller
all(st_is_valid(eez_cnt))

re_eez <- recentre(eez_cnt, shift) %>% group_by(Line_ID) %>% summarize()          # recenter and remove old seam


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create background 'oceans' polygon (Solution here: # https://wilkelab.org/practicalgg/articles/Winkel_tripel.html)

# vectors of latitudes and longitudes that go once around the globe in 1-degree steps
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Project data for mapping ####

re_world_wt <- st_transform_proj(re_world, crs = proj, use_gdal = FALSE)
re_world_wt   <- lwgeom::st_transform_proj(st_segmentize(st_buffer(re_world, -0.01), 15000), proj)

re_eez_wt <- st_transform_proj(re_eez, crs = proj, use_gdal = FALSE)
re_grid_wt <- st_transform_proj(re_grid, crs = proj, use_gdal = FALSE)
# re_oceans_prj <- st_transform_proj(re_oceans, crs = proj, use_gdal = FALSE)
outline_prj   <- lwgeom::st_transform_proj(outline, crs = proj, use_gdal = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Species richness (visiting or absolute, depending on filtering)
p <- ggplot() + theme_nothing() + # gets rid of gray box behind map
  geom_sf(data = outline_prj, color = NA, fill = "black") +                # background ocean polygon
  # geom_sf(data = outline_prj, cocor = NA, fill = "white") +
  geom_sf(data = re_grid_wt, aes(fill=richness), color=NA) +
  # scale_fill_viridis(option="plasma", breaks=c(1, 5, 10, 15, signif(max(re_grid_wt$richness),1))) +
  # scale_fill_viridis(option="cividis", breaks=c(1, 5, 10, 15, signif(max(re_grid_wt$richness),1)), direction = -1) +
  # scale_fill_carto_c(type = "quantitative", palette = "DarkMint", breaks=c(1, 5, 10, 15, signif(max(re_grid_wt$richness),1)), direction = -1) +
  # scale_fill_continuous_sequential(palette = "Blues", breaks=c(1, 5, 10, 15, signif(max(re_grid_wt$richness),1))) + # single hue color palette
  scale_fill_continuous_sequential(palette = "Lajolla", breaks=c(1, 5, 10, 15, signif(max(re_grid_wt$richness),1))) +
  geom_sf(data = re_eez_wt, fill="grey",  color="grey50") +        # EEZ borders
  # geom_sf(data = re_eez_wt, fill="grey",  color="grey30") +        # EEZ borders
  # geom_sf(data = re_eez_wt, fill="grey",  color="black") +
  geom_sf(data = re_world_wt, fill="grey30", color="grey30") +
  # geom_sf(data = re_world_wt, fill="grey55", color="grey25") +                # country polygons
  # geom_sf(data = re_world_wt, fill="grey85", color="grey85") +                # country polygons
  # geom_sf(data = re_world_wt, fill="grey85", color=NA) +                # country polygons
  guides( 
    fill = guide_colorbar(
      title="Richness",
      title.position="top",
      barwidth  = 8,
      barheight = 1.5,
      ticks.linewidth = 2)
  ) +
  theme(
    plot.margin=unit(c(0,0,0,0),"cm"),
    # legend.position="bottom",
    legend.direction = "horizontal",
    legend.position=c(0.01, 0),
    legend.justification = "left",
    legend.title=element_text(size=18),
    legend.text = element_text(size = 16)
  ) +
  coord_sf(datum = NA)
# 
# dev.new()
# p

ggsave( "C:/Users/Martim Bill/Desktop/test/plot30.png", plot = p, width=30, height=20, units="cm", dpi=250)

## Decide where to save ##
if(assign=="UK"){
  if(visit_rich == TRUE){
    ggsave(paste0(master_figs, "maps/visit_richness.dense_global_kav7_pacific_Lajolla.png"),
      width=30, height=20, units="cm", dpi=250)
    
  } else {
    ggsave(paste0(master_figs, "maps/true_richness.dense_global_kav7_pacific_Lajolla.png"),
      width=30, height=20, units="cm", dpi=250)
  }
} else if(assign=="ARG"){
  if(visit_rich == TRUE){
    ggsave(paste0(master_figs, "ARG_assign/maps/visit_richness.dense_global_kav7_pacific_Lajolla.png"),
      width=30, height=20, units="cm", dpi=250)
    
  } else {
    ggsave(paste0(master_figs, "ARG_assign/maps/true_richness.dense_global_kav7_pacific_Lajolla.png"),
      width=30, height=20, units="cm", dpi=250) 
  }
}

# ggsave("figures/test/global_maps/visit_richness.dense_global_robinson_pacific_civ.png",
# width=30, height=20, units="cm", dpi=250) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Fixcount and colony locations #### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cols <- as.data.frame(data.table::fread( "data/colony_locations.csv"))

cols$lon_colony <- as.numeric(cols$lon_colony)
cols <- cols[!is.na(cols$lat_colony) == T, ]

cols_sf <- st_as_sf(cols, coords = c("lon_colony", "lat_colony"), crs = 4326)


## Shift colony location data  ## 
re_cols <- recentre(cols_sf, shift)
re_cols_prj   <- lwgeom::st_transform_proj(re_cols, crs = proj, use_gdal = FALSE)

p <- ggplot()  +
  theme_nothing() +
  geom_sf(data = outline_prj, color = NA, fill = "black") +
  geom_sf(data = re_grid_wt, aes(fill= fixcount ), color=NA) +
  # scale_fill_viridis(option="cividis", trans="log10", breaks=scales::trans_breaks("log10", function(x) 10^x)) +
  scale_fill_continuous_sequential(rev=T, palette = "Oslo", trans="log10", breaks=scales::trans_breaks("log10", function(x) 10^x)
    #,trans  ="sqrt", breaks = scales::trans_breaks("sqrt", function(x) x ^ 2)
    ) +
  geom_sf(data = re_cols_prj, color = "red", size=3.5, pch=21, fill=alpha("white", 0.3), stroke = 1) +
  # geom_sf(data = re_eez_wt, fill="grey", color="white") +
  geom_sf(data = re_world_wt, fill="grey30", color="grey30") +
  guides( fill = guide_colorbar(title="Fixes")  ) +
  guides( 
    fill = guide_colorbar(
      title="Fix count",
      title.position="top",
      barwidth  = 8,
      barheight = 1.5,
      ticks.linewidth = 2)
  ) + 
  theme(
    plot.margin=unit(c(0,0,0,0),"cm"),
    # legend.position="bottom",
    legend.direction = "horizontal",
    legend.position=c(0.01, 0),
    legend.justification = "left",
    legend.title=element_text(size=18),
    legend.text = element_text(size = 15)
  ) +
  coord_sf(datum = NA) 

# 
# p

ggsave( "C:/Users/Martim Bill/Desktop/test/plotC5.png", plot = p, width=30, height=20, units="cm", dpi=250)

if(assign=="UK"){
  if(visit_rich == TRUE){
    ggsave(paste0(master_figs, "maps/visit_log_fixcount.dense_global_kav7_pacific_coloniesX.png"),
      width=30, height=20, units="cm", dpi=250)
  } else {
    ggsave(paste0(master_figs, "maps/true_log_fixcount.dense_global_kav7_pacific_coloniesX_.png"),
      width=30, height=20, units="cm", dpi=250) 
  }
} else if(assign=="ARG"){
  if(visit_rich == TRUE){
    ggsave(paste0(master_figs, "ARG_assign/maps/visit_log_fixcount.dense_global_kav7_pacific_coloniesX.png"),
      width=30, height=20, units="cm", dpi=250)
  } else {
    ggsave(paste0(master_figs, "ARG_assign/maps/true_log_fixcount.dense_global_kav7_pacific_coloniesX_.png"),
      width=30, height=20, units="cm", dpi=250)
  }
}

# ggsave("figures/test/global_maps/fixcount.dense_global_robinson_pacific_civ.png",
# width=30, height=20, units="cm", dpi=250) 


