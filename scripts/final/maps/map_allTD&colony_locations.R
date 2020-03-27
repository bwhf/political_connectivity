#===================================================================================
## Make a map of sampled colony/island groups ## 
#===================================================================================

cols <- as.data.frame(data.table::fread( "data/colony_locations.csv"))

pacman::p_load(dplyr, mapview, sf, ggplot2, lwgeom, stringr)

cols$lon_colony <- as.numeric(cols$lon_colony)

cols <- cols[!is.na(cols$lat_colony) == T, ]

cols_sf <- st_as_sf(cols, coords = c("lon_colony", "lat_colony"), crs = 4326)

#==================================================================================
## Quick look at the data ## 
# mapview(cols_sf)

#==================================================================================
## Make a nice map! ## 

source("C:/Users/Martim Bill/Documents/R/source_scripts/recenter_map_fxn.r") # mapdata re-centering function

# shift <- 0     # "normal"
shift <- -153  # S. Africa
# shift <- 114 # Cape Horn ( would need to generalize some of the code below )
shift <- -346  # Get all NZ islands on one side, split Hawaiian Archipelago

## Shift base data  ## 
x <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") # load country dataset
world <- rmapshaper::ms_simplify(x, keep = 0.99)

re_world <- recentre(world, shift) %>% group_by(sovereignt) %>% summarize() # recenter and remove old seam

## Shift colony location data  ## 
re_cols <- recentre(cols_sf, shift)


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


# plot(st_geometry(re_world))
# plot(st_geometry(re_oceans))
# plot(st_geometry(re_cols), col=2, add=T)

## Final map ## 


#### Project data for mapping ####

central_meridian <- 360 + shift
# central_meridian <- 360 - 246 # Cape Horn on edge
# central_meridian <- 0

# proj <-  sprintf("+proj=wintri +datum=WGS84 +lon_0=%i +no_defs +over", central_meridian)  # Winkel Tripel
proj <-  sprintf("+proj=robin +datum=WGS84 +lon_0=%i +no_defs +over", central_meridian)     # Robinson
proj <- sprintf("+proj=kav7 +datum=WGS84 +lon_0=%i +no_defs +over", central_meridian)       # Kavrayskiy VII 


re_world_prj  <- lwgeom::st_transform_proj(re_world, crs = proj, use_gdal = FALSE)
re_cols_prj   <- lwgeom::st_transform_proj(re_cols, crs = proj, use_gdal = FALSE)
outline_prj   <- lwgeom::st_transform_proj(outline, crs = proj, use_gdal = FALSE)
# re_oceans_prj <- lwgeom::st_transform_proj(re_oceans, crs = proj, use_gdal = FALSE)

## graticule ## 
# gr <- sf::st_graticule(lat = seq(-80,80,40), lon = seq((180+shift+28),(360+shift+180+28),by=80))
# gr <- lwgeom::st_transform_proj(gr, crs = proj)


## just colony locations ## 
p <- ggplot() +
  cowplot::theme_nothing() +
  geom_sf(data = outline_prj, fill = "#56B4E950", size = 0.5/.pt) +
  geom_sf(data = re_world_prj, fill="grey15", color=alpha("white", 0.2)) +
  geom_sf(data = re_cols_prj, color = "red", size=3.5, pch=21, fill=alpha("white", 0.3), stroke = 1) +
  coord_sf(datum = NA) 

# 
p 

## Save ##
# ggsave("figures/test/global_maps/colony_locations_global_winkeltripel_pacificX.png",
#   width=30, height=20, units="cm", dpi=250) 
# ggsave("figures/test/global_maps/colony_locations_global_robinson_pacific.png",
#   width=30, height=20, units="cm", dpi=250) 
ggsave("figures/test/global_maps/colony_locations_global_kav7_pacific.png",
  width=30, height=20, units="cm", dpi=250) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## trying to add track lines/points to mapp

inFolder <- 'data_test/oveez/'
files <- list.files(inFolder)
files


allTD_list <- lapply(files, function(x) readRDS(paste0(inFolder, x)) )

allTD <- do.call("rbind", allTD_list)

byGroup <- T

if(byGroup == T){
  pattern <- paste(c("Thalassarche", "Phoebastria", "Phoebetria", "Diomedea"), collapse = "|")
  
  allTD$tax_group <- ifelse(str_detect(allTD$scientific_name, pattern = pattern), "Albatross",
    ifelse(str_detect(allTD$scientific_name, pattern = "Ardenna"), "Ardenna", 
      ifelse(str_detect(allTD$scientific_name, pattern = "Calonectris"), "Calonectris",
        ifelse(str_detect(allTD$scientific_name, pattern = "Procellaria"), "Procellaria", 
          ifelse(str_detect(allTD$scientific_name, pattern = "Macronectes"), "Macronectes", "POOP")
        )
      )
    )
  )
  allTD <- allTD %>% dplyr::select("longitude", "latitude", "tax_group", "date_time", "track_id", "scientific_name")
  TD <- allTD
  TD_sf <- st_as_sf(TD, coords = c("longitude", "latitude"), 
    crs = 4326)
  
  re_TD <- recentre(TD_sf, shift) # recenter tracking data
  
  re_TD_prj  <- lwgeom::st_transform_proj(re_TD, crs = proj, use_gdal = FALSE)
  
  # color palettes: https://personal.sron.nl/~pault/#sec:qualitative
  cols <- c("Albatross" = "#332288", "Ardenna" = "#117733", "Calonectris" = "#DDCC77", "Procellaria" = "#EE7733", "Macronectes" = "#AA4499")
  
  p <- ggplot() +
    geom_sf(data = outline_prj, fill = "#56B4E950", size = 0.5/.pt) +
    # geom_sf(data = re_TD_prj, color=alpha("orange", 0.5)) +
    geom_sf(data = re_TD_prj, mapping=aes(color = tax_group), alpha = 0.03, show.legend = "point") +
    geom_sf(data = re_world_prj, fill="grey15", color=alpha("white", 0.2)) +
    scale_color_manual(values=cols) +
    guides(colour = guide_legend(override.aes = list(alpha=1, size=5))) +
    geom_sf(data = re_cols_prj, color = "red", size=3.5, pch=21, fill=alpha("white", 0.1), stroke = 1) +
    theme(panel.background = element_rect(fill = "white")) +
    labs(color = "Group") +
    coord_sf(datum = NA) 
  
  ggsave("figures/test/global_maps/colony_locations_allTD_kav7_atlantic15_groupcolorsX.png",
    width=30, height=20, units="cm", dpi=250) 
  
} else {
 
  allTD <- allTD %>% dplyr::select("longitude", "latitude", "date_time", "track_id", "scientific_name")
  TD <- allTD
  # TD <- dplyr::filter(TD, scientific_name == "Calonectris leucomelas" )
  # TD <- dplyr::filter(allTD, scientific_name == "Ardenna carneipes" | scientific_name == "Calonectris leucomelas" | scientific_name == "Macronectes giganteus" | scientific_name == "Phoebastria albatrus" | scientific_name == "Procellaria westlandica")
  # TD <- TD[c(1:5000,  160000:161000, 301000:302000, 610432:620432), ] # make smaller for testing
  # TD <- dplyr::filter(allTD, scientific_name == "Ardenna grisea") # filter to one species
  
  TD_sf <- st_as_sf(TD, coords = c("longitude", "latitude"), 
    crs = 4326)
  
  re_TD <- recentre(TD_sf, shift) # recenter tracking data
  
  re_TD_prj  <- lwgeom::st_transform_proj(re_TD, crs = proj, use_gdal = FALSE)
  
  p <- ggplot() +
    geom_sf(data = outline_prj, fill = "#56B4E950", size = 0.5/.pt) +
    # geom_sf(data = re_TD_prj, color=alpha("orange", 0.5)) +
    geom_sf(data = re_TD_prj, mapping=aes(), color = "#332288", alpha = 0.03, show.legend = "point") +
    geom_sf(data = re_world_prj, fill="grey15", color=alpha("white", 0.2)) +
    guides(colour = guide_legend(override.aes = list(alpha=1, size=5))) +
    geom_sf(data = re_cols_prj, color = "red", size=3.5, pch=21, fill=alpha("white", 0.1), stroke = 1) +
    theme(panel.background = element_rect(fill = "white")) +
    coord_sf(datum = NA) 
  
  ggsave("figures/test/global_maps/colony_locations_allTD_kav7_atlantic15_onecolor.png",
    width=30, height=20, units="cm", dpi=250) 
  
}

allTD <- allTD %>% dplyr::select("longitude", "latitude", "date_time", "track_id", "scientific_name")
TD <- allTD
TD <- dplyr::filter(TD, scientific_name == "Calonectris leucomelas" )
# TD <- dplyr::filter(allTD, scientific_name == "Ardenna carneipes" | scientific_name == "Calonectris leucomelas" | scientific_name == "Macronectes giganteus" | scientific_name == "Phoebastria albatrus" | scientific_name == "Procellaria westlandica")
# TD <- TD[c(1:5000,  160000:161000, 301000:302000, 610432:620432), ] # make smaller for testing
# TD <- dplyr::filter(allTD, scientific_name == "Ardenna grisea") # filter to one species

TD_sf <- st_as_sf(TD, coords = c("longitude", "latitude"), 
  crs = 4326)

re_TD <- recentre(TD_sf, shift) # recenter tracking data

re_TD_prj  <- lwgeom::st_transform_proj(re_TD, crs = proj, use_gdal = FALSE)




## richness (visiting or absolute, depending on filtering)

# color palettes: https://personal.sron.nl/~pault/#sec:qualitative
cols <- c("Albatross" = "#332288", "Ardenna" = "#117733", "Calonectris" = "#DDCC77", "Procellaria" = "#EE7733", "Macronectes" = "#AA4499")

p <- ggplot() +
  geom_sf(data = outline_prj, fill = "#56B4E950", size = 0.5/.pt) +
  # geom_sf(data = re_TD_prj, color=alpha("orange", 0.5)) +
  geom_sf(data = re_TD_prj, mapping=aes(color = tax_group), alpha = 0.03, show.legend = "point") +
  geom_sf(data = re_world_prj, fill="grey15", color=alpha("white", 0.2)) +
  scale_color_manual(values=cols) +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=5))) +
  geom_sf(data = re_cols_prj, color = "red", size=3.5, pch=21, fill=alpha("white", 0.1), stroke = 1) +
  theme(panel.background = element_rect(fill = "white")) +
  labs(color = "Group") +
  coord_sf(datum = NA) 


# 
# dev.new()
# p

## SAVE ## 
ggsave("figures/test/global_maps/colony_locations_allTD_kav7_pacific.png",
  width=30, height=20, units="cm", dpi=250) 




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Greenwich-centered version

# proj <-  sprintf("+proj=wintri +datum=WGS84 +lon_0=%i +no_defs +over", central_meridian)  # Winkel Tripel
proj <- "+proj=robin +datum=WGS84 +no_defs +over"    # Robinson
proj <- "+proj=kav7 +datum=WGS84 +no_defs +over"       # Kavrayskiy VII 

# vectors of latitudes and longitudes that go once around the 
# globe in 1-degree steps
lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

# turn into correctly projected sf collection
outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>% st_transform_proj(crs = proj, use_gdal = FALSE) # transform to Winkel tripel


world_prj  <- lwgeom::st_transform_proj(world, crs = proj, use_gdal = FALSE)
cols_prj   <- lwgeom::st_transform_proj(cols_sf, crs = proj, use_gdal = FALSE)


## just colony locations ## 

p <- ggplot() +
  # geom_sf(data = gr, color = 'blue') +  # DOESN"T WORK! 
  geom_sf(data = outline, fill = "#56B4E950", size = 0.5/.pt) +
  geom_sf(data = world_prj, fill="grey15", color=alpha("white", 0.2)) +
  geom_sf(data = cols_prj, color = "red", size=3.5, pch=21, fill=alpha("white", 0.3), stroke = 1) +
  theme(panel.background = element_rect(fill = "white")) +
  coord_sf(datum = NA)

# 
dev.new()
p 


# project Tracking Data 
TD_prj  <- lwgeom::st_transform_proj(TD_sf, crs = proj, use_gdal = FALSE)



## All tracking data w/ colony locations on top ##

# color palettes: https://personal.sron.nl/~pault/#sec:qualitative
cols <- c("Albatross" = "#332288", "Ardenna" = "#117733", "Calonectris" = "#DDCC77", "Procellaria" = "#EE7733", "Macronectes" = "#AA4499")

p <- ggplot() +
  geom_sf(data = outline, fill = "#56B4E950", size = 0.5/.pt) +
  # geom_sf(data = re_TD_prj, color=alpha("orange", 0.5)) +
  geom_sf(data = TD_prj, mapping=aes(color = tax_group), alpha = 0.03, show.legend = "point") +
  geom_sf(data = world_prj, fill="grey15", color=alpha("white", 0.2)) +
  scale_color_manual(values=cols) +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=5))) +
  geom_sf(data = cols_prj, color = "red", size=3.5, pch=21, fill=alpha("white", 0.1), stroke = 1) +
  theme(panel.background = element_rect(fill = "white")) +
  labs(color = "Group") +
  coord_sf(datum = NA) 

# dev.new()
# p 

## SAVE ## 
ggsave("figures/test/global_maps/colony_locations_allTD_kav7_greenwich.png",
  width=30, height=20, units="cm", dpi=250) 
