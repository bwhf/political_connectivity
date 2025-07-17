## Identifying species in hotspots

recent_sp_grid_ocean <- read_rds("data/analysis/glob_hexgrid_dawn/recent_sp_ov_cls_grid_ocean.rds")

library(sf)
library(dplyr)
library(readr)
library(fuzzyjoin)

# load species threat status
iucn <- read_csv("/Users/bwhf/Documents/GitHub/political_connectivity/data/species_status/threatened-species.csv")

# filter for seabirds
species_list <- tibble(scientific_name = unique(recent_sp_grid_ocean$scientific_name)) # get the species list in our data
seabird_iucn <- stringdist_left_join(
  species_list, 
  iucn, 
  by = c("scientific_name" = "scientific_name"),
  max_dist = 1,
  distance_col = "dist"
)

seabird_iucn_cat <- seabird_iucn %>% select(scientific_name.x, category) %>% rename("scientific_name" = scientific_name.x)

# combine column to dataset
recent_sp_grid_ocean <- fuzzy_left_join(
  recent_sp_grid_ocean,
  seabird_iucn_cat, 
  by = c("scientific_name" = "scientific_name"),
  match_fun = `==`
)

recent_sp_grid_ocean <- recent_sp_grid_ocean %>% rename("scientific_name" = scientific_name.x) %>% select(-scientific_name.y)


## define polygons of the hotspots

#1 North Pacific
north_pacific <- st_as_sfc(st_bbox(c(
  xmin = 120, xmax = 240,  # 120°E to 120°W
  ymin = 15,  ymax = 55
), crs = 4326))

#2 Agulhas Current & South Indian Ocean (South of Madagascar to South Indian gyre)
agulhas_indian <- st_as_sfc(st_bbox(c(
  xmin = 10, xmax = 100,
  ymin = -55, ymax = -20
), crs = 4326))

#3 Tasman Sea (East of Australia between AUS & NZ)
tasman_sea <- st_as_sfc(st_bbox(c(
  xmin = 140, xmax = 180,
  ymin = -55, ymax = -25
), crs = 4326))

#4 Patagonian Shelf Break (East coast of southern South America)
patagonian_shelf <- st_as_sfc(st_bbox(c(
  xmin = -75, xmax = -45,
  ymin = -55, ymax = -35
), crs = 4326))

# combine hotspots as list
hotspots <- list(
  "North Pacific" = north_pacific,
  "Agulhas & Indian Ocean" = agulhas_indian,
  "Tasman Sea" = tasman_sea,
  "Patagonian Shelf" = patagonian_shelf
)

library(purrr)

# basic list of species in the regions
species_by_hotspot <- map_dfr(names(hotspots), function(region) {
  hotspot_poly <- hotspots[[region]]
  
  recent_sp_grid_ocean %>%
    filter(st_intersects(geometry, hotspot_poly, sparse = FALSE)) %>%
    distinct(scientific_name) %>%
    mutate(region = region)
})

# include overlap and seasons
library(dplyr)
library(purrr)
library(sf)

species_by_hotspot_ol <- map_dfr(names(hotspots), function(region) {
  poly <- hotspots[[region]]
  
  recent_sp_grid_ocean %>%
    filter(
      st_intersects(geometry, poly, sparse = FALSE)
    ) %>%
    group_by(region = region, season, scientific_name, category) %>%
    summarise(
      total_overlap = sum(overlap, na.rm = TRUE),
      mean_overlap = mean(overlap, na.rm = TRUE),
      .groups = "drop"
    )
})

