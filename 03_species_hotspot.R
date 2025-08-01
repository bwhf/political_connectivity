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

## annual ranking
species_hotspot_summary <- species_by_hotspot_ol %>% group_by(region, scientific_name, category) %>% summarise(sum_overlap = sum(total_overlap)) %>% arrange(region, sum_overlap) %>% ungroup()


## creating density plot for each regions
# filter out species outside of the hotspots
# function to crop hotspot
crop_ir_hotspots <- function(dat) {
  crop_dat <- map_dfr(names(hotspots), function(region) {
    poly <- hotspots[[region]]
    
   dat %>%
     filter(st_intersects(geometry, poly, sparse = FALSE)
      )
  })
  return(crop_dat)
}

# run the function
species_hotspot <- crop_ir_hotspots(recent_sp_grid_ocean)

# prepare front and fh dat 
all_front <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_cls_front_all_coord.rds")

grid_all_fh_dat <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_ts_fh_cls_ocean_geom.rds")

grid_all_fh_dat <- grid_all_fh %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")))

# assign geometry to all fronts
# create hex grid
spatial <- dgconstruct(spacing=100, metric=TRUE, resround='nearest') # spacing unit  CLS (km)
wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing

# create cell geometry
grid_all_fh <- dgcellstogrid(spatial, cells = as.numeric(grid_all_fh$cell)) # get only cells which contained fixes
grid_all_fh <- grid_all_fh %>% 
  rename("cell" = "seqnum")

grid_all_fh_geo <- merge(grid_all_fh_dat, grid_all_fh, by.x="cell")

write_rds(grid_all_fh_geo, "/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_ts_fh_cls_ocean_geom.rds")

# crop area for hotspots
front_hotspot <- crop_ir_hotspots(all_front)
fh_hotspot <- crop_ir_hotspots(grid_all_fh_geo)

## create density plot 
# ts and fh

## front freq
pd_ff_fh_ts_hotspots <- plot_tsfh_density(species_hotspot, fh_hotspot, front_freq, "Front Freq")

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/hotspots_density_ff_fh_ts_nobg_ocean.png")
ggsave(filename = outputfile, plot = pd_ff_fh_ts_hotspots, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## front strength
pd_sf_fh_ts_hotspots <- plot_tsfh_density(species_hotspot, fh_hotspot, msfront, "Front Strength")

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/hotspots_density_sf_fh_ts_nobg_ocean.png")
ggsave(filename = outputfile, plot = pd_sf_fh_ts_hotspots, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## front persistence
pd_pf_fh_ts_hotspots <- plot_tsfh_density(species_hotspot, fh_hotspot, pfront, "Front Persistence")

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/hotspots_density_pf_fh_ts_nobg_ocean.png")
ggsave(filename = outputfile, plot = pd_pf_fh_ts_hotspots, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

### overlap vs no overlap
# front freq
pd_hotspot_ovff_density2 <- plot_dual_density_2(species_hotspot, front_hotspot, front_freq, "front frequency")

outputfile <- paste0("data/analysis/figures_dawn/density_ov_ff_cls_hotspot.png")
ggsave(filename = outputfile, plot = pd_hotspot_ovff_density2, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front strength
pd_hotspot_ovsf_density2 <- plot_dual_density_2(species_hotspot, front_hotspot, msfront, "front strength")

outputfile <- paste0("data/analysis/figures_dawn/density_ov_sf_cls_hotspot.png")
ggsave(filename = outputfile, plot = pd_hotspot_ovsf_density2, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front persistence
pd_hotspot_ovpf_density2 <- plot_dual_density_2(species_hotspot, front_hotspot, pfront, "front persistence")

outputfile <- paste0("data/analysis/figures_dawn/density_ov_pf_cls_hotspot.png")
ggsave(filename = outputfile, plot = pd_hotspot_ovpf_density2, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")
