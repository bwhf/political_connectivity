# Boxplots of overlap vs fronts by ocean regions
all_front <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_cls_front_all.rds")

grid_all_ts <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_ts_fh_cls.rds")

grid_all_fh <- read_rds("data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_fh_ts_cls.rds")

# match ocean for fh
ocean_front <- all_front %>% group_by(cell, ocean_name, ocean) %>% summarise()

# correct Southerns and Artic Ocean to Southern Ocean
ocean_front <- ocean_front %>% 
  mutate(
    ocean = case_when(
      ocean %in% "Southern & Arctic Ocean" ~ "Southern Ocean",
      TRUE ~ ocean
    ))

grid_all_fh <- grid_all_fh %>%
  left_join(ocean_front, by = "cell")  # Matches by cell number

write_rds(grid_all_fh, "/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_ts_fh_cls_ocean.rds")

# assign geometry to all fronts
# create hex grid
spatial <- dgconstruct(spacing=100, metric=TRUE, resround='nearest') # spacing unit  CLS (km)
wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing

# create cell geometry
grid_all_front <- dgcellstogrid(spatial, cells = as.numeric(all_front$cell)) # get only cells which contained fixes
grid_all_front <- grid_all_front %>% 
  rename("cell" = "seqnum")

all_front1 <- all_front %>%
  mutate(
    ocean = case_when(
      ocean %in% "Southern & Arctic Ocean" ~ "Southern Ocean",
      TRUE ~ ocean
    ))

grid_all_front <- merge(all_front1, grid_all_front, by.x="cell")

write_rds(grid_all_front, "/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_cls_front_all_coord.rds")
