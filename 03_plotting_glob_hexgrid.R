# plot it out
# recentering and projection objects

source("/Users/bwhf/Documents/GitHub/political_connectivity/scripts/final/source_fxns/recenter_map_fxn.R") # mapdata re-centering function
source("/Users/bwhf/Documents/GitHub/political_connectivity/00_function_extracting_cross_dateline_grid.R") # finding hex grids that cross dateline and extract from dataframe for plotting

# load world map
library(maps)
library(mapproj)
world <- spData::world
myworld = maps::map(wrap=c(0,360), plot=FALSE, fill=TRUE)
# Convert the map data to a dataframe for ggplot2 plotting
map_df <- map_data(myworld)

# Read eez.shp just the boundaries
eez_boundaries <- sf::st_read("/Users/bwhf/Documents/GitHub/political_connectivity/data/Marine_regions/World_EEZ_v11_20191118_HR_0_360", layer="eez_boundaries_v11_0_360")

# shifting cell to central meridian
shift <- -153
central_meridian <- 360 + shift
proj <- sprintf("+proj=kav7 +lon_0=%i", central_meridian)

# read data
grid_fh_cly <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_cly.rds")

fh_grid <- grid_fh_cly %>%
  filter(avg_fh > 0)

if (!all(c("cell", "avg_fh", "geometry") %in% colnames(fh_grid))) {
  stop("Grid object does not contain the expected columns.")
}

# converting hex grid to st_polygon and set CRS
fh_grid <- fh_grid %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")

# recenter hex grid
recent_fh_grid <- recentre(fh_grid, shift) # some long >360

crossing_cells <- find_cross(recent_fh_grid) # 78 cells
recent_fh_grid_new <- replace_cross(recent_fh_grid) 
recent_fh_grid_0360 <- wrap_360plus(recent_fh_grid_new)

# plot it out 

p_fh <- plot_world(recent_fh_grid_0360, recent_fh_grid_0360$avg_fh, "Annual total fishing hours")
p_fh

# save the plot
outputfile <- paste0("data/analysis/figures/global_fishing_hour/total_fishing_hour_cly.png")
ggsave(filename = outputfile, plot = p_fh, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

#####
# plot annual species timespent
grid_all <- read_rds("data/analysis/glob_hexgrid/global_hexgrid_95km_timespent_allspp_ann.rds")

# converting hex grid to st_polygon and set CRS
sp_grid <- grid_all %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")

# recenter hex grid
recent_sp_grid <- recentre(sp_grid, shift) # some long >360

crossing_cells <- find_cross(recent_sp_grid) # 43 cells
recent_sp_grid_new <- replace_cross(recent_sp_grid) 
recent_sp_grid_0360 <- wrap_360plus(recent_sp_grid_new)

# plot it out 
p_sp <- plot_world(recent_sp_grid_0360, recent_sp_grid_0360$timespent, "Annual total timespent")
p_sp

# save the plot
outputfile <- paste0("data/analysis/figures/glob_timespent_all_ann_timespent_log.png")
ggsave(filename = outputfile, plot = p_sp, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

######
# match with species time spent
grid_overlap <- read_rds("data/analysis/glob_hexgrid/hexgrid_res8_overlap_cly.rds")
recent_ov_grid <- recentre(grid_overlap, shift)

crossing_cells <- find_cross(recent_ov_grid) # 27 cells
recent_ov_grid_new <- replace_cross(recent_ov_grid)
recent_ov_grid_0360 <- wrap_360plus(recent_ov_grid_new)

# plot it out 
p_ov <- plot_world(recent_ov_grid_0360, recent_ov_grid_0360$overlap, "Annual total overlap")
p_ov

# save the plot
outputfile <- paste0("data/analysis/figures/global_overlap_cly.png")
ggsave(filename = outputfile, plot = p_ov, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# merge overlap with front freq
grid_var <- read_rds("data/analysis/glob_hexgrid/hexgrid_res8_frfr10_cly.rds")
# recenter 
grid_var <- grid_var %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")
grid_var$frfr <- as.numeric(grid_var$frfr)

recent_var_grid <- recentre(grid_var, shift)

crossing_cells <- find_cross(recent_var_grid) # 27 cells
recent_var_grid_new <- replace_cross(recent_var_grid)
recent_var_grid_0360 <- wrap_360plus(recent_var_grid_new)

# plot it out 
p_ff <- ggplot() +
  geom_sf(data = recent_var_grid_0360, aes(fill = frfr), color = "black") +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle(paste0("Front frequency underlying annual overlap grids")) +
  geom_polygon(data = map_df, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey") +
  geom_sf(data = eez_boundaries)
p_ff

# save the plot
outputfile <- paste0("data/analysis/figures/front_frequency_thres10_cly.png")
ggsave(filename = outputfile, plot = p_ff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

