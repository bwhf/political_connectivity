# plot it out
# recentering and projection objects
library(lubridate)
library(tidyverse)
library(sf)
library(dplyr)
library(dggridR)
library(readr)
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

# read data to get crs
grid_fh_cly <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_cly.rds")

grid_over_cls <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/hexgrid_res8_overlap_fh_ts_front_cls_mths.rds")

grid_over_cls <- grid_over_cls %>% 
  mutate(season = fct_relevel(season, "Spring", "Summer", "Autumn", "Winter"))

# filter to keep only presence of fishing effort
fh_grid <- grid_over_cls %>%
  filter(avg_fh > 0)

if (!all(c("cell", "avg_fh", "geometry") %in% colnames(fh_grid))) {
  stop("Grid object does not contain the expected columns.")
}

# converting hex grid to st_polygon and set CRS
fh_grid <- fh_grid %>%
  st_as_sf() %>%
  st_set_crs(st_crs(grid_fh_cly)) %>%
  st_cast("POLYGON")

# recenter hex grid
recent_fh_grid <- recentre(fh_grid, shift) # some long >360

crossing_cells <- find_cross(recent_fh_grid) # 251 cells
check_crossing_cells <- crossing_cells %>% group_by(season) %>% summarise(n = n()) # cells per season
recent_fh_grid_new <- replace_cross(recent_fh_grid) 
recent_fh_grid_0360 <- wrap_360plus(recent_fh_grid_new)

# plot it out 
p_fh <- plot_world_cls(recent_fh_grid_0360, recent_fh_grid_0360$avg_fh, "Seasonal total fishing hours")
p_fh

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/global_fishing_hour/total_fishing_hour_cls_mths.png")
ggsave(filename = outputfile, plot = p_fh, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

#####
# plot annual species timespent
grid_all <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/hexgrid_res8_overlap_ts_fh_front_cls_mths.rds")

grid_all <- grid_all %>% 
  mutate(season = fct_relevel(season, "Spring", "Summer", "Autumn", "Winter"))

# converting hex grid to st_polygon and set CRS
sp_grid <- grid_all %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")

# recenter hex grid
recent_sp_grid <- recentre(sp_grid, shift) # some long >360

crossing_cells <- find_cross(recent_sp_grid) # 63 cells
check_crossing_cells <- crossing_cells %>% group_by(season) %>% summarise(n = n()) # cells per season
recent_sp_grid_new <- replace_cross(recent_sp_grid) 
recent_sp_grid_0360 <- wrap_360plus(recent_sp_grid_new)

# plot it out 
p_sp <- plot_world_cls(recent_sp_grid_0360, recent_sp_grid_0360$total_ts, "Annual total timespent")
p_sp

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/glob_timespent_all_cls_mths.png")
ggsave(filename = outputfile, plot = p_sp, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# Replace NA with 0 in the entire dataframe
recent_sp_grid_0360$avg_fh[is.na(recent_sp_grid_0360$avg_fh)] <- 0

pdat <- recent_sp_grid_0360 %>% filter(avg_fh == 0)

# plot it out 
p_sp <- plot_world_cls(pdat, pdat$total_ts, "Annual total timespent where no presence of fishing effort")
p_sp

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/glob_timespent_dawn_all_cls_mths_timespent_log_nofh.png")
ggsave(filename = outputfile, plot = p_sp, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

######
# plotting global overlap 
p_ov <- plot_world_cls(recent_fh_grid_0360, recent_fh_grid_0360$overlap, "Annual total overlap")
p_ov

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/global_overlap_dawn_cls_wheat.png")
ggsave(filename = outputfile, plot = p_ov, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# plot front freq
recent_fh_grid_0360$front_freq <- as.numeric(recent_fh_grid_0360$front_freq)
recent_ov_grid_0360 <- recent_fh_grid_0360 %>% filter(overlap > 0)
p_ff <-  plot_world_cls(recent_ov_grid_0360, recent_ov_grid_0360$front_freq, "Frontal frequency underlying overlap")

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/front_frequency_dawn_thres18_cls_mths.png")
ggsave(filename = outputfile, plot = p_ff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

#####
# simple scatter plot of overlap vs front freq
p_ov_ff <- ggplot() + geom_point(data = recent_ov_grid_0360, aes(y = log10(overlap), x = front_freq, color = log10(total_ts))) + theme_grey()
p_ov_ff
# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_ff_thres18_dawn_cls.png")
ggsave(filename = outputfile, plot = p_ov_ff, width = 1280, height = 2160, unit = "px", dpi = 200, bg = "white")

hist(recent_ov_grid_0360$front_freq)

# scatter plot of timespent vs front freq

# Replace NA with 0 in the entire dataframe
recent_sp_grid_0360$overlap[is.na(recent_sp_grid_0360$overlap)] <- 0
recent_sp_grid_0360$front_freq <- as.numeric(recent_sp_grid_0360$front_freq)
grid_var_ts <- recent_sp_grid_0360 %>% 
  mutate(co = case_when(
    overlap == 0 ~ "No overlap",
    overlap > 0 ~ "Overlapped"
  ))

p_ts_ff <- ggplot() + geom_point(data = grid_var_ts, aes(y = log10(total_ts), x = front_freq, color = log10(overlap)), alpha = 0.8) + facet_wrap(vars(co)) + theme_grey()
p_ts_ff

outputfile <- paste0("data/analysis/figures_dawn/timespent_vs_ff_thres18_dawn_cls.png")
ggsave(filename = outputfile, plot = p_ts_ff, width = 2560, height = 2160, unit = "px", dpi = 200, bg = "white")

# box and whisker plot of ts vs ff (yes/ no overlap)
# Remove missing values before plotting
clean_var_ts <- grid_var_ts %>%
  filter(!is.na(total_ts) & !is.na(front_freq))

p_box_ts_ff <- ggplot() + geom_boxplot(data = clean_var_ts, aes(x = log10(total_ts), y = front_freq, group = co, color = co)) + facet_grid(season ~ co) + theme_grey()
p_box_ts_ff

outputfile <- paste0("data/analysis/figures_dawn/box_timespent_vs_ff_thres18_dawn_cls.png")
ggsave(filename = outputfile, plot = p_box_ts_ff, width = 2560, height = 2160, unit = "px", dpi = 200, bg = "white")


