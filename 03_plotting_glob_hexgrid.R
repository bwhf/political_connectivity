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

# plot where only bird present but no fishing effort
grid_ts_fh <- fuzzyjoin::fuzzy_left_join(x = grid_all, y = grid_fh_cly, by = c("cell"), match_fun = `==`)
grid_bo <- grid_ts_fh %>% dplyr::select(-cell.y, -geometry.y) %>% rename("cell" = "cell.x", "geometry" = "geometry.x")
# Replace NA with 0 in the entire dataframe
grid_bo$avg_fh[is.na(grid_bo$avg_fh)] <- 0


# converting hex grid to st_polygon and set CRS
sp_grid_bo <- grid_bo %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")

# recenter hex grid
recent_sp_grid_bo <- recentre(sp_grid_bo, shift) # some long >360

crossing_cells <- find_cross(recent_sp_grid_bo) # 43 cells
recent_sp_grid_new <- replace_cross(recent_sp_grid_bo) 
recent_sp_grid_0360 <- wrap_360plus(recent_sp_grid_new)
pdat <- recent_sp_grid_0360 %>% filter(avg_fh == 0)

# plot it out 
p_sp <- plot_world(pdat, pdat$timespent, "Annual total timespent")
p_sp

# save the plot
outputfile <- paste0("data/analysis/figures/glob_timespent_all_ann_timespent_log_nofh.png")
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
grid_var <- read_rds("data/analysis/glob_hexgrid/hexgrid_res8_frfr_cly.rds")
# recenter 
grid_var <- grid_var %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")

recent_var_grid <- recentre(grid_var, shift)

crossing_cells <- find_cross(recent_var_grid) # 27 cells
recent_var_grid_new <- replace_cross(recent_var_grid)
recent_var_grid_0360 <- wrap_360plus(recent_var_grid_new)

# plot it out 
p_ff <- ggplot() +
  geom_sf(data = recent_var_grid_0360, aes(fill = frfr27), color = "black") +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle(paste0("Front frequency underlying annual overlap grids")) +
  geom_polygon(data = map_df, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey") +
  geom_sf(data = eez_boundaries)
p_ff

# save the plot
outputfile <- paste0("data/analysis/figures/front_frequency_thres27_cly.png")
ggsave(filename = outputfile, plot = p_ff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# simple scatter plot of overlap vs front freq
grid_var1 <- grid_var %>% mutate(overlap = avg_fh*timespent)

p_ov_ff10 <- ggplot() + geom_point(data = grid_var1, aes(y = log10(overlap), x = frfr10, color = log10(timespent))) + theme_grey()
p_ov_ff10
hist(grid_var1$frfr10)

p_ov_ff18 <- ggplot() + geom_point(data = grid_var1, aes(y = log10(overlap), x = frfr18, color = log10(timespent))) + theme_grey()
p_ov_ff18
hist(grid_var1$frfr18)

p_ov_ff27 <- ggplot() + geom_point(data = grid_var1, aes(y = log10(overlap), x = frfr27, color = log10(timespent))) + theme_grey()
p_ov_ff27
hist(grid_var1$frfr27)

# scatter plot of timespent vs front freq
grid_var_ts <- read_rds("data/analysis/glob_hexgrid/hexgrid_res8_frfr_allts_cly.rds")
grid_var_ts <- grid_var_ts %>% mutate(overlap = avg_fh*timespent)

# Replace NA with 0 in the entire dataframe
grid_var_ts$overlap[is.na(grid_var_ts$overlap)] <- 0
grid_var_ts <- grid_var_ts %>% 
  mutate(co = case_when(
    overlap == 0 ~ "No overlap",
    overlap > 0 ~ "Overlapped"
))


p_ts_ff10 <- ggplot() + geom_point(data = grid_var_ts, aes(y = log10(timespent), x = frfr10, color = log10(overlap)), alpha = 0.8) + facet_wrap(vars(co)) + theme_grey()
p_ts_ff10

p_ts_ff18 <- ggplot() + geom_point(data = grid_var_ts, aes(y = log10(timespent), x = frfr18, color = log10(overlap)), alpha = 0.8) + facet_wrap(vars(co)) + theme_grey()
p_ts_ff18

p_ts_ff27 <- ggplot() + geom_point(data = grid_var_ts, aes(y = log10(timespent), x = frfr27, color = log10(overlap)), alpha = 0.8) + facet_wrap(vars(co)) + theme_grey()
p_ts_ff27


