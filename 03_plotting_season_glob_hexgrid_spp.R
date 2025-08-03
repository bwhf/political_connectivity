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

myworld180 = maps::map(wrap=c(-180,180), plot=FALSE, fill=TRUE)
# Convert the map data to a dataframe for ggplot2 plotting
map_df180 <- map_data(myworld180)

# Read eez.shp just the boundaries
eez_boundaries <- sf::st_read("/Users/bwhf/Documents/GitHub/political_connectivity/data/Marine_regions/World_EEZ_v11_20191118_HR_0_360", layer="eez_boundaries_v11_0_360")

# shifting cell to central meridian
shift <- -153
central_meridian <- 360 + shift
proj <- sprintf("+proj=kav7 +lon_0=%i", central_meridian)

## Fishing hours
# read data to get crs
grid_fh_cly <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_cly.rds")

grid_over_cls <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_fh_ts_cls.rds") # all grid with fishing hours

grid_over_cls <- grid_over_cls %>% 
  mutate(season = fct_relevel(season, "Spring", "Summer", "Autumn", "Winter"))

# filter to keep only presence of fishing effort
fh_grid <- grid_over_cls %>%
  filter(avg_fh > 0)

if (!all(c("cell", "avg_fh", "geometry") %in% colnames(fh_grid))) {
  stop("Grid object does not contain the expected columns.")
}

# create hex grid
spatial <- dgconstruct(spacing=100, metric=TRUE, resround='nearest') # spacing unit  CLS (km)
wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing

# create cell geometry
grid_fh <- dgcellstogrid(spatial, cells = as.numeric(fh_grid$cell)) # get only cells which contained fixes
grid_fh <- grid_fh %>% rename("cell" = "seqnum")
grid_fh1 <- merge(grid_fh, fh_grid, by.x="cell")

# converting hex grid to st_polygon and set CRS
grid_fh1 <- grid_fh1 %>%
  st_as_sf() %>%
  st_set_crs(st_crs(grid_fh_cly)) %>%
  st_cast("POLYGON")

# recenter hex grid
recent_fh_grid <- recentre(grid_fh1, shift) # some long >360

crossing_cells <- find_cross(recent_fh_grid) # 256 cells
check_crossing_cells <- crossing_cells %>% group_by(season) %>% summarise(n = n()) # cells per season
recent_fh_grid_new <- replace_cross(recent_fh_grid) 
recent_fh_grid_0360 <- wrap_360plus(recent_fh_grid_new)

# # plot it out
# p_fh <- plot_world_cls(recent_fh_grid_0360, recent_fh_grid_0360$avg_fh, "Seasonal total fishing hours")
# p_fh
# 
# # save the plot
# outputfile <- paste0("data/analysis/figures_dawn/global_fishing_hour/total_fishing_hour_cls.png")
# ggsave(filename = outputfile, plot = p_fh, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

#####
# plot annual species timespent
# grid_all <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_ts_fh_cls.rds")

grid_all <- read_rds("data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_ts_fh_cls.rds") # with new fronts data: front freq 18, pfront, msfront

grid_all <- grid_all %>% 
  mutate(season = fct_relevel(season, "Spring", "Summer", "Autumn", "Winter"))

grid_all <- grid_all %>% 
  mutate(ol = case_when(
    overlap == 0 ~ "No overlap",
    overlap > 0 ~ "Overlapped"
  ))

grid_all$ol[is.na(grid_all$ol)] <- "No overlap"

# create hex grid
spatial <- dgconstruct(spacing=100, metric=TRUE, resround='nearest') # spacing unit  CLS (km)
wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing

# create cell geometry
grid_allg <- dgcellstogrid(spatial, cells = as.numeric(grid_all$cell)) # get only cells which contained fixes
grid_allg <- grid_allg %>% rename("cell" = "seqnum")
grid_allg <- merge(grid_allg, grid_all, by.x="cell")

# converting hex grid to st_polygon and set CRS
sp_grid <- grid_allg %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")

# recenter hex grid
recent_sp_grid <- recentre(grid_allg, shift) # some long >360

## assign ocean regions to data

# add ocean region to dataset
# Step 1: Load your species dataset (which has DGGRID cell IDs)
ocean_region <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/Ocean_regions/dggrid_ocean_regions.rds")
ocean_region <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/Ocean_regions/dggrid_IHO_ocean_regions.rds") # IHO oceans

# Step 2: Merge ocean names using DGGRID cell ID
sp_grid1 <- sp_grid %>%
  left_join(ocean_region, by = "cell")  # Matches by cell number

# # Alternative method: Remove duplicates (more general)
# sp_grid2 <- sp_grid1 %>%
#   distinct(.keep_all = TRUE)  # Keeps only one row per cell

# Check results
table(sp_grid1$ocean_name)

# groups seas into 7 oceans
# Assign each sea to one of 7 major oceans
sp_grid1 <- sp_grid1 %>%
  mutate(ocean = case_when(
    ocean_name %in% c("Bering Sea", "North Pacific Ocean", "Gulf of Alaska", 
                      "The Coastal Waters of Southeast Alaska and British Columbia",
                      "Gulf of California", "Philippine Sea", "Japan Sea", "Sea of Okhotsk") ~ "North Pacific Ocean",
    
    ocean_name %in% c("South Pacific Ocean", "Tasman Sea", "Coral Sea") ~ "South Pacific Ocean",
    
    ocean_name %in% c("North Atlantic Ocean", "Gulf of St. Lawrence", "Caribbean Sea") ~ "North Atlantic Ocean",
    
    ocean_name %in% c("South Atlantic Ocean", "Rio de La Plata") ~ "South Atlantic Ocean",
    
    ocean_name %in% c("Adriatic Sea", "Mediterranean Sea - Eastern Basin", 
                      "Tyrrhenian Sea", "Ionian Sea") ~ "Mediterranean Sea",
    
    ocean_name %in% c("Indian Ocean", "Mozambique Channel") ~ "Indian Ocean",
    
    ocean_name %in% c("Southern Ocean", "Great Australian Bight", "Bass Strait") ~ "Southern Ocean",
    
    TRUE ~ NA_character_  # Keep NA if unknown
  ))

# Print the results
table(sp_grid1$ocean)

# save dataframe
write_rds(sp_grid1, "data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_complete.rds")

# converting hex grid to st_polygon and set CRS
sp_grid1_sf <- sp_grid1 %>%
  st_as_sf() %>%
  st_set_crs(4326) %>%
  st_transform(4326) %>%
  st_cast("POLYGON")

# recenter hex grid
recent_sp_grid_ocean <- recentre(sp_grid1_sf, shift) # some long >360
recent_sp_grid_ocean <- recent_sp_grid_ocean%>% 
  mutate(season = fct_relevel(season, "Spring", "Summer", "Autumn", "Winter"))

write_rds(recent_sp_grid_ocean, "data/analysis/glob_hexgrid_dawn/recent_sp_ov_cls_grid_ocean.rds")

recent_sp_grid_ocean <- read_rds("data/analysis/glob_hexgrid_dawn/recent_sp_ov_cls_grid_ocean.rds")

# remove cells cross dateline
crossing_cells <- find_cross(recent_sp_grid_ocean) # 68 cells, 39 overlapped
check_crossing_cells <- crossing_cells %>% group_by(season) %>% summarise(n = n()) # cells per season
recent_sp_grid_new <- replace_cross(recent_sp_grid_ocean) 
recent_sp_grid_new360 <- wrap_360plus(recent_sp_grid_new)
recent_sp_grid_0360 <- wrap_360plus(recent_sp_grid_new) # exclude crossing cells only for plotting on maps

## plot on interactive map, color by species and group by overlap/not
# plot it out 
p_spp <- plot_world_spp_pop(recent_sp_grid_new360, recent_sp_grid_new360$scientific_name, "Population by species")
p_spp

## check Laysan and blackfooted and black-browed
spp_list <- list(c("Phoebastria immutabilis", "Phoebastria nigripes", "Thalassarche melanophris"))

grid_lay_ov <- recent_sp_grid_0360 %>% filter(scientific_name == spp_list[[1]][1])
p_lay_ov <- plot_world_spp_ov(grid_lay_ov, grid_lay_ov$ol, paste0(spp_list[[1]][1]))
p_lay_ov

## plot by ocean regions
# Define the oceans to plot
target_oceans <- c("North Pacific Ocean", "South Pacific Ocean", "Indian Ocean")

# Filter dataset for only these oceans
species_ocean_data <- recent_sp_grid_0360 %>%
  filter(ocean %in% target_oceans)

pint_npac_spp <- plot_ocean_spp_pop(species_ocean_data, "North Pacific Ocean", "North Pacific - Species Presence", map_df) # call function
pint_spac_spp <- plot_ocean_spp_pop(species_ocean_data, "South Pacific Ocean", "South Pacific - Species Presence", map_df)
pint_indian_spp <- plot_ocean_spp_pop(species_ocean_data, "Indian Ocean", "Indian Ocean - Species Presence", map_df)

pint_npac_spp
pint_spac_spp
pint_indian_spp

# plot fronts map by season
# Convert to sf
all_front_st <- st_as_sf(all_front)

# convert to 360
all_front_st360 <- wrap_360plus(all_front_st)

# convert to 360
all_front_st360 <- wrap_360plus_dateline(all_front_st)
st_geometry(all_front_st360)

# remove cells cross dateline
crossing_cells_fronts <- find_cross(all_front_st360) # 68 cells, 39 overlapped
check_crossing_cells <- crossing_cells_fronts %>% group_by(season) %>% summarise(n = n()) # cells per season
all_front_st360_new <- replace_cross(all_front_st360) 


# Confirm it's fixed:
st_geometry(all_front_st360_new)

p_frfr <- plot_world_fronts(all_front_st360, front_freq, "Seasonal map of front freq")
p_frfr

# save the plot
outputfile <- paste0("data/analysis/figures/fronts_frontfreq_season.png")
ggsave(filename = outputfile, plot = p_frfr, width = 1920, height = 1080, unit = "px", dpi = 80, bg = "white")

p_msfr <- plot_world_fronts(all_front_st360, msfront, "Seasonal map of mean front strength")
p_msfr

# save the plot
outputfile <- paste0("data/analysis/figures/fronts_msfront_season.png")
ggsave(filename = outputfile, plot = p_msfr, width = 1920, height = 1080, unit = "px", dpi = 80, bg = "white")

p_pfr <- plot_world_fronts(all_front_st360, pfront, "Seasonal map of front persistence")
p_pfr

# save the plot
outputfile <- paste0("data/analysis/figures/fronts_pfront_season.png")
ggsave(filename = outputfile, plot = p_msfr, width = 1920, height = 1080, unit = "px", dpi = 80, bg = "white")

# plot for atlantic
# extract cells in atlantic ocean and convert to -180 -180
# write function for -180 to 180 
# Function to assign ocean based on -180 to 180 longitude format
assign_ocean_180 <- function(geometry) {
  # Compute centroid of the DGGRID cell
  centroid <- st_centroid(geometry)
  
  # Extract longitude and latitude
  lon <- st_coordinates(centroid)[, 1]  # X-coordinate (longitude)
  lat <- st_coordinates(centroid)[, 2]  # Y-coordinate (latitude)
  
  # Assign ocean region
  if (lat > 60) {
    return("Arctic Ocean")
  } else if (lat < -60) {
    return("Southern Ocean")
  } else if (lon >= -80 & lon < 20) {
    if (lat >= 0) {
      return("North Atlantic")
    } else if (lon < -50) {  # Fix for South America west coast
      return("South Pacific")
    } else {
      return("South Atlantic")
    }
  } else if (lon >= 20 & lon < 147) {
    return("Indian Ocean")
  } else if (lon >= 147 | lon < -80) {  # Corrects Pacific boundaries
    if (lat >= 0) {
      return("North Pacific")
    } else {
      return("South Pacific")
    }
  } else {
    return(NA)  # Unclassified (shouldn't happen)
  }
}

# Apply the function to the species dataset
atlantic_grid <- sp_grid %>% 
  mutate(ocean = sapply(geometry, assign_ocean_180)) %>%  # Apply function to each row
  filter(ocean %in% c("North Atlantic", "South Atlantic"))

atlantic_grid <- atlantic_grid %>% 
  filter(!(ocean %in% c("North Atlantic", "South Atlantic") & (st_coordinates(st_centroid(geometry))[, 1] > 170 | st_coordinates(st_centroid(geometry))[, 1] < -170))) # filter out cells crossing dateline

pint_natl_spp <- plot_ocean_spp_pop(atlantic_grid, "North Atlantic", "North Atlantic - Species Presence", map_df180) # call function
pint_satl_spp <- plot_ocean_spp_pop(atlantic_grid, "South Atlantic", "South Atlantic - Species Presence", map_df180) # call function

pint_natl_spp
pint_satl_spp

# Plot species presence for each ocean
p_spp_oceanregion <- ggplot() +
  geom_sf(data = species_ocean_data, aes(fill = scientific_name), color = NA) +  # Color by species
  facet_wrap(~ ocean) +  
  geom_polygon(data = map_df, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey", alpha = 0.5) +
  geom_sf(data = eez_boundaries, color = "grey", alpha = 0.7) +
# Separate maps for each ocean
  labs(
    title = "Species Presence in Major Ocean Regions",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

p_spp_oceanregion

###### Boxplots
## boxplot by species
## overlap
p_spov_box_ff <- ggplot() + geom_boxplot(data = sp_grid1, aes(x = front_freq, y = log10(overlap), color = season, group = scientific_name)) + geom_jitter(data = sp_grid1, aes(x = front_freq, y = log10(overlap), color = season), alpha = 0.7, shape = 1) + facet_wrap(vars(scientific_name)) + theme_grey() + theme(text = element_text(size = 20)) 
p_spov_box_ff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_ff_cls_byspp.png")
ggsave(filename = outputfile, plot = p_spov_box_ff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## identify species: 
## 1st priority: Phoebastria immutabilis (Laysan), Diomedea exulans (Snowy), Diomedea amsterdamensis (Amsterdam), Phoebetria fuscas (Sooty)
## 2nd: Diomedea antipodensis (Antipodean), Phoebastria nigripes (Black-footed), Procellaria aequinoctialis (White-chinned petrel), Procellaria conspicillata (Spectacled)
## 3rd: Ardenna gravis (Great shearwater), Phoebetria palpebrata (Light-mantled), Thalassarche carteri (Indian yellow-nosed), Thalassarche melanophris (black-browed)
## 4th: Ardenna carneipes (Flesh-footed shearwater), Ardenna creatopus (Pink-footed shearwater), Calonectris borealis (Cory's shearwater), Diomedea dabbenena (Tristan), Diomedea sanfordi (Northern royal), Macronectes giganteus (Southern giant petrel), Macronectes halli (Northern giant petrel), Phoebastria albatrus (Short-tailed), 

ov_ff_sp_v_ssn <- function(dat, vary, xtitle) {
  plot_sel_ovff <- ggplot() + geom_boxplot(data = dat, aes(x = vary, y = log10(overlap), color = season, group = season)) + geom_jitter(data = dat, aes(x = vary, y = log10(overlap), color = season, group = season), alpha = 0.5, shape = 1) + facet_grid(season ~ scientific_name) + labs(x = xtitle) + theme_grey() + theme(text = element_text(size = 20))
  return(plot_sel_ovff)
}

# all
plot_spp <- ov_ff_sp_v_ssn(sp_grid1, sp_grid1$pfront, "pfront")
plot_spp

# 1st priority:
grid_select <- grid_all %>% filter(scientific_name %in% c("Diomedea antipodensis", "Diomedea exulans", "Diomedea amsterdamensis", "Phoebetria fusca"))

# front freq
plot_sel1_ovff <- ov_ff_sp_v_ssn(grid_select, grid_select$front_freq, "front frequency")
plot_sel1_ovff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_ff_cls_byspp_select1.png")
ggsave(filename = outputfile, plot = plot_sel1_ovff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front persistence
plot_sel1_ovpf <- ov_ff_sp_v_ssn(grid_select, grid_select$pfront, "front persistence")
plot_sel1_ovpf

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_pf_cls_byspp_select1.png")
ggsave(filename = outputfile, plot = plot_sel1_ovpf, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front strength
plot_sel1_ovsf <- ov_ff_sp_v_ssn(grid_select, grid_select$msfront, "mean front strength")
plot_sel1_ovsf

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_sf_cls_byspp_select1.png")
ggsave(filename = outputfile, plot = plot_sel1_ovsf, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# 2nd priority:
grid_select2 <- grid_all %>% filter(scientific_name %in% c("Phoebastria immutabilis", "Phoebastria nigripes", "Procellaria aequinoctialis", "Procellaria conspicillata"))

# front freq
plot_sel2_ovff <- ov_ff_sp_v_ssn(grid_select2, grid_select2$front_freq, "front frequency")
plot_sel2_ovff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_ff_cls_byspp_select2.png")
ggsave(filename = outputfile, plot = plot_sel2_ovff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front persistence
plot_sel2_ovpf <- ov_ff_sp_v_ssn(grid_select2, grid_select2$pfront, "front persistence")
plot_sel2_ovpf

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_pf_cls_byspp_select2.png")
ggsave(filename = outputfile, plot = plot_sel2_ovpf, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front strength
plot_sel2_ovsf <- ov_ff_sp_v_ssn(grid_select2, grid_select2$msfront, "mean front strength")
plot_sel2_ovsf

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_sf_cls_byspp_select2.png")
ggsave(filename = outputfile, plot = plot_sel2_ovsf, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# 3rd priority:
grid_select3 <- grid_all %>% filter(scientific_name %in% c("Ardenna gravis", "Phoebetria palpebrata", "Thalassarche carteri", "Thalassarche melanophris"))

# front freq
plot_sel3_ovff <- ov_ff_sp_v_ssn(grid_select3, grid_select3$front_freq, "front frequency")
plot_sel3_ovff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_ff_cls_byspp_select3.png")
ggsave(filename = outputfile, plot = plot_sel3_ovff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front persistence
plot_sel3_ovpf <- ov_ff_sp_v_ssn(grid_select3, grid_select3$pfront, "front persistence")
plot_sel3_ovpf

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_pf_cls_byspp_select3.png")
ggsave(filename = outputfile, plot = plot_sel3_ovpf, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front strength
plot_sel3_ovsf <- ov_ff_sp_v_ssn(grid_select3, grid_select3$msfront, "mean front strength")
plot_sel3_ovsf

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_sf_cls_byspp_select3.png")
ggsave(filename = outputfile, plot = plot_sel3_ovsf, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")


## Boxplot by region
ov_ff_sp_v_ocean <- function(dat, vary, xtitle) {
  plot_sel_ovff <- ggplot() + geom_boxplot(data = dat, aes(x = vary, y = log10(overlap), color = season, group = season)) + geom_jitter(data = dat, aes(x = vary, y = log10(overlap), color = season, group = season), alpha = 0.5, shape = 1) + facet_grid(season ~ ocean) + labs(x = xtitle) + theme_grey() + theme(text = element_text(size = 20))
  return(plot_sel_ovff)
}

## finding proportion of overlap where front freq is higher than xx%
# Categorize overlaps (with flexibility for different thresholds)
cat_overlap <- function(data, threshold) {
  data %>%
    filter(overlap > 0) %>%
    mutate(co = case_when(
      front_freq == 0 ~ "No signal",
      front_freq > threshold ~ paste0("Above ", threshold, "%"), 
      front_freq <= threshold ~ paste0("Below ", threshold, "%")
    )) %>%
    mutate(co = factor(co, levels = c("No signal", paste0("Below ", threshold, "%"), paste0("Above ", threshold, "%")))) %>%
    mutate(co = replace_na(co, "No signal"))
}

cal_prop <- function(dat) {
  # calculate proportion by season and species
  prop_ov <- dat %>% 
    group_by(scientific_name, season, co) %>%
    summarise(n = n()) %>%
    arrange(scientific_name, co, season)
  
  total_ov <- dat %>% 
    group_by(scientific_name, season) %>%
    summarise(n = n()) %>%
    arrange(scientific_name, season)
  
  # Calculate proportion by adding total count to prop_ov
  prop_ov <- prop_ov %>%
    left_join(total_ov, by = c("scientific_name", "season"), suffix = c("_prop", "_total")) %>%
    mutate(proportion = n_prop / n_total)
  
  # Print the updated dataframe
  return(prop_ov)
}

# Stacked column chart for each species, faceted by season
plot_prop_ov <- function(dat, threshold) {
  plot <- ggplot(data = dat, aes(x = scientific_name, y = proportion, fill = co)) +
    geom_col(position = "stack") +  # Stacked columns
    geom_text(
      aes(label = scales::percent(proportion, accuracy = 0.1)),  # Show proportions as percentages
      position = position_stack(vjust = 0.5),  # Center labels in each stack
      size = 3  # Adjust text size
    ) +
    facet_grid(vars(rows = season)) +  # Facet by season
    labs(
      x = "Scientific Name",
      y = "Proportion",
      fill = "Front Freq Category (co)",
      title = paste0("Proportion of overlaps at ", threshold, "% front freq by Species and Season")) +
    theme_grey() +
    theme(text = element_text(size = 18),
      axis.text.x = element_text(angle = 45, hjust = 1),        # Rotate x-axis labels
      legend.text = element_text(size = 12), 
      legend.title = element_text(size = 12),
      legend.position = "bottom"
    )
}

# Apply categorization using 50% front freq
grid_ov_cls_spp <- cat_overlap(grid_all, 50)
prop_ov <- cal_prop(grid_ov_cls_spp)
p_prop_ov <- plot_prop_ov(prop_ov, 50)
p_prop_ov

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/prop_ff_ov_cls_byspp_50.png")
ggsave(filename = outputfile, plot = p_prop_ov, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# calculate at 70%
# Apply categorization using 70% front freq
grid_ov_cls_spp <- cat_overlap(grid_all, 70)
prop_ov <- cal_prop(grid_ov_cls_spp)
p_prop_ov <- plot_prop_ov(prop_ov, 70)
p_prop_ov

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/prop_ff_ov_cls_byspp_70.png")
ggsave(filename = outputfile, plot = p_prop_ov, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")


# individual
ov_ff_spp <- function(dat, sp_name) {
  grid_sp <- dat
  grid_sp <- grid_sp %>% filter(scientific_name == sp_name)
  plot_spp <- ggplot() + geom_boxplot(data = grid_sp, aes(x = front_freq, y = log10(overlap), color = season, group = season)) + geom_jitter(data = grid_sp, aes(x = front_freq, y = log10(overlap), color = season, group = season), alpha = 0.5, shape = 1) + facet_wrap(vars(season)) + theme_grey() + theme(text = element_text(size = 20))
  return(plot_spp)
}

p_laysan <- ov_ff_spp(grid_all, "Phoebastria immutabilis")
p_laysan

## finding front-like species?
## timespent vs ff
p_sp_box_tsff <- ggplot() + geom_jitter(data = grid_all, aes(x = front_freq, y = log10(total_ts), color = season), alpha = 0.7, shape = 1) + facet_wrap(vars(scientific_name)) + theme_gray() + theme(text = element_text(size = 20))
p_sp_box_tsff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_ff_cls_byspp.png")
ggsave(filename = outputfile, plot = p_sp_box_tsff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# Boxplot by species
ts_ff_sp_v_ssn <- function(dat, vars, ytitle) {
  plot_sel_tsff <- ggplot() + geom_jitter(data = dat, aes(y = vars, x = log10(total_ts) + 1, group = ol, color = ol, alpha = 0.3)) + geom_boxplot(data = dat, aes(y = vars, x = log10(total_ts) + 1, group = ol, fill = ol)) + facet_grid(scientific_name ~ season) + labs(y = ytitle) + theme_grey() + theme(text = element_text(size = 20))
  return(plot_sel_tsff)
}

plot_sel1_tsff <- ts_ff_sp_v_ssn(grid_select, grid_select$front_freq, "front frequency")
plot_sel1_tsff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_ff_cls_byspp_select1_ov.png")
ggsave(filename = outputfile, plot = plot_sel1_tsff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

plot_sel2_tsff <- ts_ff_sp_v_ssn(grid_select2, grid_select2$front_freq, "front frequency")
plot_sel2_tsff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_ff_cls_byspp_select2_ov.png")
ggsave(filename = outputfile, plot = plot_sel2_tsff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

plot_sel3_tsff <- ts_ff_sp_v_ssn(grid_select3, grid_select3$front_freq, "front frequency")
plot_sel3_tsff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_ff_cls_byspp_select3_ov.png")
ggsave(filename = outputfile, plot = plot_sel3_tsff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## timespent vs mean front strength 
plot_sel1_tsfs <- ts_ff_sp_v_ssn(grid_select, grid_select$msfront, "front strength")
plot_sel1_tsfs

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_fs_cls_byspp_select1_ov.png")
ggsave(filename = outputfile, plot = plot_sel1_tsfs, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

plot_sel2_tsfs <- ts_ff_sp_v_ssn(grid_select2, grid_select2$msfront, "front strength")
plot_sel2_tsfs

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_fs_cls_byspp_select2_ov.png")
ggsave(filename = outputfile, plot = plot_sel2_tsfs, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

plot_sel3_tsfs <- ts_ff_sp_v_ssn(grid_select3, grid_select3$msfront, "front strength")
plot_sel3_tsfs

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_fs_cls_byspp_select3_ov.png")
ggsave(filename = outputfile, plot = plot_sel3_tsfs, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## timespent vs front persistence 
plot_sel1_tsfp <- ts_ff_sp_v_ssn(grid_select, grid_select$pfront, "front persistence")
plot_sel1_tsfp

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_fp_cls_byspp_select1_ov.png")
ggsave(filename = outputfile, plot = plot_sel1_tsfp, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

plot_sel2_tsfp <- ts_ff_sp_v_ssn(grid_select2, grid_select2$pfront, "front persistence")
plot_sel2_tsfp

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_fp_cls_byspp_select2_ov.png")
ggsave(filename = outputfile, plot = plot_sel2_tsfp, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

plot_sel3_tsfp <- ts_ff_sp_v_ssn(grid_select3, grid_select3$pfront, "front persistence")
plot_sel3_tsfp

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_fp_cls_byspp_select3_ov.png")
ggsave(filename = outputfile, plot = plot_sel3_tsfp, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## fishhour vs fronts
fh_ff_sp_v_ssn <- function(vars, xtit) {
  p <- ggplot() + geom_jitter(data = grid_all, aes(x = vars, y = avg_fh, color = season), alpha = 0.7, shape = 1) + facet_wrap(vars(season), nrow = 1, ncol = 4) + labs(x = xtit, y = "average fishing hours") + theme_gray() + theme(text = element_text(size = 20))
  
}

# front freq
p_sp_box_fhff <- fh_ff_sp_v_ssn(grid_all$front_freq, "front frequency")
p_sp_box_fhff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/fh_vs_ff_cls.png")
ggsave(filename = outputfile, plot = p_sp_box_fhff, width = 2560, height = 2160, unit = "px", dpi = 200, bg = "white")

# front strength
p_sp_box_fhfs <- fh_ff_sp_v_ssn(grid_all$msfront, "front strength")
p_sp_box_fhfs

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/fh_vs_fs_cls.png")
ggsave(filename = outputfile, plot = p_sp_box_fhfs, width = 2560, height = 2160, unit = "px", dpi = 200, bg = "white")

# front persistence
p_sp_box_fhfp <- fh_ff_sp_v_ssn(grid_all$pfront, "front persistence")
p_sp_box_fhfp

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/fh_vs_fp_cls.png")
ggsave(filename = outputfile, plot = p_sp_box_fhfp, width = 2560, height = 2160, unit = "px", dpi = 200, bg = "white")

## Finding centroid for each species
sp_centroid_overlap <- grid_allg %>%
  filter(ol == "Overlapped") %>%
  group_by(scientific_name, season) %>%
  summarise(timespent = mean(total_ts, na.rm = TRUE),
            fishing_hours = mean(avg_fh, na.rm = TRUE),
            .groups = "drop")  # Drop grouping after summarization

# Create the centroid plot by species
p_ct_spp <- ggplot(data = sp_centroid_overlap, aes(y = log10(timespent), x = log10(fishing_hours))) +
  geom_point(data = sp_centroid_overlap, aes(y = log10(timespent), x = log10(fishing_hours), color = scientific_name, shape = season), alpha = 0.8, size = 3) +  # Add centroid points
 facet_wrap(vars(scientific_name)) +
  # geom_text(aes(label = scientific_name), hjust = 0.5, vjust = -0.5, size = 3, color = "darkgray") +  # Add species labels
  labs(
    title = "Centroid of Bird Occurrence vs Fishing Hours by Species",
    y = "Bird-months (log10)",
    x = "Fishing Hours (log10)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )
p_ct_spp

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/centroid_ts_vs_fh_cls_ov_byspp.png")
ggsave(filename = outputfile, plot = p_ct_spp, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# Create the centroid plot by season
p_ct_spp2 <- ggplot(data = sp_centroid_overlap, aes(y = log10(timespent), x = log10(fishing_hours))) +
  geom_point(data = sp_centroid_overlap, aes(y = log10(timespent), x = log10(fishing_hours), color = scientific_name), alpha = 1, size = 3) +  # Add centroid points
  facet_wrap(vars(season)) +
  geom_text(aes(label = scientific_name), hjust = 0.5, vjust = -0.5, size = 3, color = "darkgray") +  # Add species labels
  labs(
    title = "Centroid of Bird Occurrence vs Fishing Hours by Species",
    y = "Bird-months (log10)",
    x = "Fishing Hours (log10)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    axis.title = element_text(size = 20),
    text = element_text(size = 20)
  )
p_ct_spp2

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/centroid_ts_vs_fh_cls_ov_byseason.png")
ggsave(filename = outputfile, plot = p_ct_spp2, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")
