# Load necessary libraries
library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(plotly)

# create a function that identify the cells that cross 360 lon
find_cross <- function(hex_grid) {
  # Apply st_bbox to each geometry to get the bounding box for each polygon
  bbox_grid <- hex_grid %>%
    rowwise() %>%
    mutate(bbox = list(st_bbox(geometry))) %>%
    ungroup()
  
  # Now extract the xmin and xmax from the bbox for each polygon
  bbox_grid <- bbox_grid %>%
    mutate(xmin = map_dbl(bbox, ~ .["xmin"]),
           xmax = map_dbl(bbox, ~ .["xmax"]))
  
  # Filter the polygons that cross the 360-degree longitude boundary
  crossing_cells <- bbox_grid %>%
    filter(xmin < 360 & xmax > 360)
  
  # Inspect the filtered grid cells
  print(crossing_cells)
  
  return(crossing_cells)
}

# create function to replace the original polygons that crossed 360
replace_cross <- function(hex_grid) {
  no_cross_grid <- hex_grid %>%
    filter(!(cell %in% crossing_cells$cell)) # Remove the original crossing cells
  return(no_cross_grid)
}

# Create a function to adjust longitudes exceeding 360 for a polygon
adjust_longitude <- function(geometry) {
  coords <- st_coordinates(geometry)
  
  # Adjust longitudes where they exceed 360 by subtracting 360
  coords[, "X"] <- ifelse(coords[, "X"] > 360, coords[, "X"] - 360, coords[, "X"])
  
  # Reconstruct the polygon with the corrected coordinates
  # st_polygon expects a list of coordinates for each ring of the polygon
  st_polygon(list(coords[, 1:2]))  # We drop Z and M dimensions if present
}

# Create function to apply the adjustment and wrapping to each geometry
wrap_360plus <- function(new_hex_grid) {
  wrap_hex_grid <- new_hex_grid %>%
    mutate(geometry = st_sfc(lapply(new_hex_grid$geometry, adjust_longitude), crs = st_crs(new_hex_grid)))
  
  return(wrap_hex_grid)
}

# Function to adjust longitude for Atlantic Ocean visualization
adjust_atlantic_longitude <- function(geometry, ocean) {
  if (ocean %in% c("North Atlantic", "South Atlantic")) {
    # Convert geometry: If longitude > 180, subtract 360 (e.g., 350 -> -10)
    adjusted_geom <- st_set_geometry(geometry, st_transform(geometry, crs = "+proj=longlat +datum=WGS84"))
    adjusted_geom <- st_shift_longitude(adjusted_geom)  # Shift coordinates from 0-360 to -180-180
    return(adjusted_geom)
  } else {
    return(geometry)  # Keep other oceans unchanged
  }
}

# load world map
library(maps)
library(mapproj)
world <- spData::world
myworld1 = maps::map(wrap=c(0,360), plot=FALSE, fill=TRUE)
# Convert the map data to a dataframe for ggplot2 plotting
map_df1 <- map_data(myworld1)

# plot it out 
plot_world <- function(dat, var, tit) {
  
  p <- ggplot() +
    geom_sf(data = dat, aes(fill = log10(var)), color = "black") +
    scale_fill_viridis_c() +
    theme_minimal() +
    ggtitle(paste0(tit)) +
    geom_polygon(data = map_df, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey") +
    geom_sf(data = eez_boundaries)
  
  return(p)
}

# plot it out 
plot_world_cls <- function(dat, var, tit) {
  
  p <- ggplot() +
    geom_sf(data = dat, aes(fill = log10(var)), color = "NA") +
    facet_wrap(vars(season)) +
    scale_fill_viridis_c() +
    theme_minimal() +
    ggtitle(paste0(tit)) +
    geom_polygon(data = map_df, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey", alpha = 0.5) +
    geom_sf(data = eez_boundaries, color = "grey", alpha = 0.7)
  
  return(p)
}

# interactive population map 
plot_world_fronts <- function(dat, var, tit) {
  
  p <- ggplot() +
    geom_sf(data = dat, aes(fill = {{var}}), color = NA) +
    facet_wrap(vars(season)) +
    theme_minimal() +
    ggtitle(tit) +
    geom_polygon(data = map_df, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey", alpha = 0.5) +
    geom_sf(data = eez_boundaries, color = "grey", alpha = 0.7) + 
    theme(text = element_text(size = 16)) 
  return(p)
}

plot_world_spp_pop <- function(dat, var, tit) {
  
  p <- ggplot() +
    geom_sf(data = dat, aes(fill = scientific_name, shape = ol), color = "NA") +
    theme_minimal() +
    ggtitle(paste0(tit)) +
    geom_polygon(data = map_df, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey", alpha = 0.5) +
    geom_sf(data = eez_boundaries, color = "grey", alpha = 0.7)

    # Convert the ggplot to a plotly object for interactivity
  interactive_plot <- ggplotly(p, tooltip = "text")
  
  return(interactive_plot)
}

# interactive population by ocean region
plot_ocean_spp_pop <- function(dat, region, tit, map) {
  p_ocean_spp <- ggplot(data = dat %>% filter(ocean == region)) +
    geom_sf(aes(fill = scientific_name)) +
    labs(title = tit) +
    geom_polygon(data = map, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey", alpha = 0.5) +
    geom_sf(data = eez_boundaries, color = "grey", alpha = 0.7) +
    theme_minimal()
  
  # Convert the ggplot to a plotly object for interactivity
  pint_npac_spp <- ggplotly(p_ocean_spp, tooltip = "text")
  return(pint_npac_spp)
}

plot_world_spp_ov <- function(dat, var, tit) {
  
  p <- ggplot() +
    geom_sf(data = dat, aes(fill = var, shape = ol), color = "NA") +
    theme_minimal() +
    ggtitle(paste0(tit)) +
    geom_polygon(data = map_df, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey", alpha = 0.5) +
    geom_sf(data = eez_boundaries, color = "grey", alpha = 0.7)
  
  # Convert the ggplot to a plotly object for interactivity
  interactive_plot <- ggplotly(p, tooltip = "text")
  
  return(interactive_plot)
}




# Optionally visualize the polygons that cross the 360 boundary
ggplot() +
  geom_sf(data = crossing_cells, fill = "red", color = "black") +
  theme_minimal() +
  ggtitle("Polygons Crossing the 360-degree Longitude Boundary")

# Plot to verify the result
p3 <- ggplot() +
  geom_sf(data = recent_fh_grid_new, aes(fill = log10(avg_fh)), color = "black") +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle(paste0("Annual total fishing hour")) +
  geom_polygon(data = map_df1, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey") 

p3 # need to adjust grids starting from >360 to 0

# Plot to verify the result
p4 <- ggplot() +
  geom_sf(data = recent_fh_grid_new2, aes(fill = log10(avg_fh)), color = "black") +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle(paste0("Annual total fishing hour")) +
  geom_polygon(data = map_df1, aes(x=long, y = lat, group=group), fill = "grey", colour = "grey") 

p4 # perfecto!
