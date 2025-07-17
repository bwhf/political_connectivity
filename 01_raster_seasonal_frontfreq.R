library(stars)
library(sf)
library(dplyr)
library(dggridR)
library(readr)

# Define the file path and seasonal file names
base_path <- "//Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/bycatch_seascapes_global_front_frequency_pml_v2.2_strong_thresh/bycatch_seascapes_global_front_frequency_pml_v2.2_climatology_strong18/"
file_template <- "bycatch_seascapes_global_front_frequency_pml_v2.0_scl1_thresh18_All_Season_"
seasons <- c("Winter", "Spring", "Summer", "Autumn")
file_extension <- ".nc"

# Define the target resolution (1x1 degree)
res <- c(1, 1)  # 1 degree by 1 degree

# Initialize an empty list to store the processed dataframes
seasonal_data <- list()

# Loop through each season and process the corresponding file
for (season in seasons) {
  # Construct the full file path
  file_path <- paste0(base_path, file_template, season, file_extension)
  
  # Read the NetCDF file
  tff_season <- read_ncdf(file_path)
  
  # Resample the stars object to the target resolution
  resampled_stars <- st_warp(tff_season, cellsize = res, crs = st_crs(tff_season), dest = tff_season)
  
  # Convert the resampled stars object into a dataframe
  raster_df <- as.data.frame(resampled_stars, xy = TRUE)
  
  # Optionally, rename columns for clarity
  colnames(raster_df) <- c("Lon", "Lat", "time", "front_freq")
  
  # Assign hex grid cell IDs
  spatial <- dgconstruct(spacing = 100, metric = TRUE, resround = 'nearest') # 100 km spacing
  raster_df$cell <- dgGEO_to_SEQNUM(spatial, raster_df$Lon, raster_df$Lat)$seqnum
  
  # Save to the list
  seasonal_data[[season]] <- raster_df
  
  # Write the processed data to an RDS file
  output_path <- paste0(base_path, "raster_", season, "_front_freq.rds")
  write_rds(raster_df, output_path)
  return(seasonal_data)
}

# Optionally, combine all seasonal data into one dataframe
all_seasons_df <- bind_rows(seasonal_data, .id = "season")

# Save the combined dataframe
write_rds(all_seasons_df, paste0(base_path, "raster_all_seasons_front_freq.rds"))
