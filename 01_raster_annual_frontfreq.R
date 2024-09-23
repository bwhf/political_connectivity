library(stars)
library(sf)
library(dplyr)
library(dggridR)
library(readr)

tff_ann <- read_ncdf("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/bycatch_seascapes_global_front_frequency_pml_v2.0_scl1_thresh27_All_Data.nc")

# Define the target resolution (1x1 degree)
res <- c(1, 1)  # 1 degree by 1 degree

# Resample the stars object to the target resolution
resampled_stars <- st_warp(tff_ann, cellsize = res, crs = st_crs(tff_ann), dest = tff_ann)

# Convert the resampled stars object into a dataframe
raster_df <- as.data.frame(resampled_stars, xy = TRUE)

# Optionally, rename columns for clarity
colnames(raster_df) <- c("Lon", "Lat", "time", "front_freq")


spatial <- dgconstruct(spacing=100, metric=TRUE, resround='nearest') # spacing unit  CLS (km)

wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing

# assign dggridR seqnum to each location
# hexgrid: get cell name for each fix ~~~~~~~~~~~~~~~~~
raster_df$cell <- dgGEO_to_SEQNUM(spatial, raster_df$Lon, raster_df$Lat)$seqnum
write_rds(raster_df, "/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/raster_ann_front_freq.rds")
