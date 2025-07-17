library(stars)
library(sf)
library(dplyr)
library(dggridR)
library(readr)
library(lubridate)
library(tidyverse)
# Load the NetCDF file

# Step 1: List all 12 NetCDF files
file_list <- list.files(path = "/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/bycatch_seascapes_global_front_frequency_pml_v2.2_strong_thresh/bycatch_seascapes_global_front_frequency_pml_v2.2_climatology_strong18/monthly", pattern = "\\.nc$", full.names = TRUE)

# Step 2: Read all files as a single `stars` object
tff_ann <- do.call(c, lapply(file_list, read_stars))

msfront_ann <- read_ncdf("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/bycatch_seascapes_global_front_means_v1.0_climatology/monthly_climatology.g7.front_step4_sst.UIR.L3_mean.data.nc")

pfront_ann <- read_ncdf("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/bycatch_seascapes_global_front_means_v1.0_climatology/monthly_climatology.g7.front_step4_sst.UIR.L3_pfront.data.nc")

files <- list(tff_ann, msfront_ann, pfront_ann)
files_names <- list("front_freq", "msfront", "pfront")

# Create a target grid with 1x1 degree resolution
target_grid <- st_as_stars(
  st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs = st_crs(4326)),
  dx = 0.5, dy = 0.5  # Grid resolution in degrees
)

# Create a hexagonal grid with appropriate spacing (e.g., 100 km)
spatial <- dgconstruct(spacing = 100, metric = TRUE, resround = 'nearest')
wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing

# convert raster to hexagonal grid 
make_hex <- function(dat, name) { 
  # Resample the stars object to the target grid
  resampled_stars <- st_warp(src = dat, dest = target_grid)
  
  # Convert the resampled stars object into a dataframe
  raster_df <- as.data.frame(resampled_stars, xy = TRUE)
  
  # Rename columns for clarity
  colnames(raster_df) <- c("Lon", "Lat", "time", name)
  
  # Assign dggridR sequence numbers to each grid cell
  raster_df$cell <- dgGEO_to_SEQNUM(spatial, raster_df$Lon, raster_df$Lat)$seqnum

  raster_df1 <- raster_df %>%
    mutate(date = as.Date(time)) %>%
    mutate(month = month(date))
  
  raster_df1 <- raster_df1 %>%
    mutate(
      season = case_when(
        month %in% c(12, 1, 2) ~ "Summer",
        month %in% c(3, 4, 5) ~ "Autumn",
        month %in% c(6, 7, 8) ~ "Winter",
        month %in% c(9, 10, 11) ~ "Spring",
        TRUE ~ NA_character_
      ),
      # Adjust seasons for Northern Hemisphere (Lat > 0)
      season = case_when(
        Lat > 0 & season == "Winter" ~ "Summer",
        Lat > 0 & season == "Spring" ~ "Autumn",
        Lat > 0 & season == "Summer" ~ "Winter",
        Lat > 0 & season == "Autumn" ~ "Spring",
        TRUE ~ season  # Keep unchanged for Southern Hemisphere
      )
    )
  
  raster_df1 <- raster_df1 %>%
    mutate(season = fct_relevel(season, "Spring", "Summer", "Autumn", "Winter"))
  
  # Save the resulting dataframe as an RDS file
  write_rds(raster_df1, paste0("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_clm_", name, ".rds"))
  
  }

# run function to each data set
front_freq <- make_hex(files[[1]], files_names[[1]])
msfront <- make_hex(files[[2]], files_names[[2]])
pfront <- make_hex(files[[3]], files_names[[3]])


# pfront <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_clm_pfront.rds")
# msfront <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_clm_msfront.rds")

front_dat <- bind_cols(pfront, msfront$msfront)
front_dat <- front_dat %>%
  rename(msfront = "...9")

front_dat <- bind_cols(front_dat, front_freq$front_freq)
front_dat <- front_dat %>%
  rename(front_freq = "...10") %>%
  mutate(front_freq = as.numeric(front_freq)) %>%
  select(cell, Lon, Lat, date, time, month, season, front_freq, msfront, pfront) %>%
  arrange(cell, Lon, Lat, month, season)

# Save the resulting dataframe as an RDS file
write_rds(front_dat, paste0("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_clm_front_all.rds"))

# group by season
front_dat_cls <- front_dat %>%
  group_by(cell, season) %>%
  summarise(front_freq = mean(front_freq), msfront = mean(msfront), pfront = mean(pfront))

# Save the resulting dataframe as an RDS file
write_rds(front_dat_cls, paste0("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_cls_front_all.rds"))

## matching ocean region
ocean_region <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/Ocean_regions/dggrid_IHO_ocean_regions.rds") # IHO oceans

all_front <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_cls_front_all.rds")

# Step 2: Merge ocean names using DGGRID cell ID
all_front <- all_front %>%
  left_join(ocean_region, by = "cell")  # Matches by cell number

# Assign each sea to one of 7 major oceans
all_front <- all_front %>%
  mutate(ocean = case_when(
    ocean_name %in% c("Bering Sea", "Chukchi Sea", "North Pacific Ocean", "Gulf of Alaska", 
                      "The Coastal Waters of Southeast Alaska and British Columbia",
                      "Gulf of California", "Philippine Sea", "Japan Sea", "Sea of Okhotsk", 
                      "Eastern China Sea", "South China Sea", "Yellow Sea", "Seto Naikai or Inland Sea") ~ "North Pacific Ocean",
    
    ocean_name %in% c("South Pacific Ocean", "Tasman Sea", "Coral Sea", "Bismarck Sea", "Solomon Sea") ~ "South Pacific Ocean",
    
    ocean_name %in% c("North Atlantic Ocean", "Gulf of St. Lawrence", "Caribbean Sea", "Gulf of Mexico", 
                      "Bay of Fundy", "Inner Seas off the West Coast of Scotland", "Irish Sea and St. George's Channel",
                      "Celtic Sea", "Bristol Channel", "English Channel", "Bay of Biscay") ~ "North Atlantic Ocean",
    
    ocean_name %in% c("South Atlantic Ocean", "Gulf of Guinea", "Rio de La Plata") ~ "South Atlantic Ocean",
    
    ocean_name %in% c("Adriatic Sea", "Mediterranean Sea - Western Basin", "Balearic (Iberian Sea)", 
                      "Ligurian Sea", "Tyrrhenian Sea", "Mediterranean Sea - Eastern Basin", 
                      "Ionian Sea", "Aegean Sea", "Alboran Sea", "Black Sea", "Sea of Marmara") ~ "Mediterranean Sea",
    
    ocean_name %in% c("Indian Ocean", "Mozambique Channel", "Red Sea", "Gulf of Suez", 
                      "Gulf of Aqaba", "Gulf of Aden", "Arabian Sea", "Persian Gulf", "Gulf of Oman", 
                      "Laccadive Sea", "Bay of Bengal", "Andaman or Burma Sea", "Malacca Strait", "Gulf of Thailand",
                      "Java Sea", "Bali Sea", "Makassar Strait", "Celebes Sea", "Sulu Sea", "Flores Sea",
                      "Savu Sea", "Gulf of Boni", "Banda Sea", "Gulf of Tomini", "Timor Sea", 
                      "Molukka Sea", "Ceram Sea", "Halmahera Sea", "Arafura Sea") ~ "Indian Ocean",
    
    ocean_name %in% c("Southern Ocean", "Great Australian Bight", "Bass Strait", "Barentsz Sea", "Laptev Sea",
                      "East Siberian Sea", "White Sea", "Kara Sea", "Beaufort Sea", "Arctic Ocean", "Greenland Sea",
                      "Norwegian Sea", "Lincoln Sea", "Baffin Bay", "The Northwestern Passages", "Davis Strait",
                      "Hudson Bay", "Hudson Strait", "Labrador Sea", "Gulf of Bothnia", "Gulf of Riga", "Gulf of Finland",
                      "Sea of Azov", "Skagerrak", "Kattegat", "Baltic Sea") ~ "Southern & Arctic Ocean",
    
    TRUE ~ NA_character_  # Keep NA if unknown
  ))

all_front <- all_front %>%
  mutate(season = fct_relevel(season, "Spring", "Summer", "Autumn", "Winter"))

# Save the resulting dataframe as an RDS file
write_rds(all_front, paste0("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_cls_front_all.rds"))
