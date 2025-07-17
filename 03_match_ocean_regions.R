## matching ocean regions using shapefiles
library(sf)
library(terra)
library(dplyr)

# Step 1: Load the ocean shapefile and ensure it's valid
ocean_shapes <- st_read("/Users/bwhf/Documents/PhD/USC/OceanProducts/Ocean_regions/goas_v01.shp")  # Replace with actual ocean shapefile
ocean_shapes <- st_read("/Users/bwhf/Documents/PhD/USC/OceanProducts/Ocean_regions/World_Seas_IHO_v3/World_Seas_IHO_v3.shp") # world seas IHO file
ocean_shapes <- st_transform(ocean_shapes, 4326)  # Ensure CRS is WGS84
ocean_shapes <- st_make_valid(ocean_shapes)  # Fix invalid geometries

# Step 2: Create an empty raster (1° x 1° resolution)
r <- rast(ext(ocean_shapes), resolution = c(0.1, 0.1), crs = "EPSG:4326")

# Step 3: Rasterize the ocean polygons (assign ocean names to grid cells)
r <- rasterize(ocean_shapes, r, field = "NAME")  # "name" contains ocean region names

# Step 4: Convert raster to dataframe
raster_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
colnames(raster_df) <- c("lon", "lat", "ocean_name")

# View raster data
head(raster_df)

library(dggridR)

# Step 1: Construct DGGRID system (ensure it matches your dataset)
spatial_grid <- dgconstruct(spacing = 100, metric = TRUE, resround = 'nearest')  # Adjust based on your resolution

# Step 2: Compute DGGRID cell for each ocean raster point
raster_df$cell <- dgGEO_to_SEQNUM(spatial_grid, raster_df$lon, raster_df$lat)$seqnum

# Step 3: Remove duplicates (each cell should have one ocean name)
raster_df_dg <- raster_df %>%
  group_by(cell) %>%
  summarise(ocean_name = first(ocean_name))  # Keeps only the first ocean name per cell

# Alternative method: Remove duplicates (more general)
raster_df_dg <- raster_df_dg %>%
  distinct(cell, .keep_all = TRUE)  # Keeps only one row per cell

# View DGGRID-mapped ocean data
head(raster_df_dg)
raster_ocean <- raster_df_dg %>% distinct(ocean_name)

# Assign each sea to one of 7 major oceans
raster_df_dg <- raster_df_dg %>%
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

write_rds(raster_df_dg, "/Users/bwhf/Documents/PhD/USC/OceanProducts/Ocean_regions/dggrid_IHO_ocean_regions.rds")
