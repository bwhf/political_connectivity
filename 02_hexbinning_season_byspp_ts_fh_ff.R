## fish_hour_hexbinning 
library(lubridate)
library(tidyverse)
library(sf)
library(dplyr)
library(dggridR)
library(readr)

## codes to run for later part
# create hex grid
spatial <- dgconstruct(spacing=100, metric=TRUE, resround='nearest') # spacing unit  CLS (km)
wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing

fh_dat <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid/hexgrid_res8_fishhour.rds")

# assign north hemispheric season
fh_dat <- fh_dat %>%
  mutate(mths = case_when(
    season == "Spring" ~ "SON",
    season == "Summer" ~ "DJF",
    season == "Autumn" ~ "MAM",
    season == "Winter" ~ "JJA"
  )) %>%
  mutate(season = case_when(
    Lat > 0 & season == "Winter" ~ "Summer",
    Lat > 0 & season == "Spring" ~ "Autumn",
    Lat > 0 & season == "Summer" ~ "Winter",
    Lat > 0 & season == "Autumn" ~ "Spring",
    TRUE ~ season  # Keep unchanged for Southern Hemisphere
  ))

# summarising fh
##### 
# summarise per season
fh_sum_ssn <- fh_dat %>%
  group_by(cell, Time_Range, year, season, mths) %>%
  summarise(total_fh = sum(Apparent_Fishing_Hours),
            n_vessel = n())

# summarise per month across year
fh_sum_cls <- fh_sum_ssn %>%
  group_by(cell, season, mths) %>%
  summarise(n_year = n(),
            avg_fh = mean(total_fh),
            median_fh = median(total_fh),
            sd_fh = sd(total_fh),
            se_fh = sd_fh/sqrt(n_year),
            sum_vessel = sum(n_vessel),
            avg_vessel = mean(n_vessel))

# get grid geometry for each cell
grid_fh_cls <- dgcellstogrid(spatial, cells = as.numeric(fh_sum_cls$cell)) # get only cells which contained fixes
grid_fh_cls <- grid_fh_cls %>% rename("cell" = "seqnum")
grid_fh_cls <- merge(grid_fh_cls, fh_sum_cls, by.x="cell")

write_rds(grid_fh_cls, "data/fh_month/fishhour_month/hexgrid_res8_fh_cls_mthsname.rds") # nth/sth season and mth names
# grid_fh_cls <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_cls.rds")

# merge fh with timespent
grid_all <- read_rds("data/analysis/glob_hexgrid_dawn/global_hexgrid_95km_timespent_byspp_mth.rds")

# look for centroid and lat
grid_all1 <- grid_all %>%
  mutate(centroid = st_centroid(geometry),  # Get centroid of each polygon
         Lat = st_coordinates(centroid)[, 2])  # Extract latitude (Y-coordinate)

# summarise by season
grid_all1 <- grid_all1 %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Summer",
      month %in% c(3, 4, 5) ~ "Autumn",
      month %in% c(6, 7, 8) ~ "Winter",
      month %in% c(9, 10, 11) ~ "Spring",
      TRUE ~ NA_character_
    ),
    mths = case_when(
      season == "Spring" ~ "SON",
      season == "Summer" ~ "DJF",
      season == "Autumn" ~ "MAM",
      season == "Winter" ~ "JJA"
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

grid_ssn <- grid_all1 %>%
  dplyr::select(-centroid, -Lat) %>%
  group_by(cell, scientific_name, season, mths) %>%
  summarise(total_ts = sum(timespent),
            avg_ts = mean(timespent),
            n = n())

write_rds(grid_ssn, "data/analysis/glob_hexgrid_dawn/global_hexgrid_95km_timespent_byspp_ssn_mthname.rds") # nth/sth season and mth names

# combine with fishing hours
# combine season by season
source("/Users/bwhf/Documents/GitHub/political_connectivity/00_function_match_ts_fh_season.R") # call function

grid_ssn <- read_rds("data/analysis/glob_hexgrid_dawn/global_hexgrid_95km_timespent_byspp_ssn_mthname.rds")

# grid only with presence of birds
grid_ts_fh <- process_all_seasons(grid_ssn, fh_sum_cls) 
grid_ts_fh <- grid_ts_fh %>% dplyr::select(-cell.y, -season.y, -mths.y) %>% rename("cell" = "cell.x", "season" = "season.x", "mths" = "mths.x")
grid_ts_fh <- grid_ts_fh %>% 
  mutate(overlap = total_ts*avg_fh)

# merge with front frequency
# Paths and seasons
# front_freq_base_path <- "//Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/bycatch_seascapes_global_front_frequency_pml_v2.2_strong_thresh/bycatch_seascapes_global_front_frequency_pml_v2.2_climatology_strong18/"
output_base_path <- "data/analysis/glob_hexgrid_dawn/"
seasons <- c("Winter", "Spring", "Summer", "Autumn")

front_cls <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_cls_front_all.rds") # already matched nth/sth seasons

# call function to merge front frequency
final_data <- match_fronts(grid_ts_fh, front_cls, output_base_path, seasons)
final_data <- final_data %>%
  ungroup() %>%
  dplyr::select(-season.x, -season.y, -cell.y) %>%
  rename(cell = cell.x)

# Save the final combined dataframe
write_rds(final_data, "data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_ts_fh_cls_mths.rds") # nth/sth season and mth names

# grid with fishing effort 
grid_fh_cls <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_cls_mthsname.rds")

source("/Users/bwhf/Documents/GitHub/political_connectivity/00_function_match_fh_ts_season.R") # call function
grid_fh_ts <- process_all_seasons_fhts(grid_ssn, fh_sum_cls) 
grid_fh_ts <- grid_fh_ts %>% dplyr::select(-cell.y, -season.y, -mths.y) %>% rename("cell" = "cell.x", "season" = "season.x", "mths" = "mths.x")

grid_fh_ts <- grid_fh_ts %>% 
  mutate(overlap = total_ts*avg_fh)

# Paths and seasons
# call function to merge front frequency
final_data <- match_fronts(grid_fh_ts, front_cls, output_base_path, seasons)
final_data <- final_data %>%
  ungroup() %>%
  dplyr::select(-season.x, - season.y, -cell.y) %>%
  rename(cell = cell.x)

# Save the final combined dataframe
write_rds(final_data, "data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_fh_ts_cls_mths.rds") # nth/sth season and mth names

