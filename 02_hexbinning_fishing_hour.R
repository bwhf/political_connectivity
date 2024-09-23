## fish_hour_hexbinning 
library(lubridate)
library(tidyverse)

# import fh data
fh_format <- read_csv("data/fh_month/fishhour_month/fh2014_2018_longlines_res01.csv") # AIS data world wide during 2014 to 2018
head(fh_format)

# correct Time_range column
fh_date <- fh_format %>% 
  mutate(Time_Range = as.Date(paste0(Time_Range, "-01")))

# create hex grid
spatial <- dgconstruct(spacing=100, metric=TRUE, resround='nearest') # spacing unit  CLS (km)
wgs84 <- sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # WGS for spatializing

# assign dggridR seqnum to each location
# hexgrid: get cell name for each fix ~~~~~~~~~~~~~~~~~
fh_date$cell <- dgGEO_to_SEQNUM(spatial, fh_date$Lon, fh_date$Lat)$seqnum

# create month, season, year column
fh_date$month <- month(fh_date$Time_Range)
fh_dat <- fh_date %>%
  mutate(
    season = case_when(
      month %in% c("12", "1", "2") ~ "Summer",
      month %in% c("3", "4", "5") ~ "Autumn",
      month %in% c("6", "7", "8") ~ "Winter",
      month %in% c("9", "10", "11") ~ "Spring",
      TRUE ~ NA_character_
    )
  )

write_rds(fh_dat, "/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid/hexgrid_res8_fishhour.rds")

# summarising fh
##### 
# summarise per month and year
fh_sum_mth <- fh_dat %>%
  group_by(cell, Time_Range, year, month, season) %>%
  summarise(total_fh = sum(Apparent_Fishing_Hours),
            n_vessel = n())

# summarise per month across year
fh_sum_clm <- fh_sum_mth %>%
  group_by(cell, month) %>%
  summarise(n_year = n(),
            avg_fh = mean(total_fh),
            median_fh = median(total_fh),
            sd_fh = sd(total_fh),
            se_fh = sd_fh/sqrt(n_year),
            sum_vessel = sum(n_vessel),
            avg_vessel = mean(n_vessel))

# get grid geometry for each cell
grid_fh_clm <- dgcellstogrid(spatial, cells = as.numeric(fh_sum_clm$cell)) # get only cells which contained fixes
grid_fh_clm <- grid_fh_clm %>% rename("cell" = "seqnum")
grid_fh_clm <- merge(grid_fh_clm, fh_sum_clm, by.x="cell")

write_rds(grid_fh_clm, "data/fh_month/fishhour_month/hexgrid_res8_fh_clm.rds")
# grid_fh_clm <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_clm.rds")

# summarise per season per year
fh_sum_ssn <- fh_dat %>%
  group_by(cell, year, season) %>%
  summarise(total_fh = sum(Apparent_Fishing_Hours),
            n_vessel = n())

# summarise per season across year
fh_sum_cls <- fh_sum_ssn %>%
  group_by(cell, season) %>%
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

write_rds(grid_fh_cls, "data/fh_month/fishhour_month/hexgrid_res8_fh_cls.rds")
# grid_fh_cls <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_cls.rds")

# summarise per year
fh_sum_yr <- fh_dat %>%
  group_by(cell, year) %>%
  summarise(total_fh = sum(Apparent_Fishing_Hours),
            n_vessel = n())

# summarise average across years
fh_sum_cly <- fh_sum_yr %>%
  group_by(cell) %>%
  summarise(n_year = n(),
            avg_fh = mean(total_fh),
            median_fh = median(total_fh),
            sd_fh = sd(total_fh),
            se_fh = sd_fh/sqrt(n_year),
            sum_vessel = sum(n_vessel),
            avg_vessel = mean(n_vessel))
 
# get grid geometry for each cell
grid_fh_cly <- dgcellstogrid(spatial, cells = as.numeric(fh_sum_cly$cell)) # get only cells which contained fixes
grid_fh_cly <- grid_fh_cly %>% rename("cell" = "seqnum")
grid_fh_cly <- merge(grid_fh_cly, fh_sum_cly, by.x="cell")

write_rds(grid_fh_cly, "data/fh_month/fishhour_month/hexgrid_res8_fh_cly.rds")
# grid_fh_cly <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_cly.rds")

# merge fh with timespent
grid_all <- read_rds("data/analysis/glob_hexgrid/global_hexgrid_95km_timespent_allspp_ann.rds")
grid_fh_ts <- merge(fh_sum_cly, grid_all, by.x="cell") # merge fh and bird timespent
write_rds(grid_fh_ts, "data/analysis/glob_hexgrid//hexgrid_res8_fh_ts_cly.rds")

grid_overlap <- grid_fh_ts %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")
grid_overlap$overlap <- as.numeric(grid_overlap$overlap)
# write_rds(grid_overlap, "data/analysis/glob_hexgrid/hexgrid_res8_overlap_cly.rds")

# merge overlap with front freq
grid_frfr <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/raster_ann_front_freq_10.rds")
grid_frfr <- grid_frfr %>%
  group_by(cell) %>%
  summarise(frfr = mean(front_freq))

grid_var <- merge(grid_fh_ts, grid_frfr, by.x="cell") # merge overlap and front freq map

# recenter 
grid_var <- grid_var %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")
grid_var$frfr <- as.numeric(grid_var$frfr)
# write_rds(grid_var, "data/analysis/glob_hexgrid/hexgrid_res8_frfr10_cly.rds")
