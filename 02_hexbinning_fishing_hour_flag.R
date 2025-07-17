## fish_hour_hexbinning by flags
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

# import fh data
fh_format <- read_csv("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month_flag/fh2014_2018_longlines_res01.csv") # AIS data world wide during 2014 to 2018
head(fh_format)

# correct Time_range column
fh_date <- fh_format %>% 
  mutate(Time_Range = as.Date(paste0(Time_Range, "-01")))
head(fh_date) # check restult

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

write_rds(fh_dat, "/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid/hexgrid_res8_fishhour_flag.rds")
write_rds(fh_dat, "/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month_flag/hexgrid_res8_fishhour_flag.rds")


# summarising fh
##### 
# summarise per month and year
fh_sum_mth <- fh_dat %>%
  group_by(cell, Time_Range, Flag, year, month, season) %>%
  summarise(total_fh = sum(Apparent_Fishing_Hours),
            n_vessel = n())

# summarise per month across year
fh_sum_clm <- fh_sum_mth %>%
  group_by(cell, month, Flag) %>%
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

write_rds(grid_fh_clm, "data/fh_month_flag/hexgrid_res8_fh_flag_clm.rds")
# grid_fh_clm <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_clm.rds")

# summarise per season per year
fh_sum_ssn <- fh_dat %>%
  group_by(cell, year, Flag, season) %>%
  summarise(total_fh = sum(Apparent_Fishing_Hours),
            n_vessel = n())

# summarise per season across year
fh_sum_cls <- fh_sum_ssn %>%
  group_by(cell, Flag, season) %>%
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

write_rds(grid_fh_cls, "data/fh_month_flag/hexgrid_res8_fh_flag_cls.rds")
# grid_fh_cls <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_cls.rds")

# summarise per year
# fh_dat <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid/hexgrid_res8_fishhour.rds")
fh_sum_yr <- fh_dat %>%
  group_by(cell, year, Flag) %>%
  summarise(total_fh = sum(Apparent_Fishing_Hours),
            n_vessel = n())

# summarise average across years
fh_sum_cly <- fh_sum_yr %>%
  group_by(cell, Flag) %>%
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

fh_grid <- grid_fh_cly
fh_grid <- fh_grid %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")

write_rds(grid_fh_cly, "data/fh_month_flag/hexgrid_res8_fh_flag_cly.rds")
# grid_fh_cly <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/fh_month/fishhour_month/hexgrid_res8_fh_cly.rds")

## match annual average fishing hours by flag with all species timespent
## investigate overlap by countries
# merge fh with timespent
grid_all <- read_rds("data/analysis/glob_hexgrid/global_hexgrid_95km_timespent_allspp_ann.rds")
grid_fh_ts <- merge(fh_sum_cly, grid_all, by.x="cell") # merge fh and bird timespent, only overlap grids
# write_rds(grid_fh_ts, "data/analysis/glob_hexgrid//hexgrid_res8_fh_flag_ts_cly.rds")

grid_overlap <- grid_fh_ts %>%
  mutate(overlap = median_fh*timespent) %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")
grid_overlap$overlap <- as.numeric(grid_overlap$overlap)
# write_rds(grid_overlap, "data/analysis/glob_hexgrid/hexgrid_res8_overlap_by_flag_allspp_cly.rds")

# merge timespent with fishing hours by flag, include all timespent (i.e., with or without fh)
grid_ts_fh <- fuzzyjoin::fuzzy_left_join(x = grid_all, y = fh_sum_cly, by = c("cell"), match_fun = `==`)
grid_ts_fh <- grid_ts_fh %>% dplyr::select(-cell.y) %>% rename("cell" = "cell.x")

## PLOTS
# which countries overlap the most with all species?
# Distribution of overlap: boxplot of overlap against flag

# Filter to keep only the top 15 Flags by the sum of overlap
top_flags <- grid_overlap %>%
  group_by(Flag) %>%
  summarise(total_overlap = sum(overlap, na.rm = TRUE)) %>%
  slice_max(total_overlap, n = 15) %>%
  pull(Flag)

# Filter the original data to keep only rows with these top 12 Flags
grid_overlap_15 <- grid_overlap %>%
  filter(Flag %in% top_flags)

p_bp_flag_over <- ggplot(data = grid_overlap_15, aes(x = reorder(Flag, -overlap, FUN = sum), y = log10(overlap), fill = Flag)) + geom_boxplot() + labs(x = "Flag", y = "Log10(Overlap)") + ggtitle("Boxplot of overlap estimates arranged by Sum of Overlap in descending order")
p_bp_flag_over

# total estimation of overlap: bar chart
p_bc_flag_over <- ggplot(data = grid_overlap_15 %>% arrange(Flag, -overlap), aes(x = reorder(Flag, -overlap, FUN = sum), y = overlap, fill = overlap)) + geom_col(linewidth = 0.5)
p_bc_flag_over +
  labs(x = "Flag", y = "Overlap") +
  ggtitle("Top 15 Flags by Sum of Overlap in descending order")

# summary of the top 15 flags overlap

# Calculate median, quartiles, standard deviation, and standard error
summary_overlap_15 <- grid_overlap_15 %>%
  group_by(Flag) %>%
  summarize(
    total = sum(overlap),
    median = median(overlap),
    Q1 = quantile(overlap, 0.25),
    Q3 = quantile(overlap, 0.75),
    sd = sd(overlap),
    se = sd(overlap) / sqrt(n()),  # Standard Error
    .groups = "drop"
  ) %>%
  arrange(total)

# Print the summary statistics
print(summary_overlap_15)

########
# merge overlap with front freq with different thresholds
grid_frfr <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/raster_ann_front_freq_10.rds")
grid_frfr <- grid_frfr %>%
  group_by(cell) %>%
  summarise(frfr = mean(front_freq))

grid_var10 <- merge(grid_ts_fh, grid_frfr, by.x="cell") # merge overlap and front freq map

grid_var10$frfr10 <- grid_var10$frfr

# repeat line 138 with other threshold 
grid_var18 <- merge(grid_ts_fh, grid_frfr, by.x="cell") # merge overlap and front freq map
grid_var18$frfr18 <- grid_var18$frfr
grid_var18 <- grid_var18 %>% dplyr::select(cell, n_year, frfr18)

grid_var27 <- merge(grid_ts_fh, grid_frfr, by.x="cell") # merge overlap and front freq map
grid_var27$frfr27 <- grid_var27$frfr
grid_var27 <- grid_var27 %>% dplyr::select(cell, n_year, frfr27)

grid_var <- fuzzyjoin::fuzzy_left_join(grid_var10, grid_var18, by = c("cell"), match_fun = `==`)
grid_var <- grid_var %>% dplyr::select(-cell.y, -n_year.y) %>% rename("cell" = "cell.x", "n_year" = "n_year.x")

grid_var <- fuzzyjoin::fuzzy_left_join(grid_var, grid_var27, by = c("cell"), match_fun = `==`)
grid_var <- grid_var %>% dplyr::select(-cell.y, -n_year.y, -frfr) %>% rename("cell" = "cell.x", "n_year" = "n_year.x")

# recenter 
grid_var <- grid_var %>%
  st_as_sf() %>%
  st_set_crs(st_crs(fh_grid)) %>%
  st_cast("POLYGON")
grid_var$frfr10 <- as.numeric(grid_var$frfr10)
grid_var$frfr18 <- as.numeric(grid_var$frfr18)
grid_var$frfr27 <- as.numeric(grid_var$frfr27)

write_rds(grid_var, "data/analysis/glob_hexgrid/hexgrid_res8_frfr_allts_cly.rds")
