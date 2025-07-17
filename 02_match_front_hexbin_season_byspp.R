## matching pfront (persistence) and msfront (mean strength) to hexgrid dataset
source("/Users/bwhf/Documents/GitHub/political_connectivity/00_function_match_fh_ts_season.R") # call function

grid_all_ts <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_ts_fh_cls.rds")

grid_all_fh <- read_rds("data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_fh_ts_cls.rds")

# Paths and seasons
front_path <- "/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/raster_clm_front_all.rds"
output_base_path <- "/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/"
seasons <- c("Winter", "Spring", "Summer", "Autumn")

# call function to merge both front metrics
final_data_front <- match_front(grid_all, front_path, seasons)
final_data_front <- final_data_front %>%
  dplyr::select(-season.y, -season) %>%
  rename("season" = "season.x")

# save dataframe
write_rds(final_data_front, paste0(output_base_path,"hexgrid_res8_byspp_overlap_ts_fh_cls_all_fronts.rds"))

          