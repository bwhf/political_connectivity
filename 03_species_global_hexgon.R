## Inspecting overlap and patterns with front freq species by species
library(lubridate)
library(tidyverse)
library(sf)
library(dplyr)
library(dggridR)
library(readr)

# import data
grid_sp_ts <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid/global_hexgrid_95km_timespent_byspp_ann.rds")

# match fishing hour and calculate overlap
fh_dat <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid/hexgrid_res8_fishhour.rds")
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

## matching grid cells to species timespent
grid_match <- fuzzyjoin::fuzzy_left_join(grid_sp_ts, fh_sum_cly, by = c("cell"), match_fun = `==`)
grid_match <- grid_match %>% dplyr::select(-cell.y) %>% rename("cell" = "cell.x")

# match front freq 10, 18, 27
# merge overlap with front freq with different thresholds
grid_frfr <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/raster_ann_front_freq_10.rds")
grid_frfr <- grid_frfr %>%
  group_by(cell) %>%
  summarise(frfr = mean(front_freq)) %>%
  rename("frfr10" = "frfr")

# for front freq 10
grid_match_frfr10 <- merge(grid_match, grid_frfr, by.x="cell") # merge overlap and front freq map
grid_match_frfr10$frfr10 <- grid_match_frfr10$frfr

# repeat for front freq 18 and 27
grid_frfr18 <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/raster_ann_front_freq_18.rds")
grid_frfr18 <- grid_frfr18 %>%
  group_by(cell) %>%
  summarise(frfr = mean(front_freq)) %>%
  rename("frfr18" = "frfr")

grid_match_frfr18 <- merge(grid_match_frfr10, grid_frfr18, by.x="cell") # merge overlap and front freq map

# 27
grid_frfr27 <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/raster_ann_front_freq_27.rds")
grid_frfr27 <- grid_frfr27 %>%
  group_by(cell) %>%
  summarise(frfr = mean(front_freq)) %>%
  rename("frfr27" = "frfr")

grid_match_frfr27 <- merge(grid_match_frfr18, grid_frfr27, by.x="cell") # merge overlap and front freq map

grid_match_sp <- grid_match_frfr27 
grid_match_sp$frfr10 <- as.numeric(grid_match_sp$frfr10)
grid_match_sp$frfr18 <- as.numeric(grid_match_sp$frfr18)
grid_match_sp$frfr27 <- as.numeric(grid_match_sp$frfr27)

grid_match_sp$overlap <- grid_match_sp$timespent*grid_match_sp$avg_fh

#write_rds(grid_match_sp, "data/analysis/glob_hexgrid/hexgrid_res8_by_sp_frfr_cly.rds")

## inspecting stats by species
grid_match_sp <- read_rds("data/analysis/glob_hexgrid/hexgrid_res8_by_sp_frfr_cly.rds")

# boxplot by species
## overlap
p_spov_box_ff <- ggplot() + geom_boxplot(data = grid_all, aes(x = front_freq, y = log10(overlap), group = scientific_name)) + geom_jitter(data = grid_all, aes(x = front_freq, y = log10(overlap), color = log10(overlap)), alpha = 0.2, shape = 1) + facet_wrap(vars(scientific_name)) + scale_color_gradient(low = "blue", high = "red") + theme_grey() + theme(text = element_text(size = 20)) 
p_spov_box_ff

p_spov_box_ff18 <- ggplot() + geom_boxplot(data = grid_match_sp, aes(x = frfr18, y = log10(overlap), group = scientific_name), alpha = 0.8) + geom_jitter(data = grid_match_sp, aes(x = frfr18, y = log10(overlap), color = log10(overlap)), alpha = 0.2, shape = 1) + facet_wrap(vars(scientific_name)) + scale_color_gradient(low = "blue", high = "red") + theme_gray() + theme(text = element_text(size = 20))
p_spov_box_ff18

p_spov_box_ff27 <- ggplot() + geom_boxplot(data = grid_match_sp, aes(x = frfr27, y = log10(overlap), group = scientific_name), alpha = 0.8) + geom_jitter(data = grid_match_sp, aes(x = frfr27, y = log10(overlap), color = log10(overlap)), alpha = 0.2, shape = 1) + facet_wrap(vars(scientific_name)) + scale_color_gradient(low = "blue", high = "red") + theme_gray() + theme(text = element_text(size = 20))
p_spov_box_ff27

## timespent
p_spts_box_ff10 <- ggplot() + geom_boxplot(data = grid_match_sp, aes(x = frfr10, y = log10(timespent), group = scientific_name)) + geom_jitter(data = grid_match_sp, aes(x = frfr10, y = log10(timespent), color = log10(timespent)), alpha = 0.2, shape = 1) + facet_wrap(vars(scientific_name)) + scale_color_gradient(low = "blue", high = "red") + theme_grey() + theme(text = element_text(size = 20)) 
p_spts_box_ff10

p_spts_box_ff18 <- ggplot() + geom_boxplot(data = grid_match_sp, aes(x = frfr18, y = log10(timespent), group = scientific_name)) + geom_jitter(data = grid_match_sp, aes(x = frfr18, y = log10(timespent), color = log10(timespent)), alpha = 0.2, shape = 1) + facet_wrap(vars(scientific_name)) + scale_color_gradient(low = "blue", high = "red") + theme_grey() + theme(text = element_text(size = 20)) 
p_spts_box_ff18

p_spts_box_ff27 <- ggplot() + geom_boxplot(data = grid_match_sp, aes(x = frfr27, y = log10(timespent), group = scientific_name)) + geom_jitter(data = grid_match_sp, aes(x = frfr27, y = log10(timespent), color = log10(timespent)), alpha = 0.2, shape = 1) + facet_wrap(vars(scientific_name)) + scale_color_gradient(low = "blue", high = "red") + theme_grey() + theme(text = element_text(size = 20)) 
p_spts_box_ff27


# timespent density plot
p_spts_den_ff10 <- ggplot(data = grid_match_sp, aes(x = frfr10, y = log10(timespent), group = scientific_name)) + geom_density2d(linewidth = 0.25, color = "darkgrey") + geom_density_2d_filled(alpha = 0.5, bins = 4) + facet_wrap(vars(scientific_name)) + theme(text = element_text(size = 20))
p_spts_den_ff10 # need to produce plot by plot

p_spts_den_ff18 <- ggplot(data = grid_match_sp, aes(x = frfr18, y = log10(timespent), group = scientific_name)) + geom_density2d(linewidth = 0.25, color = "darkgrey") + geom_density_2d_filled(alpha = 0.5, bins = 4) + facet_wrap(vars(scientific_name)) + theme(text = element_text(size = 20))
p_spts_den_ff18 # need to produce plot by plot


# try simple GLM
library(mgcv)
glm_ts_frfr10 <- glm(log10(timespent) ~ frfr10 + scientific_name, family = gaussian, data = grid_match_sp)
summary(glm_ts_frfr10)
aov(glm_ts_frfr10)
plot(glm_ts_frfr10)

gam_ts_frfr10 <- gam(log10(timespent) ~ s(frfr10, k = 12), data = grid_match_sp)
summary(gam_ts_frfr10)
plot(gam_ts_frfr10)
