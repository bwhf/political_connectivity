# Call function for plots
source("/Users/bwhf/Documents/GitHub/political_connectivity/scripts/final/source_fxns/density_plot_by_ocean.R")

# load world map
library(maps)
library(mapproj)
world <- spData::world

myworld = maps::map(wrap=c(0,360), plot=FALSE, fill=TRUE)
# Convert the map data to a dataframe for ggplot2 plotting
map_df <- map_data(myworld)

myworld180 = maps::map(wrap=c(-180,180), plot=FALSE, fill=TRUE)
# Convert the map data to a dataframe for ggplot2 plotting
map_df180 <- map_data(myworld180)

# Read eez.shp just the boundaries
eez_boundaries <- sf::st_read("/Users/bwhf/Documents/GitHub/political_connectivity/data/Marine_regions/World_EEZ_v11_20191118_HR_0_360", layer="eez_boundaries_v11_0_360")

# Density plot of fishing hours, bird timespent and fronts ------
recent_sp_grid_ocean <- read_rds("data/analysis/glob_hexgrid_dawn/recent_sp_ov_cls_grid_ocean.rds")
all_front <- read_rds("/Users/bwhf/Documents/PhD/USC/OceanProducts/front_freq/hexbin_cls_front_all_coord.rds")
grid_all_fh <- read_rds("/Users/bwhf/Documents/GitHub/political_connectivity/data/analysis/glob_hexgrid_dawn/hexgrid_res8_byspp_overlap_ts_fh_cls_ocean.rds")

grid_all_fh <- grid_all_fh %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")))

## front freq
p_ff_fh_ts <- plot_triple_density(recent_sp_grid_ocean, all_front, grid_all_fh, front_freq, "Front Freq")

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_ff_fh_ts_ocean.png")
ggsave(filename = outputfile, plot = p_ff_fh_ts, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## front strength
p_sf_fh_ts <- plot_triple_density(recent_sp_grid_ocean, all_front, grid_all_fh, msfront, "Mean Front Strength")

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_sf_fh_ts_ocean.png")
ggsave(filename = outputfile, plot = p_sf_fh_ts, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## front persistence
p_pf_fh_ts <- plot_triple_density(recent_sp_grid_ocean, all_front, grid_all_fh, pfront, "Front Persistence")

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_pf_fh_ts_ocean.png")
ggsave(filename = outputfile, plot = p_pf_fh_ts, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# Density of timespent and fishin hours ------
## front freq
pd_ff_fh_ts <- plot_tsfh_density(recent_sp_grid_ocean, grid_all_fh, front_freq, "Front Freq")

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_ff_fh_ts_nobg_ocean.png")
ggsave(filename = outputfile, plot = pd_ff_fh_ts, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## front strength
pd_sf_fh_ts <- plot_tsfh_density(recent_sp_grid_ocean, grid_all_fh, msfront, "Mean Front Strength")

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_sf_fh_ts_nobg_ocean.png")
ggsave(filename = outputfile, plot = pd_sf_fh_ts, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## front persistence
pd_pf_fh_ts <- plot_tsfh_density(recent_sp_grid_ocean, grid_all_fh, pfront, "Front Persistence")

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_pf_fh_ts_nobg_ocean.png")
ggsave(filename = outputfile, plot = pd_pf_fh_ts, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")



# Density of timespent and background fronts ------
# front freq
plot_ocean_tsff <- ts_ff_sp_v_ocean(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), front_freq, "front frequency")
plot_ocean_tsff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_ff_cls_ocean_bg.png")
ggsave(filename = outputfile, plot = plot_ocean_tsff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front persistence
plot_ocean_tspf <- ts_ff_sp_v_ocean(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front%>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), pfront, "front persistence")
plot_ocean_tspf

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_pf_cls_ocean_bg.png")
ggsave(filename = outputfile, plot = plot_ocean_tspf, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front strength
plot_ocean_tssf <- ts_ff_sp_v_ocean(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front%>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")),msfront, "mean front strength")
plot_ocean_tssf

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/ts_vs_sf_cls_ocean_bg.png")
ggsave(filename = outputfile, plot = plot_ocean_tssf, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")


# Density plot of Overlap and background fronts --------
# front freq
plot_ocean_ovff_density <- plot_dual_density(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), front_freq, "front frequency")
plot_ocean_ovff_density

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_ov_ff_cls_ocean.png")
ggsave(filename = outputfile, plot = plot_ocean_ovff_density, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front persistence
plot_ocean_ovpf_density <- plot_dual_density(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front%>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), pfront, "front persistence")
plot_ocean_ovpf_density

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_ov_pf_cls_ocean.png")
ggsave(filename = outputfile, plot = plot_ocean_ovpf_density, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front strength
plot_ocean_ovsf_density <- plot_dual_density(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front%>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")),msfront, "mean front strength")
plot_ocean_ovsf_density

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_ov_sf_cls_ocean.png")
ggsave(filename = outputfile, plot = plot_ocean_ovsf_density, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")


# Density plot of Overlap and no overlap ------
# front freq
plot_ocean_ovff_density2 <- plot_dual_density_2(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), front_freq, "front frequency")
plot_ocean_ovff_density2

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_ov_ts_cls_ocean.png")
ggsave(filename = outputfile, plot = plot_ocean_ovff_density2, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front freq
plot_ocean_ovff_density2 <- plot_dual_density_2(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), msfront, "front strength")
plot_ocean_ovff_density2

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_ov_ts_cls_ocean_msfront.png")
ggsave(filename = outputfile, plot = plot_ocean_ovff_density2, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front freq
plot_ocean_ovff_density2 <- plot_dual_density_2(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), pfront, "front presistence")
plot_ocean_ovff_density2

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/density_ov_ts_cls_ocean_pfront.png")
ggsave(filename = outputfile, plot = plot_ocean_ovff_density2, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# Background density and Overlap metrics in jitter -----
# front freq
plot_ocean_ovff <- ov_ff_sp_v_ocean(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), front_freq, "front frequency")
plot_ocean_ovff

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_ff_cls_ocean_bg.png")
ggsave(filename = outputfile, plot = plot_ocean_ovff, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front persistence
plot_ocean_ovpf <- ov_ff_sp_v_ocean(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front%>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), pfront, "front persistence")
plot_ocean_ovpf

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_pf_cls_ocean_bg.png")
ggsave(filename = outputfile, plot = plot_ocean_ovpf, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

# front strength
plot_ocean_ovsf <- ov_ff_sp_v_ocean(recent_sp_grid_ocean %>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")), all_front%>% dplyr::filter(ocean %in% c("Indian Ocean", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South Pacific Ocean")),msfront, "mean front strength")
plot_ocean_ovsf

# save the plot
outputfile <- paste0("data/analysis/figures_dawn/overlap_vs_sf_cls_ocean_bg.png")
ggsave(filename = outputfile, plot = plot_ocean_ovsf, width = 3840, height = 2160, unit = "px", dpi = 200, bg = "white")

## By species ----------------

# Overlap vs non overlap -----
# Generate list of plots (example for pfront)
dir.create("data/analysis/figures_dawn/species_density", recursive = TRUE)

species_plots <- plot_overlap_density_by_species(
  data = recent_sp_grid_ocean,
  variable = pfront,
  var_name = "pfront",
  x_label = "Persistence (pfront)",
  output_dir = "data/analysis/figures_dawn/species_density"
)

species_plots <- plot_overlap_density_by_species(
  data = recent_sp_grid_ocean,
  variable = msfront,
  var_name = "msfront",
  x_label = "mean front strength",
  output_dir = "data/analysis/figures_dawn/species_density"
)

species_plots <- plot_overlap_density_by_species(
  data = recent_sp_grid_ocean,
  variable = front_freq,
  var_name = "front_freq",
  x_label = "Front Freq (%)",
  output_dir = "data/analysis/figures_dawn/species_density"
)

# Overlap vs background -----
