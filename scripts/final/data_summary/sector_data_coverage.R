# ~~~~~~~ # Estating regional coverage of tracking samples to give more detailed 
# information on potential spatial sampling bias #~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allpops <- readxl::read_xlsx("data_summaries_FINAL\\pop_data\\AllSpp_global_pops.xlsx")

allpops <- allpops %>% group_by(Species) %>% summarise(
  n_sectors = n_distinct(ocean_sector)
) %>% left_join(allpops)

sec_pops <- allpops %>% group_by(Species, ocean_sector) %>%
  summarise(
    glob_pop_IND = first(`Global pop_IND`),
    sector_pop = if_else(first(n_sectors) > 1, sum(na.omit(`Population estimate_IND`)), first(`Global pop_IND`))
)

sec_pops

track_pops <- allpops %>% group_by(Species, ocean_sector, tracks_rep) %>% summarise(
  tracked_pop = sum(na.omit(`Population estimate_IND`))
) %>% ungroup() %>% filter(tracks_rep == 1) 


## species summary ## 
sp_sum <- sec_pops %>% left_join(track_pops, by=c("Species", "ocean_sector") ) %>% group_by(Species, ocean_sector) %>% summarise(
  perc_glob  = (sector_pop / glob_pop_IND)*100,
  perc_cover = (tracked_pop / sector_pop)*100
)

## sectoral summary ##
#** could manually add a column for number of species with breeding pops in each sector w/out tracking data
#* could 
sector_sum <- sp_sum %>% group_by(ocean_sector) %>% summarise(
  n_sp_tracked = n_distinct(Species),
  mn_p_cover = mean(na.omit(perc_cover)),
  sd_p_cover = sd(na.omit(perc_cover)),
  min_p_cover = min(na.omit(perc_cover)),
  max_p_cover = max(na.omit(perc_cover)),
  mn_perc_glob = mean(perc_glob)
)

write.csv(sector_sum, "C:\\Users\\Martim Bill\\Documents\\political_connectivity\\data\\analysis\\summary_tables\\sector_cover.csv")
