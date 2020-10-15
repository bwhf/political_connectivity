#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate importance: richness and time spent per jurisdiction, and create bar charts and tables 

pacman::p_load(dplyr, tidyr, stringr, ggplot2)

## Choose whether to use high threshold or low threshold data (i.e. >1 bird per month) ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# thresh <- "high"
thresh  <- "low"

if(thresh == "high") {
  master <- "data/analysis/bird_thresh/"
  master_figs <- "figures/bird_thresh/"
} else {
  master <- "data/analysis/"
  master_figs <- "figures/"
}

## Choose whether to analyze UK-assigned or Argentina-assigned data ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

assign <- "A"   #UK and Spain
# assign <- "B" #Argentina and Morocco


PD <- read.csv('data/population_estimates.csv', stringsAsFactors = F) # population data 

if(assign == "A"){
  folder <- paste0(master, "glob_count/")
} else if(assign == "B"){
  folder <- paste0(master, "sovereign_B_assign/glob_count/")
  # re-assign birds on disputed islands to Argentina and Morocco
  PD$jurisdiction <- ifelse(PD$site_name == "Falkland Islands (Islas Malvinas)" | PD$site_name == "South Georgia (Islas Georgias del Sur)", "Argentina", 
                            ifelse(PD$site_name == "Chafarinas", "Morocco", PD$jurisdiction))
  PD$origin <- ifelse(PD$site_name == "Falkland Islands (Islas Malvinas)" | PD$site_name == "South Georgia (Islas Georgias del Sur)", "Argentina", ifelse(PD$site_name == "Chafarinas", "Morocco", PD$jurisdiction))
}

## Run analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

files <- list.files(folder, full.names = T)
# files <- files[1:2]
alltimes <- do.call("rbind", lapply(files, function(x) readRDS(x)))

alltimes$is_origin <- ifelse(alltimes$jurisdiction == alltimes$origin, "Breeding", "Visiting")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate RICHNESS: breeding, visiting, only breeding/visiting (species from pops outside country)

# what is relationship(s) between species and a jurisdiction?
relations <- alltimes %>% group_by(jurisdiction, scientific_name) %>% summarise(
  n_relations = n_distinct(is_origin), 
  relation = if_else(n_relations == 2, "Both", first(is_origin))
) 

tot <- relations %>% group_by(jurisdiction) %>% summarise(                                      # total species count
  richness  = n_distinct(scientific_name)
)

visit <- relations %>% group_by(jurisdiction) %>% dplyr::filter(relation == "Visiting") %>% summarise( # count of species visiting only
  visitonly_rich  = n_distinct(scientific_name)
)

both <- relations %>% group_by(jurisdiction) %>% dplyr::filter(relation == "Both") %>% summarise(      # both visiting and breeding spp count
  both_rich  = n_distinct(scientific_name)
)

breed <- relations %>% group_by(jurisdiction) %>% dplyr::filter(relation == "Breeding") %>% summarise( # count of species only breeding
  breedonly_rich  = n_distinct(scientific_name)
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Use population data to calculate FULL breeding richness 

PD$spp_site <- paste(PD$scientific_name, PD$standard_site_name)
alltimes$spp_site <- paste(alltimes$scientific_name, alltimes$adj_site_name)

#filter to only spp-site combos which we have tracking data for 
PD <- PD[which(PD$spp_site %in% unique(alltimes$spp_site)),]


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# global, species-site estimates from tracked populations
pop_by_jur_spp <- PD %>% group_by(scientific_name, jurisdiction) %>% summarise( 
  glob_pop_IND   =  first(global_pop_estimate_IND),
  tot_breeders = sum(na.omit(pop_estimate_IND)),
  breed_rich   = n_distinct(scientific_name)
)

# total number of birds, covered, and not, by tracking 
glob_cover <- pop_by_jur_spp %>% group_by(scientific_name) %>% summarise(
  glob_pop_IND = first(glob_pop_IND),
  tot_track_pops = sum(tot_breeders)
) %>% ungroup() %>% summarise(
  glob_tot = sum(glob_pop_IND),
  glob_tot_track_pops = sum(tot_track_pops),
  not_covered = glob_tot - glob_tot_track_pops
) ## Number of birds not covered by our tracking (roughly)

# sum totals for host countries (of tracked populations) and breeding richnesses
pop_by_jur <- pop_by_jur_spp %>% group_by(jurisdiction) %>% summarise(
  tot_breeders = sum(na.omit(tot_breeders)),
  breed_rich   = sum(breed_rich)
)

## DF of all angles on richness
rich <- full_join(tot, visit, by="jurisdiction", all=T) %>% left_join(both, by="jurisdiction", all=T) %>% 
  left_join(breed, by="jurisdiction", all=T) %>% 
  left_join(pop_by_jur, by="jurisdiction") %>% 
  mutate(
    visit_rich = visitonly_rich + ifelse(is.na(both_rich), 0, both_rich)
  ) %>%
  dplyr::select(jurisdiction, richness, breed_rich, visit_rich, both_rich, breedonly_rich, visitonly_rich, tot_breeders) %>% arrange(desc(richness))

rich <- rich %>% mutate(
  breed_rich = ifelse(is.na(breed_rich), 0, breed_rich),
  visit_rich = ifelse(is.na(visit_rich), 0, visit_rich),
  both_rich = ifelse(is.na(both_rich), 0, both_rich),
  breedonly_rich = ifelse(is.na(breedonly_rich), 0, breedonly_rich),
  visitonly_rich = ifelse(is.na(visitonly_rich), 0, visitonly_rich)
)


## Plot ordered by breeding richness ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# plots combined in to composites in "patchwork_plots.R" script

## Combining observed richness and time spent result w/ error bars from re-sampling ##

# summarized results from re-sampling analysis 100 times
# summ <- readRDS("data_test//GLS_error//result//summary.rds")
# summ <- readRDS("data_test//GLS_error//result//summary_plus_obs.rds")
summ <- readRDS("data//analysis//GLS_error//result//summary.rds")
summ <- readRDS("data//analysis//GLS_error//result//summary_plus_obs.rds")
#-----------------------------------------------------------------------------
## Breeding Richness ##

rich_plot2 <- rich %>% left_join(summ[c(1:10)], by="jurisdiction") %>%
  top_n(n=15, breed_rich) %>% arrange(desc(breed_rich)) %>% 
  mutate(jurisdiction = factor(jurisdiction, levels=c(jurisdiction))) %>% 
  gather("rich_type", "rich", both_rich, breedonly_rich, visitonly_rich) %>% 
  mutate(rich_type = factor(rich_type, levels=c("visitonly_rich",  "both_rich", "breedonly_rich")))

# make long format for plotting
p <- ggplot() + 
  geom_col(
    data=rich_plot2,
    aes(x=jurisdiction, y=rich, fill=rich_type), 
    color="black"
  ) + 
  geom_errorbar(
    data=rich_plot2,
    aes(
      ymin = (min_richness), 
      ymax = (max_richness), 
      x=jurisdiction),
    width=.5,
    size=1.25
  ) +
  theme_bw() +
  scale_fill_viridis_d(begin = 0.15, end = 1, direction = 1, labels = c(' Visiting', ' Both', ' Breeding')) +
  theme(legend.title = element_blank()) 
# p
p2 <- p + 
  ylab("Richness") +
  theme( 
    axis.text.x = element_text( angle = 45, hjust=1, size = 30),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 32, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_blank(),
    legend.text.align = 0,
    legend.text = element_text(size = 30),
    plot.margin = unit(c(0.1,0.1,0.1,0.6), "cm")
  ) + 
  scale_y_continuous(expand=expansion(mult = c(0, .09))) +
  theme(legend.position = "none")
p2

## SAVE ##
# if(assign == "A"){
#   ggsave(paste0(master_figs, "barcharts/country_richness_split3_bybreedrichX.png"), width = 13, height = 9)
# } else if(assign == "B"){
#   ggsave(paste0(master_figs, "sovereign_B_assign/barcharts/country_richness_split3_bybreedrichX.png"), width = 13, height = 9)
# }


## Plot ordered by TOTAL richness ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-------------------------------------------------------------------------------
## Absolute richness ## 

rich_plot1 <- rich %>% left_join(summ[c(1:10)], by="jurisdiction") %>% 
  top_n(n=15, richness) %>% 
  arrange(desc(richness)) %>% 
  mutate(
    jurisdiction = factor(jurisdiction, levels=c(jurisdiction))
    ) %>% 
  gather(
    "rich_type", "rich", both_rich, breedonly_rich, visitonly_rich
    ) %>%
  mutate(
    rich_type = factor(rich_type, levels=c("visitonly_rich",  "both_rich", "breedonly_rich"))
    )

p <- ggplot() + 
  geom_col(
    data=rich_plot1,
    aes(x=jurisdiction, y=rich, fill=rich_type), 
    color="black"
  ) + 
  geom_errorbar(
    data=rich_plot1,
    aes(
      ymin = (min_richness), ymax = (max_richness), x=jurisdiction
      ),
    width=.5,
    size=1.25
  ) +
  theme_bw() +
  scale_fill_viridis_d(
    begin = 0.15, end = 1, direction = 1, labels = c(' Visiting', ' Both', ' Breeding')
    ) +
  theme(legend.title = element_blank()) 
# p
p1 <- p + 
  ylab("Richness") +
  theme( 
    axis.text.x = element_text( angle = 45, hjust=1, size = 30),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 32, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_blank(),
    legend.text.align = 0,
    legend.text = element_text(size = 30),
    plot.margin = unit(c(0.1,0.1,0.1,0.6), "cm")
  ) + 
  scale_y_continuous(expand=expansion(mult = c(0, .09))) #+
#theme(legend.position = "none")
p1

## SAVE ##
# if(assign == "A"){
#   ggsave(paste0(master_figs, "barcharts/country_richness_split3_bytotalrichX.png"), width = 13, height = 9)
# } else if(assign == "B"){
#   ggsave(paste0(master_figs, "sovereign_B_assign/barcharts/country_richness_split3_bytotalrichX.png"), width = 13, height = 9)
# }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculate annual time spent per jurisdiction

# aggregate time spent by ORIGIN 
time_origin <- alltimes %>% group_by(scientific_name, adj_site_name, month, is_origin, jurisdiction) %>% summarise(
  tot_atatime = first(tot_atatime)
) %>% group_by(is_origin, jurisdiction) %>% summarise( 
  tot_staying = sum(na.omit(tot_atatime)) / 12
)

sum(time_origin$tot_staying) ## total bird years estimated by our tracking datas (pop coverage - annual time coverage)

timespentsum <- time_origin %>% 
  group_by(jurisdiction) %>% 
  summarise(sum_tot_staying = sum(na.omit(tot_staying))) %>% 
  left_join(time_origin)

# lump together jurisdictions outside top X

top15 <- timespentsum %>% top_n(n=25, sum_tot_staying)
top15 <- unique(top15$jurisdiction)
other <- timespentsum[!timespentsum$jurisdiction %in% top15, ]
other <- other %>% mutate(jurisdiction = "Other") %>% filter(jurisdiction == "Other") %>% group_by(is_origin) %>% summarise(jurisdiction = first(jurisdiction), tot_staying = sum(na.omit(tot_staying)))

other$sum_tot_staying <- rep(sum(other$tot_staying))

time <- timespentsum %>% mutate(jurisdiction = if_else(sum_tot_staying < ((glob_cover$glob_tot) * 0.001), "Other", jurisdiction)) %>% filter(jurisdiction != "Other") %>% bind_rows(other) 

time <- timespentsum %>% bind_rows(other)

time$is_origin <- factor(time$is_origin, levels = c("Visiting", "Breeding"))

#-----------------------------------------------------------------------------
## Time spent  ##

time_resample <- summ %>% 
  top_n(mn_tot_atatime, n=15) %>% 
  arrange(desc(mn_tot_atatime)) %>% 
  mutate(
    jurisdiction = factor(jurisdiction, levels=unique(jurisdiction))
  ) 

time_plot <- time %>% top_n(n=25, sum_tot_staying) %>% arrange(sum_tot_staying) %>% mutate(
  jurisdiction = factor(jurisdiction, levels=c("Other", unique(jurisdiction[jurisdiction != "Other"]))))

p3 <- ggplot() + geom_col(
  data=time_plot,
  aes(x=reorder(jurisdiction, desc(jurisdiction)), y=tot_staying/1000000, fill=is_origin), 
  color="black") +
  theme_bw() +
  geom_errorbar( # error bard
    data=time_resample,
    aes(
      # ymin = (mn_tot_atatime-sd_tot_atatime)/1000000,
      # ymax = (mn_tot_atatime+sd_tot_atatime)/1000000,
      ymin = (min_tot_atatime)/1000000, 
      ymax = (max_tot_atatime)/1000000, 
      x=jurisdiction),
    width=.5,
    size=1.25
  ) +
  scale_fill_viridis_d(begin = 0.15, end = 1, direction = 1) +
  theme( 
    axis.text.x = element_text( angle = 45, hjust=1, size = 30),
    axis.text.y = element_text(size = 30),
    axis.title.y = element_text(size = 32, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.text.align = 0,
    legend.text = element_text(size = 30),
    plot.margin = unit(c(0.1,0.1,0.1,1), "cm")
  ) + 
  scale_y_continuous(expand=expansion(mult = c(0, .09))) +
  ylab("Bird year (millions)") + 
  theme(legend.position = "none")
p3

## SAVE ##
# if(assign == "A"){
#   ggsave(paste0(master_figs, "barcharts/country_abundance_byTIMESPENT_birdyears_AVG.png"), width = 10, height = 11)
# } else if(assign == "B"){
#   ggsave(paste0(master_figs, "sovereign_B_assign/barcharts/country_abundance_byTIMESPENT_birdyears_AVG.png"), width = 10, height = 11)
# }


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create table summarizing richness and timespent values per jurisdiction

sum2save <- timespentsum %>% spread(is_origin, tot_staying) %>% 
  full_join(rich, by="jurisdiction") %>% 
  rename(breed_time=Breeding, visit_time=Visiting) %>% 
  mutate(is_origin = if_else(breed_rich==0, TRUE, FALSE)) %>% 
  arrange(is_origin) %>% 
  dplyr::select(
    jurisdiction, richness, breed_rich, visit_rich, both_rich, breedonly_rich, 
    visitonly_rich, tot_breeders, sum_tot_staying, breed_time, visit_time
  )
sum2save

## SAVE ##

if(thresh == "high"){
  write.csv(sum2save, paste0(master, "summary_tables/richness_visit_per_jur.csv"), row.names = F)
  
} else if(thresh == "low"){
  
  if(assign == "A"){
    write.csv(sum2save, paste0(master, "summary_tables/richness_visit_per_jur.csv"), row.names = F)
  } else if(assign == "B"){
    write.csv(sum2save, paste0(master, "sovereign_B_assign/summary_tables/richness_visit_per_jur.csv"), row.names = F)
  }
  
}
