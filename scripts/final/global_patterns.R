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

# assign <- "A"   #UK and Spain
assign <- "B" #Argentina and Morocco


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

visit <- relations %>% group_by(jurisdiction) %>% filter(relation == "Visiting") %>% summarise( # count of species visiting only
  visitonly_rich  = n_distinct(scientific_name)
)

both <- relations %>% group_by(jurisdiction) %>% filter(relation == "Both") %>% summarise(      # both visiting and breeding spp count
  both_rich  = n_distinct(scientific_name)
)

breed <- relations %>% group_by(jurisdiction) %>% filter(relation == "Breeding") %>% summarise( # count of species only breeding
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
rich <- full_join(tot, visit, by="jurisdiction", all=T) %>% left_join(both, by="jurisdiction", all=T) %>% left_join(breed, by="jurisdiction", all=T) %>% left_join(pop_by_jur, by="jurisdiction") %>% mutate(visit_rich = visitonly_rich + both_rich) %>% dplyr::select(jurisdiction, richness, breed_rich, visit_rich, both_rich, breedonly_rich, visitonly_rich, tot_breeders) %>% arrange(desc(richness))

rich <- rich %>% mutate(
  breed_rich = ifelse(is.na(breed_rich), 0, breed_rich),
  visit_rich = ifelse(is.na(visit_rich), 0, visit_rich),
  both_rich = ifelse(is.na(both_rich), 0, both_rich),
  breedonly_rich = ifelse(is.na(breedonly_rich), 0, breedonly_rich),
  visitonly_rich = ifelse(is.na(visitonly_rich), 0, visitonly_rich)
)


## Plot ordered by breeding richness ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
# plots combined in to composites in "patchwork_plots.R" script

# make long format for plotting
p <- rich %>% top_n(n=15, breed_rich) %>% arrange(desc(breed_rich)) %>% mutate(jurisdiction = factor(jurisdiction, levels=c(jurisdiction))) %>% gather("rich_type", "rich", both_rich, breedonly_rich, visitonly_rich) %>% mutate(rich_type = factor(rich_type, levels=c("visitonly_rich",  "both_rich", "breedonly_rich"))) %>% 
  ggplot(aes(x=jurisdiction, y=rich, fill=rich_type)) + geom_col(color="black") + theme_bw() +
  scale_fill_manual(values = c("grey70","grey35","black"), labels = c(' Visiting', ' Both', ' Breeding')) +
  # scale_fill_manual(values = c("#fee8c8","#fdbb84","#e34a33"), labels = c(' Visiting', ' Both', ' Breeding')) +
  # scale_fill_viridis_d(option="cividis", labels = c(' Visiting', ' Both', ' Breeding')) +
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
  scale_y_continuous(expand=expansion(mult = c(0, .09)))
  # theme(legend.position = "none")

## SAVE ##
# if(assign == "A"){
#   ggsave(paste0(master_figs, "barcharts/country_richness_split3_bybreedrichX.png"), width = 13, height = 9)
# } else if(assign == "B"){
#   ggsave(paste0(master_figs, "sovereign_B_assign/barcharts/country_richness_split3_bybreedrichX.png"), width = 13, height = 9)
# }


## Plot ordered by TOTAL richness ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make long format for plotting
p <- rich %>% top_n(n=15, richness) %>% arrange(desc(richness)) %>% mutate(jurisdiction = factor(jurisdiction, levels=c(jurisdiction))) %>% gather("rich_type", "rich", both_rich, breedonly_rich, visitonly_rich) %>% mutate(rich_type = factor(rich_type, levels=c("visitonly_rich",  "both_rich", "breedonly_rich"))) %>% 
  ggplot(aes(x=jurisdiction, y=rich, fill=rich_type)) + geom_col(color="black") + theme_bw() +
  scale_fill_manual(values = c("grey70","grey35","black"), labels = c(' Visiting', ' Both', ' Breeding')) +
  # scale_fill_manual(values = c("#fee8c8","#fdbb84","#e34a33"), labels = c(' Visiting', ' Both', ' Breeding')) +
  theme(    
    legend.title = element_blank(),
    legend.text.align = 0,
    legend.text = element_text(size = 14)
    ) 
p2 <- p + 
  ylab("Richness") +
  theme( 
    axis.text.x = element_text( angle = 45, hjust=1, size = 30),
    axis.text.y = element_text(size = 30),
    axis.title = element_text(size = 32),
    axis.title.y = element_text(size = 32, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_blank(),
    legend.text.align = 0,
    legend.text = element_text(size = 30),
    plot.margin = unit(c(0.1,0.1,0.1,0.5), "cm")
  ) + 
  scale_y_continuous(expand=expansion(mult = c(0, .09)))
  # theme(legend.position = "none")


## SAVE ##
# if(assign == "A"){
#   ggsave(paste0(master_figs, "barcharts/country_richness_split3_bytotalrichX.png"), width = 13, height = 9)
# } else if(assign == "B"){
#   ggsave(paste0(master_figs, "sovereign_B_assign/barcharts/country_richness_split3_bytotalrichX.png"), width = 13, height = 9)
# }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculate annual time spent per jurisdiction

# aggregate time spent by ORIGIN 
x <- alltimes %>% group_by(scientific_name, adj_site_name, month, is_origin, jurisdiction) %>% summarise(
  tot_atatime = first(tot_atatime)
) %>% group_by(is_origin, jurisdiction) %>% summarise( 
  tot_staying = sum(na.omit(tot_atatime)) / 12
)

sum(x$tot_staying) ## total bird years estimated by our tracking datas (pop coverage - annual time coverage)

timespentsum <- x %>% group_by(jurisdiction) %>% summarise(sum_tot_staying = sum(na.omit(tot_staying))) %>% left_join(x)

# lump together jurisdictions hosting less than XX% of global time
other <- timespentsum %>% mutate(jurisdiction = if_else(sum_tot_staying/12 < ( (glob_cover$glob_tot) * 0.001), "Other", jurisdiction)) %>% filter(jurisdiction == "Other") %>% group_by(is_origin) %>% summarise(jurisdiction = first(jurisdiction), tot_staying = sum(na.omit(tot_staying)))

other$sum_tot_staying <- rep(sum(other$tot_staying))

time <- timespentsum %>% mutate(jurisdiction = if_else(sum_tot_staying < ((glob_cover$glob_tot) * 0.001), "Other", jurisdiction)) %>% filter(jurisdiction != "Other") %>% bind_rows(other) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Horizontal time spent plot 
p3 <- time %>% top_n(n=20, sum_tot_staying) %>% arrange(sum_tot_staying) %>% mutate(
  jurisdiction = factor(jurisdiction, levels=c("Other", unique(jurisdiction[jurisdiction != "Other"])))) %>% 
  ggplot(aes(x=reorder(jurisdiction, desc(jurisdiction)), y=tot_staying/1000000, fill=is_origin)) + geom_col(color="black", position = position_stack(reverse = TRUE)) +
  theme_bw() +
  scale_fill_manual(values = c("black","grey70"), labels = c(' Breeding',' Visiting')) +
  # scale_fill_manual(values = c("#fee8c8","#e34a33"), labels = c(' Visiting', ' Breeding')) +
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

## Vertical time spent plot #~~~~~~~~~~~~~~~~
# p <- time %>% top_n(n=20, sum_tot_staying) %>% arrange(desc(sum_tot_staying)) %>% mutate(
#   jurisdiction = factor(jurisdiction, levels=c(unique(jurisdiction[jurisdiction != "Other"]), "Other"))) %>% 
#   ggplot(aes(x=reorder(jurisdiction, desc(jurisdiction)), y=tot_staying/1000000, fill=is_origin)) + geom_col(color="black", position = position_stack(reverse = TRUE)) +
#   theme_bw() +
#   scale_fill_manual(values = c("black","grey70"), labels = c(' Breeding',' Visiting')) +
#   theme(
#     legend.title = element_blank(),
#     legend.text.align = 0,
#     legend.text = element_text(size = 18)
#     ) +
#   ylab("Bird year (millions)") + 
#   xlab("")
# 
# p <- p + theme( 
#   text = element_text(size=14),
#   # axis.text.x = element_text( angle = 90, hjust = 1, vjust=0.3, size = 16),
#   axis.text.x = element_text( angle = 0, hjust=.5, size = 25),
#   axis.text.y = element_text(size = 25),
#   axis.title = element_text(size = 28),
#   axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))
# ) + 
#   scale_y_continuous(expand=expand_scale(mult = c(0, .09))) + 
#   coord_flip() +
#   theme(legend.position = "none")

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
