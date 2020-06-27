## Producing tables and figures prodividing summaries for reporting/methods in paper ##
# tables/figures of % coverage of global breeding population, as well as annual cycle time 

pacman::p_load(dplyr, stringr)

#=====================================================================

## Choose whether to use high threshold or low threshold data (i.e. >1 bird per month) ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# thresh <- "high"
thresh  <- "low"

if(thresh == "high"){
  master <- "data/analysis/bird_thresh/"
  master_figs <- "figures/bird_thresh/"
} else {
  master <- "data/analysis/"
  master_figs <- "figures/"
}

folder <- paste0(master, "glob_count/")

#~~~~~~~~~~~~~~~~~~~~ 
# load glob_cnt data and combine
files <- list.files(folder, full.names = T)
alltimes <- do.call("rbind", lapply(files, function(x) readRDS(x)))

# Abbreviating UK and USA
alltimes <- alltimes %>% mutate(
  jurisdiction = if_else(jurisdiction == "United States", "USA", 
    if_else(jurisdiction == "United Kingdom", "UK", jurisdiction)),
  origin = if_else(origin == "United States", "USA", 
    if_else(origin == "United Kingdom", "UK", origin))
)

# # assigning Falklands and S. Georgia to the UK (for expediency)
# alltimes <- alltimes %>% mutate( 
#   origin = if_else(origin == "Disputed " | origin == "Disputed", "UK", origin), 
#   jurisdiction = if_else(jurisdiction == "Disputed " | jurisdiction == "Disputed" , "UK", jurisdiction))


#######~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# proportion of the global population covered by tracking data
PD <- read.csv('data/population_estimates.csv', stringsAsFactors = F)

PD$spp_site <- paste(PD$scientific_name, PD$standard_site_name)
alltimes$spp_site <- paste(alltimes$scientific_name, alltimes$adj_site_name)

# filter to only spp-site combos which we have tracking data for 
PD <- PD[which(PD$spp_site %in% unique(alltimes$spp_site)),] %>% mutate(prop_global_pop = (pop_estimate_IND/global_pop_estimate_IND)) 


sp_cover <- alltimes %>% group_by(scientific_name, adj_site_name) %>% summarise(
  n_month = n_distinct(month),
  ann_cover = n_month / 12
) %>% left_join(
  PD[,c("scientific_name", "standard_site_name", "prop_global_pop", "global_pop_estimate_IND")],
  by=c("scientific_name", "adj_site_name"="standard_site_name")) %>% mutate(
    glob_ann_cov = ann_cover * prop_global_pop
  ) %>% group_by(scientific_name) %>% summarise(
    n_sites      = n_distinct(adj_site_name),
    glob_pop     = first(global_pop_estimate_IND),
    glob_pop_cov = sum(na.omit(prop_global_pop)) * 100,
    glob_ann_cov = sum(glob_ann_cov) *100,
    glob_ann_time = (glob_ann_cov/100) * glob_pop
  ) %>% dplyr::select(scientific_name, n_sites, glob_pop, glob_pop_cov, glob_ann_time, glob_ann_cov)

sum(sp_cover$glob_pop)   # global total number of breeding large petrels 
# sum(sp_cover$glob_pop) + 15848 # global total + S. Royal
sum(PD$pop_estimate_IND) # total covered
sum(sp_cover$glob_ann_time)
round(sum(sp_cover$glob_ann_time) / (sum(PD$pop_estimate_IND)), 2)
# sum(sp_cover$glob_ann_time) / (sum(sp_cover$glob_pop) + 15848) # global total + S. Royal

round(mean(sp_cover$glob_pop_cov))   # average coverage of global breeding population for each species
c(min(sp_cover$glob_pop_cov), max(sp_cover$glob_pop_cov)) # range
# sum(sp_cover$glob_pop_cov)/40 # average including S. Royal Albatross (no data)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# table of perc. time spent in ABNJ, EEZ, and what is unknown

visitsum <- alltimes %>% group_by(scientific_name, adj_site_name, month, jurisdiction) %>% summarise(
  globprop = first(globprop)) %>% group_by(scientific_name, jurisdiction) %>% summarise(
    glob_ann_prop = sum(na.omit(globprop)) / 12 
  ) 

# proportion of time spent in ABNJ waters 
abnj <- visitsum %>% filter(jurisdiction == "High seas") %>% arrange(desc(glob_ann_prop)) %>% rename(ABNJ = glob_ann_prop) %>% dplyr::select(-jurisdiction)

# which (if any) species don't visit the high seas
# sp1 <- unique(alltimes$scientific_name)
# sp2 <- unique(abnj$scientific_name)
#
# sp1[!sp1 %in% sp2] # D. epomorphora doesn't have enough data to show this

# proportion of time spent in EEZ waters 
nation <- visitsum %>% filter(jurisdiction != "High seas") %>% arrange(desc(glob_ann_prop)) %>% group_by(scientific_name) %>% summarise(
  EEZ = sum(glob_ann_prop)
) %>% print(n=Inf)

# adding the unknown perc. of time (via untracked pops or months)
annual_time <- nation %>% left_join(abnj, by="scientific_name", all=T) %>% mutate(
  unknown = (1 - (ABNJ + EEZ)) * 100,
  ABNJ    = ABNJ * 100,
  EEZ     = EEZ * 100
) %>% arrange(scientific_name) %>% print(n=Inf)


sp_sum <- sp_cover %>% left_join(annual_time, by="scientific_name") %>% print(n=Inf)

## add number of tracks to table
track_summary <- read.csv( "data/analysis/summary_tables/track_summary_sp_site.csv", stringsAsFactors = F)

n_tracks <- track_summary %>% group_by(scientific_name) %>% summarise(
  n_tracks = sum(n_tracks),
  n_birds  = sum(n_birds)
  )

sp_sum <- merge(sp_sum, n_tracks)

## SAVE ##
data.table::fwrite(sp_sum, paste0(master, "summary_tables/sp_time_spent.csv") )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# split EEZ into "breeding" and "non-breeding" country

origin_visitsum <- alltimes %>% group_by(scientific_name, adj_site_name, origin, month, jurisdiction) %>% summarise(
  globprop = first(globprop)) %>% group_by(scientific_name, origin, jurisdiction) %>% summarise(
  glob_ann_prop = sum(na.omit(globprop)) / 12 
) 

breed <- origin_visitsum %>% filter(jurisdiction == origin) %>% arrange(desc(glob_ann_prop)) %>% group_by(scientific_name) %>% summarise(
  breeding = sum(glob_ann_prop)
) %>% print(n=Inf)

non_breed <- origin_visitsum %>% filter(jurisdiction != origin & jurisdiction != "High seas") %>% arrange(desc(glob_ann_prop)) %>% group_by(scientific_name) %>% summarise(
  non_breeding = sum(glob_ann_prop)
) %>% print(n=Inf)


# adding the unknown perc. of time (via untracked pops or months)
eez_time <- breed %>% left_join(non_breed, by="scientific_name", all=T) 

annual_time <- eez_time %>% left_join(abnj, by="scientific_name", all=T) %>% mutate(
  unknown = (1 - (ABNJ + breeding + non_breeding)) * 100,
  ABNJ    = ABNJ * 100,
  breeding         = breeding * 100,
  non_breeding     = non_breeding * 100
) %>% arrange(scientific_name) %>% print(n=Inf)


sp_sum_bnb <- sp_cover %>% left_join(annual_time, by="scientific_name") %>% print(n=Inf)

##
# data.table::fwrite(sp_sum_bnb, "data/analysis/summary_tables/sp_time_spent_breed.nonbreed.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create bar chart(s) of annual time spent in general Jurisdiction categories, and/or global population coverage
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

species_df <- read.csv("data_summaries_FINAL/species_list.csv", stringsAsFactors = F)



## sp_sum is made in 'general_summaries' script
sp_sum$common_name <- species_df$common_name[match(sp_sum$scientific_name, species_df$scientific_name)]
sp_sum$genus       <- species_df$genus[match(sp_sum$scientific_name, species_df$scientific_name)]
sp_sum$family      <- species_df$family[match(sp_sum$scientific_name, species_df$scientific_name)]

# order common names by family, genus and alphabetically within that
sp_sum <- sp_sum %>% filter(scientific_name != "Diomedea epomophora")

sp_sum$common_name <- factor(sp_sum$common_name, levels=unique(sp_sum$common_name[order(sp_sum$family, sp_sum$genus, sp_sum$common_name )]), ordered=TRUE)
sp_sum$common_name <- factor(sp_sum$common_name, levels=unique(sp_sum$common_name[c(order(sp_sum$family, decreasing = T), order(sp_sum$genus, sp_sum$common_name ))]), ordered=TRUE)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sp_sum$name <- sp_sum$common_name ## choose which name to use in plots


#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ one plot (horizontal, reverse-direction) showing % pop., % annual time, and % untracked per sp.

sp_sum_plot <- sp_sum %>% mutate(
  glob_ann_cov = glob_ann_cov,
  glob_pop_cov  = glob_pop_cov - glob_ann_cov,
  untracked     = 100 - (glob_pop_cov + glob_ann_cov)
) %>% dplyr::select(scientific_name, family, name, n_sites, glob_pop_cov, glob_ann_cov, untracked)

long <- tidyr::pivot_longer(sp_sum_plot, cols=c("glob_ann_cov", "untracked", "glob_pop_cov"), names_to="type") %>% group_by(name) %>% mutate(
  type = factor(type, levels = c("untracked", "glob_pop_cov", "glob_ann_cov"))
)

long <- long %>% filter(name != "Diomedea epomophora" | name != "Southern Royal Albatross")


## add number of birds tracked for labelling 
track_summary <- read.csv( "data/analysis/summary_tables/track_summary_sp_site_device.csv", stringsAsFactors = F)

n_birds <- track_summary %>% group_by(scientific_name) %>% summarise( n_birds = sum(n_birds))

long <- merge(long, n_birds)

p <- ggplot(long, aes(x=reorder(name, desc(name)), y=-value, fill=type)) +
  geom_col() + 
  # scale_fill_manual(values = c("unknown" = "grey80", "ABNJ" = "#e31a1c", "EEZ" = '#08519c'), labels=c("Unknown", "EEZ", "High seas")) + 
  scale_fill_manual(values = c("untracked" = "grey80", "glob_ann_cov" = 'black', "glob_pop_cov" = 'grey40'), labels=c("Untracked", "Population", "Annual time")) +
  ylab("% coverage") +
  xlab(NULL)
# p

p + 
  theme(
    text = element_text(size=14),
    # axis.text.x = element_text( , hjust = 1, vjust=0.3, size = 16),
    axis.text.x = element_text( hjust=0.5, size = 16),
    axis.text.y = element_text(hjust=1, size = 14),
    axis.title = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=14),
  plot.margin=unit(c(1,0,1,1.5),"cm")
) +
  guides(
    edge_width = guide_legend(title="Strength", order = 1),
    size  = guide_legend(title="Species richness", order = 2),
    fill = guide_legend(title="Ocean basin", order = 3),
    color = guide_legend(title="Jurisdiction", order = 4, override.aes = list(size=4))
  ) + 
  scale_x_discrete(expand=c(0,0), name = "", position = "top") +
  scale_y_continuous(expand=c(0,0), breaks = seq(0, -100, by=-25), labels=seq(0, 100, by=25)) + 
  geom_text(aes(label= paste0("", n_birds, "")), y=-101.5, nudge_x=0.025, hjust = 1, color="grey30", size=5.4) +
  coord_flip(clip = 'off') 


## SAVE ## 
ggsave("C:/Users/Martim Bill/Desktop/test/spp_cover_plot5.png", width=10, height=11, device="png")



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Flip the other direction ~~~~~~~~~~~~~~~~~~~~~~~~~

p <- ggplot(long, aes(x=reorder(name, desc(name)), y=value, fill=type)) +
  geom_col() + 
  # scale_fill_manual(values = c("unknown" = "grey80", "ABNJ" = "#e31a1c", "EEZ" = '#08519c'), labels=c("Unknown", "EEZ", "High seas")) + 
  scale_fill_manual(values = c("untracked" = "grey80", "glob_ann_cov" = 'black', "glob_pop_cov" = 'grey40'), labels=c("Untracked", "Population", "Annual time")) +
  ylab("% coverage") +
  xlab(NULL)
# p

p + 
  theme(
    text = element_text(size=14),
    # axis.text.x = element_text( , hjust = 1, vjust=0.3, size = 16),
    axis.text.x = element_text( hjust=0.5, size = 16),
    axis.text.y = element_text(hjust=1, size = 14),
    axis.title = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    plot.margin=unit(c(1,1.5,1,0),"cm")
  ) +
  guides(
    edge_width = guide_legend(title="Strength", order = 1),
    size  = guide_legend(title="Species richness", order = 2),
    fill = guide_legend(title="Ocean basin", order = 3),
    color = guide_legend(title="Jurisdiction", order = 4, override.aes = list(size=4))
  ) + 
  # scale_x_discrete(expand=c(0,0), name = "", position = "top") +
  scale_y_continuous(expand=c(0,0), breaks = seq(0, 100, by=25), labels=seq(0, 100, by=25)) +
  geom_text(aes(label= paste0("", n_birds, "")), y=106, nudge_x=0.025, hjust = 1, color="grey30", size=5.4) +
  coord_flip(clip = 'off') 


## SAVE ## 
ggsave("C:/Users/Martim Bill/Desktop/test/spp_cover_plot6.png", width=10, height=11, device="png")


#============================================================================================================

#### Individual plots for annual time, and population coverage (combined outside R) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
long <- tidyr::pivot_longer(sp_sum, cols=c("EEZ", "ABNJ", "unknown"), names_to="type") %>% group_by(name) %>% mutate(
  type = factor(type, levels = c("unknown", "EEZ", "ABNJ"))
)



# flip horizontally ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p <- ggplot(long, aes(x=reorder(name, desc(name)), y=value, fill=type)) +
  geom_col() + 
  # scale_fill_manual(values = c("unknown" = "grey80", "ABNJ" = "#e31a1c", "EEZ" = '#08519c'), labels=c("Unknown", "EEZ", "High seas")) + 
  scale_fill_manual(values = c("unknown" = "grey80", "ABNJ" = "grey10", "EEZ" = 'grey10')) + # combine EEZ and ABNJ
  # scale_fill_manual(values = c("unknown" = "grey80", "ABNJ" = "#af8dc3", "non_breeding" = '#08519c', "breeding" = '#e31a1c')) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ylab("% annual cycle time") +
  xlab(NULL)

p + theme(
  text = element_text(size=14),
  # axis.text.x = element_text( , hjust = 1, vjust=0.3, size = 16),
  axis.text.x = element_text(hjust=0.5, size = 13),
  axis.text.y = element_text(hjust = 0.5, size = 16),
  axis.title = element_text(size = 16),
  legend.position = "none",
  plot.margin=unit(c(1,1,1,0),"cm")
) + scale_x_discrete(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) + coord_flip()

## SAVE ## 
# ggsave(paste0(master_figs, "test/global_summaries/spp_global_time_horizontal.png"), width=10, height=11, device="png")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Bar chart of global population coverage per species ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sp_sum$Untracked <- abs(sp_sum$glob_pop_cov - 100)

# sp_sum$unknown <- ifelse(is.na(sp_sum$unknown) & !is.na(sp_sum$breeding), (100 - sp_sum$breeding), 
#   ifelse(is.na(sp_sum$breeding) & is.na(sp_sum$non_breeding), 100, sp_sum$unknown) 
# )


long <- tidyr::pivot_longer(sp_sum, cols=c("glob_pop_cov", "Untracked"), names_to="type") %>% group_by(name) %>% mutate(
  type = factor(type, levels = c("Untracked", "glob_pop_cov"))
)

long <- long %>% filter(name != "Diomedea epomophora" | name != "Southern Royal Albatross")

##  flip horizontally and reverse direction
p <- ggplot(long, aes(x=reorder(name, desc(name)), y=-value, fill=type)) +
  geom_col() + scale_fill_manual(values = c( "glob_pop_cov" = "grey10", "Untracked" = "grey80")) +
  ylab("% global population covered") +
  xlab(NULL)


p + theme(
  text = element_text(size=14),
  # axis.text.x = element_text( , hjust = 1, vjust=0.3, size = 16),
  axis.text.x = element_text( hjust=0.5, size = 13),
  axis.text.y = element_text(hjust=1, size = 14),
  axis.title = element_text(size = 16),
  legend.position = "none",
  plot.margin=unit(c(1,0,1,1),"cm")
) + scale_x_discrete(expand=c(0,0), name = "", position = "top") +
  scale_y_continuous(expand=c(0,0), breaks = seq(0, -100, by=-25), labels=seq(0, 100, by=25)) + coord_flip()

## SAVE ## 
ggsave(paste0(master, "test/global_summaries/spp_global_pop_cover_horizontal_reverse.png"), width=10, height=11, device="png")
