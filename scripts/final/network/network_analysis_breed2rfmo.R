#### Create network connecting origin country communities to high seas RFMOs ####

pacman::p_load(igraph, ggraph, tidyverse, ggplot2, stringr)

basin_class <- read.csv("data_test/basin_class_df.csv") # ocean basin classification

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

## Choose whether to analyse UK-assigned or Argentina-assigned data ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assign <- "UK"
# assign <- "ARG"

if(assign == "UK"){
  folder <- paste0(master, "glob_count_rfmo/")
  subfolders <- paste0(list.files(folder, full.names = T), "/")
} else if(assign == "ARG"){
  folder <- paste0(master, "data/analysis/ARG_assign/glob_count_rfmo/")
  subfolders <- paste0(list.files(folder, full.names = T), "/")
}

## Combining ~15 RFMO overlay results ~~~~~~~~~~~~~~~~~~~~~~~~~
# folder <- "data_test/globcnt_rfmo/"
# subfolders <- paste0(list.files(folder, full.names = T), "/")

rfmo_list <- vector("list", length(subfolders))

for(i in 1:length(subfolders)){
  
  rfmo_run <- list.files(folder)[i] # get name of RFMO which data was overlaid with

  one <- do.call("rbind", lapply(as.list(list.files(subfolders[i], full.names = T)), function(x) readRDS(x) ))
  
  one$rfmo_run <- rep(rfmo_run)
  
  rfmo_list[[i]] <- one
  
}

rfmo_df <- do.call("rbind", rfmo_list)

## adjust proportion of monthly time spent values by the total proportion of that month spent in high seas 
# globprop_rel == proportion of species' monthly High Seas time spent in RFMO x
# globprop_hs  == proportion of species' monthly time spent in High Seas in month 
# globprop_abs == proportion of species' monthly time spent in RFMO x
# NEED to get alltimes from 'network_analysis_breed2visit_month.r'
highseas <- alltimes %>% filter(jurisdiction=="High seas") %>% select(adj_site_name, scientific_name, month, globprop) %>% group_by(adj_site_name, scientific_name, month) %>% summarise(globprop = first(globprop))

rfmo_df <- merge(rfmo_df, highseas, by=c("scientific_name", "adj_site_name", "month")) %>% rename(globprop_rel = globprop.x, globprop_hs = globprop.y) %>% mutate(globprop_abs = globprop_hs * globprop_rel)

######## NETWORK!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# don't need to group_by 'rfmo_run' b/c no RFMO will be visited twice by same individual
visitsum <- rfmo_df %>% group_by(scientific_name, origin, jurisdiction) %>% summarise( 
  glob_ann_prop = sum(na.omit(globprop_abs)) / 12 
) %>% filter(!is.na(origin)) %>% filter(jurisdiction != "otherRFMO") #NOTE# need to make sure to earlier ID which "otherRMFO" are NO RFMO

origins <- unique(visitsum$origin)

### create an EDGELIST; adding an edge weight attribute ( AVG proportion of time spent by "community's" time spent)
# edgelist_full <- visitsum %>%
#   group_by(origin, jurisdiction) %>%
#   summarise(weight = sum(glob_ann_prop) / n_distinct(scientific_name)) %>%
#   ungroup() 

#                                                       ( SUM of proportion of time spent by "community's" time spent)
edgelist_full <- visitsum %>% 
  group_by(origin, jurisdiction) %>%
  summarise(
    weight = sum(glob_ann_prop),
    n_spp  = n_distinct(scientific_name)) %>%
  ungroup() %>%
  filter(!origin == jurisdiction) # remove self links
## filter EDGES to top N number of visited countries per origin
edgelist <- edgelist_full %>% group_by(origin) %>% arrange(desc(weight))


### create an NODELIST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
from <- rfmo_df %>% filter(jurisdiction %in% edgelist$jurisdiction) %>% 
  group_by(origin) %>% summarise(
    breed_rich = n_distinct(scientific_name) 
    ) %>% ungroup() %>% rename(label = origin) 

to <- rfmo_df %>% filter(jurisdiction %in% edgelist$jurisdiction ) %>% 
  group_by(jurisdiction) %>% # TO
  rename(label = jurisdiction) %>% 
  summarise(
    visit_rich = n_distinct(scientific_name)
    ) %>% ungroup() 

# nodelist with two nodes for highly visited origins (i.e. an breeding node and a visiting one)
nodelist <- plyr::rbind.fill(from, to) %>% arrange(label) %>% mutate( breed_node = ifelse(is.na(breed_rich), FALSE, TRUE) )

nodelist <- merge(nodelist, basin_class, by.x=c("label"), by.y = c("jurisdiction")) %>% mutate(
  ocean_basin = factor(ocean_basin, 
    levels = c("none", "Southern Ocean", "Indian Ocean", "shared (Pacific/Indian)", "Pacific Ocean", "shared (Atlantic/Pacific)", "Atlantic Ocean")) ) %>% 
  arrange(ocean_basin) # set up order of nodes by ocean basin levels

nodelist <- nodelist %>% mutate(id = 1:nrow(nodelist)) %>% filter(!is.na(label)) %>% dplyr::select(id, label, breed_rich, visit_rich, breed_node, ocean_basin, landlocked) # add numerical IDs for nodes



## make edgelist ids numeric, and labels attributes ##
breed_nodes <- nodelist %>% filter(breed_node == TRUE) %>% dplyr::select(id, label)
x <- merge(edgelist, breed_nodes, by.x=c("origin"), by.y=c("label"))

visit_nodes <- nodelist %>% filter(breed_node == FALSE) %>% dplyr::select(id, label)
xx <- merge(edgelist, visit_nodes, by.x=c("jurisdiction"), by.y=c("label"))

edgelist <- merge(x, xx, by=c("origin", "jurisdiction", "weight")) %>% rename(origin_id = id.x, jurisdiction_id = id.y) %>% dplyr::select(origin_id, jurisdiction_id, origin, jurisdiction, weight)

# igraph network object
routes_igraph <- graph_from_data_frame(d = edgelist, vertices = nodelist, directed = TRUE)
# plot(routes_igraph)

## plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

source("scripts/exploration/network_analysis_breed2visiting_layout.fxn.r")

l <- lay_breed2visit(routes_igraph, which_lay = "RFMO", plotit = F)


lay <- data.frame(x = l[, 1], y = l[, 2]) 

lay <- cbind(lay, nodelist)

# coordinates for Ocean basins boxes in plot
IboxO <- lay %>% filter( breed_node == TRUE & (ocean_basin %in% unique(str_subset(lay$ocean_basin, pattern = "Indian"))) ) %>% dplyr::select(x, y) %>% mutate(ocean_basin = rep("Indian Ocean"), origin = rep(TRUE))
IboxN <- lay %>% filter( breed_node == FALSE & (ocean_basin %in% unique(str_subset(lay$ocean_basin, pattern = "Indian"))) ) %>% dplyr::select(x, y) %>% mutate(ocean_basin = rep("Indian Ocean"), origin = rep(FALSE))

PboxO <- lay %>% filter( breed_node == TRUE & (ocean_basin %in% unique(str_subset(lay$ocean_basin, pattern = "Pacific"))) ) %>% dplyr::select(x, y) %>% mutate(ocean_basin = rep("Pacific Ocean"), origin = rep(TRUE))
PboxN <- lay %>% filter( breed_node == FALSE & (ocean_basin %in% unique(str_subset(lay$ocean_basin, pattern = "Pacific"))) ) %>% dplyr::select(x, y) %>% mutate(ocean_basin = rep("Pacific Ocean"), origin = rep(FALSE))

AboxO <- lay %>% filter( breed_node == TRUE & (ocean_basin %in% unique(str_subset(lay$ocean_basin, pattern = "Atlantic"))) ) %>% dplyr::select(x, y) %>% mutate(ocean_basin = rep("Atlantic Ocean"), origin = rep(TRUE))
AboxN <- lay %>% filter( breed_node == FALSE & (ocean_basin %in% unique(str_subset(lay$ocean_basin, pattern = "Atlantic"))) ) %>% dplyr::select(x, y) %>% mutate(ocean_basin = rep("Atlantic Ocean"), origin = rep(FALSE))

# need y-position of breed nodes
origin.y <- lay %>% filter(breed_node == TRUE) %>% summarise(y = first(y)) %>% pull(y)

boxes <- rbind.data.frame(IboxO, IboxN, PboxO, PboxN, AboxO, AboxN) %>% group_by(ocean_basin, origin) %>% mutate(
  xmin = min(x) - 1,
  xmax = max(x) + 1,
  ymin = ifelse(origin == TRUE, origin.y - 1, 0.2),       # conditionally set position of boxes behind origin/non-origin nodes
  ymax = ifelse(origin == TRUE, max(y) + .8, max(y) + 1)
) %>% summarise(
  xmin = first(xmin), xmax = first(xmax), ymin = first(ymin), ymax = first(ymax)
)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# PLOT # 
## to plot only top N connections (top 1, 2, etc) for illustrative purposes
edges_topn <- edgelist_full %>% group_by(origin) %>% arrange(desc(weight)) %>% top_n(3, weight)
maxweight <- ceiling(max(na.omit(edges_topn$weight)) * 100) 
routes_igraph2 <- delete.edges(routes_igraph, which(!E(routes_igraph)$weight %in% edges_topn$weight))

plot_igraph <- routes_igraph2

# getting nodes right (size and color)
# nodesize = breed_rich for breeding countries, visit_rich for others
lay$nodesize <- ifelse(is.na(lay$breed_rich), lay$visit_rich, lay$breed_rich)

lay$origin_label <- ifelse(lay$breed_node == T, "Breeding", "Visiting")

p2 <- ggraph(plot_igraph, layout = "manual", node.positions = lay) +
  geom_rect(data = boxes, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill = ocean_basin), alpha=0.6) +
  geom_edge_link(aes(width = weight*100), lineend = "round", colour = "black", show.legend = NA, alpha=0.9) +
  ## Width scale
  scale_edge_width(breaks = c(50, 100, 200, 300), limits = c(0, 327)) +
  geom_node_point(data = lay, aes(x=x, y=y, size = nodesize, color=origin_label)) + scale_size(
    limits = c(0,39), breaks = c(1,5,10,30), range=c(2,20)) +
  scale_color_manual(values = c("Breeding" = "gold", "Visiting" = "darkorchid"))  +
  ggforce::theme_no_axes() + theme(panel.border = element_blank()) + coord_cartesian(ylim=c(min(lay$y)-2, max(lay$y)+1 )) +
  # legends 
  guides(
    edge_width = guide_legend(title="Strength", order = 4),
    size       = guide_legend(title="Species richness", order = 3),
    fill       = guide_legend(title="Ocean basin", order = 2),
    color      = guide_legend(title="Jurisdiction", order = 1, override.aes = list(size=4))
  )
# p2

# full country names
p2 <- p2 + coord_cartesian(ylim=c(min(lay$y)-2.5, max(lay$y) + 4.5 )) +
  geom_text(
    data=subset(lay, breed_node==FALSE),
    aes(x=x, y=y, label = label),
    size=4.75,
    nudge_y = -1,
    hjust = 0,
    angle = -45, size = 6
  ) +
  geom_richtext(
    data=subset(lay, breed_node==TRUE),
    aes(x=x, y=y, label = label),
    nudge_y = +.5,
    hjust = 1,
    angle = -45, size = 6, 
    label.size = 0, label.r=unit(0.25, "lines"), label.color = NA, fill=alpha("white", 0)
  ) +
  ylim(c(-8, max(lay$y)+2)) +
  # legends
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16) 
  )

p2 + theme(legend.position = "none")



##Save##

# ggsave("figures/test/networks/breed2RFMO_full.png",
#   width=40, height=30, units="cm", dpi=250)
# ggsave("figures/test/networks/breed2RFMO_top1.png",
# width=40, height=30, units="cm", dpi=250)
# ggsave("figures/test/networks/breed2RFMO_noEDGES.png",
#   width=40, height=30, units="cm", dpi=250)

if(assign == "UK"){
  ggsave(paste0(master_figs, "networks/breed2RFMO_top3_nolegendXX.png"),
    width=40, height=16, units="cm", dpi=250) 
} else if(assign == "ARG"){
  ggsave(paste0(master_figs, "figures/ARG_assign/networks/breed2RFMO_top3_abb_nolegendX.png"),
    width=40, height=16, units="cm", dpi=250)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## abbreviate country names ##
lay$label <- basin_class$abb[match(lay$label, basin_class$jurisdiction)]

p +
  geom_text(
    data=subset(lay, breed_node==FALSE),
    aes(x=x, y=y, label = label),
    size=6.5,
    nudge_y = -1.2, 
    hjust = 0.5,
    angle = 0
  ) +
  geom_label(
    data=subset(lay, breed_node==TRUE),
    aes(x=x, y=y, label = label),
    nudge_y = + 1.1,
    hjust = 0.5,
    size = 8.5,
    alpha = 0.7, label.size = 0, label.r=unit(0.5, "lines") # label boxes
  ) +
  theme(legend.position = "none")



##Save##

# ggsave("figures/test/networks/breed2RFMO_full_abb.png",
#   width=50, height=20, units="cm", dpi=250)
# ggsave("figures/networks/networks/breed2RFMO_top3_abb.png",
#   width=50, height=20, units="cm", dpi=250)
# ggsave("figures/networks/breed2RFMO_top3_abb_nolegendX.png",
#   width=50, height=20, units="cm", dpi=250)
# ggsave("figures/test/networks/breed2RFMO_noEDGES_abb.png",
  # width=50, height=20, units="cm", dpi=250)


## SAVE ##
if(assign == "UK"){
  ggsave(paste0(master_figs, "networks/breed2RFMO_top3_abb_nolegendX.png"),
    width=50, height=20, units="cm", dpi=250)
} else if(assign == "ARG"){
  ggsave(paste0(master_figs, "figures/ARG_assign/networks/breed2RFMO_top3_abb_nolegendX.png"),
    width=50, height=20, units="cm", dpi=250)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save data frame of top or all connections ## 

edges_topn_summ <- nodelist %>% group_by(label) %>% summarise(breed_rich = first(breed_rich)) %>% right_join(edges_topn, by=c("label"="origin")) %>% dplyr::select(label, breed_rich, jurisdiction, weight, n_spp) %>% rename(origin=label) %>% arrange(origin, desc(weight)) %>% mutate(weight=weight*100)

edgelist_full_summ <- nodelist %>% group_by(label) %>% summarise(breed_rich = first(breed_rich)) %>% right_join(edgelist_full, by=c("label"="origin")) %>% dplyr::select(label, breed_rich, jurisdiction, weight, n_spp) %>% rename(origin=label) %>% arrange(origin, desc(weight)) %>% mutate(weight=weight*100)


## SAVE ##
if(assign == "UK"){
  write.csv(edges_topn_summ, paste0(master, "summary_tables/network_topconnex_country2RFMO.csv"), row.names = F)
  write.csv(edgelist_full_summ, paste0(master, "summary_tables/network_allconnex_country2RFMO.csv"), row.names = F)
} else if(assign == "ARG"){
  write.csv(edges_topn_summ, paste0(master, "ARG_assign/summary_tables/network_topconnex_country2RFMO.csv"), row.names = F)
  write.csv(edgelist_full_summ, paste0(master, "ARG_assign/summary_tables/network_allconnex_country2RFMO.csv"), row.names = F)
}
