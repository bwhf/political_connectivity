## Network plot of time spent in visited (non-origin countries - highlighting important relationships between origin and non-origin jurisdictios ###) ####

pacman::p_load(igraph, ggplot2, tidyverse, ggraph, dplyr, stringr, ggtext)


basin_class <- read.csv("data_test/basin_class_df.csv", stringsAsFactors = F)


## Choose whether to use high threshold or low threshold data (i.e. >1 bird per month) #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
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
  folder <- paste0(master, "glob_count/")
} else if(assign == "ARG"){
  folder <- paste0(master, "ARG_assign/glob_count/")

}

## Network analysis ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # folder <- "data_test/globcnt_month/" # OLD
# # folder <- "data_test/globcnt_eez/"
# folder <- "data/analysis/glob_count/"

files <- list.files(folder, full.names = T)

alltimes <- do.call("rbind", lapply(files, function(x) readRDS(x)))

head(alltimes)

#######~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

netsum <- alltimes %>% filter(!is.na(origin))

# summarize global time spent by TOTAL population breeding in a country (origin)
visitsum <- netsum %>% group_by(scientific_name, adj_site_name, month, origin, jurisdiction) %>% summarise(
  globprop = first(globprop)) %>% group_by(scientific_name, origin, jurisdiction) %>%  summarise(
  glob_ann_prop = sum(na.omit(globprop)) / 12 
) %>% filter(!is.na(origin))



### filter out connections below a certain threshold 
# visitsum <- visitsum %>% filter(glob_ann_prop > 0.009) # 
## filter to top N number of visited countries per origin
# visitsum <- visitsum %>% group_by(origin) %>% arrange(desc(glob_ann_prop)) %>% top_n(5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Create network which highlights top N connections (making visiting nodes for origins too ( excluding inter-origin connections) ) ####

origins <- unique(visitsum$jurisdiction[which(visitsum$jurisdiction %in% visitsum$origin)])

### create an edgelist; adding an edge weight attribute ( AVG proportion of time spent by "community's" time spent)
# *** THIS METHOD IS MISSING 0s!! ***
# edgelist_full <- visitsum %>%
#   group_by(origin, jurisdiction) %>%
#   summarise(weight = sum(glob_ann_prop) / n_distinct(scientific_name)) %>%
#   ungroup() %>%
#   filter(!origin == jurisdiction) # remove self links
# #                                                     ( SUM of proportion of time spent by "community's" time spent)

edgelist_full <- visitsum %>%
  group_by(origin, jurisdiction) %>%
  summarise(
    weight = sum(glob_ann_prop),
    n_spp  = n_distinct(scientific_name)) %>%
  ungroup() %>%
  filter(!origin == jurisdiction) # remove self links

## filter EDGES to top N number of visited countries per origin
edgelist <- edgelist_full %>% group_by(origin) %>% arrange(desc(weight)) %>% top_n(5, weight)



### create an nodelist
# define two classes of node (from and two), adding an attribute (e.g. breeding spp. richness) to the from nodes
from <- netsum %>% filter(jurisdiction %in% edgelist$jurisdiction) %>% group_by(origin) %>% summarise(breed_rich = n_distinct(scientific_name) ) %>% rename(label = origin) 
to <- netsum %>% filter(jurisdiction %in% edgelist$jurisdiction ) %>% group_by(jurisdiction) %>% rename(label = jurisdiction) %>% summarise(visit_rich = n_distinct(scientific_name))

# nodelist with two nodes for highly visited origins (i.e. an breeding node and a visiting one)
nodelist <- plyr::rbind.fill(from, to) %>% arrange(label) %>% mutate( breed_node = ifelse(is.na(breed_rich), FALSE, TRUE) )

nodelist <- merge(nodelist, basin_class, by.x=c("label"), by.y = c("jurisdiction")) %>% mutate(
  ocean_basin = factor(ocean_basin, 
    levels = c("none", "Southern Ocean", "Indian Ocean", "shared (Pacific/Indian)", "Pacific Ocean", "shared (Atlantic/Pacific)", "Atlantic Ocean")) ) %>% 
  arrange(ocean_basin) # set up order of nodes by ocean basin levels

nodelist <- nodelist %>% mutate(id = 1:nrow(nodelist)) %>% filter(!is.na(label)) %>% dplyr::select(id, label, breed_rich, visit_rich, breed_node, ocean_basin, landlocked) # add numerical IDs for nodes

# add whether jurisdiction is an origin as attribute
nodelist$is_origin <- nodelist$label %in% origins 


## make edgelist ids numeric, and labels attributes ##
breed_nodes <- nodelist %>% filter(breed_node == TRUE) %>% dplyr::select(id, label)
x <- merge(edgelist, breed_nodes, by.x=c("origin"), by.y=c("label"))

visit_nodes <- nodelist %>% filter(breed_node == FALSE) %>% dplyr::select(id, label)
xx <- merge(edgelist, visit_nodes, by.x=c("jurisdiction"), by.y=c("label"))

edgelist <- merge(x, xx, by=c("origin", "jurisdiction", "weight")) %>% rename(origin_id = id.x, jurisdiction_id = id.y) %>% dplyr::select(origin_id, jurisdiction_id, origin, jurisdiction, weight)
  
# igraph network object
routes_igraph <- graph_from_data_frame(d = edgelist, vertices = nodelist, directed = TRUE)

## Create layout from igraph object ##

source("scripts/exploration/network_analysis_breed2visiting_layout.fxn.r")

l <- lay_breed2visit(routes_igraph, which_lay = "EEZ", plotit = F)

## plotting ## 


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
  ymin = ifelse(origin == TRUE, origin.y - 2.4, -0.2),       # conditionally set position of boxes behind origin/non-origin nodes
  ymax = ifelse(origin == TRUE, max(y) + 1.2, max(y) + 2.4)
) %>% summarise(
  xmin = first(xmin), xmax = first(xmax), ymin = first(ymin), ymax = first(ymax)
)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# PLOT # 
## to plot only top N connections (top 1, 2, etc) for illustrative purposes
edges_topn <- edgelist_full %>% group_by(origin) %>% arrange(desc(weight)) %>% top_n(5, weight)
maxweight <- ceiling(max(na.omit(edges_topn$weight)) * 100) # 327

routes_igraph2 <- delete.edges(routes_igraph, which(!E(routes_igraph)$weight %in% edges_topn$weight))

plot_igraph <- routes_igraph2

# getting nodes right (size and color)
# nodesize = breed_rich for breeding countries, visit_rich for others
lay$nodesize <- ifelse(is.na(lay$breed_rich), lay$visit_rich, lay$breed_rich)

lay$origin_label <- ifelse(lay$breed_node == T, "Breeding", "Visiting")


p1 <- ggraph(plot_igraph, layout = "manual", node.positions = lay) +
  geom_rect(data = boxes, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill = ocean_basin), alpha=0.6) +
  geom_edge_link(aes(width = weight*100), lineend = "round", colour = "black", show.legend = NA, alpha=0.9) +
  ## Width scale
  scale_edge_width(breaks = c(50, 100, 200, 300), limits = c(0, 327)) +
  geom_node_point(data = lay, aes(x=x, y=y, size = nodesize, color=origin_label)) + scale_size(
    limits = c(0,39), breaks = c(1,5,10,30), range=c(2,20)) +
  scale_color_manual(values = c("Breeding" = "gold", "Visiting" = "darkorchid"))  +
  ggforce::theme_no_axes() + theme(panel.border = element_blank()) +
  # legends 
  guides(
    edge_width = guide_legend(title="Strength", order = 1),
    size  = guide_legend(title="Species richness", order = 2),
    fill = guide_legend(title="Ocean basin", order = 3),
    color = guide_legend(title="Jurisdiction", order = 4, override.aes = list(size=4))
  )
# p1

# shorten disputed area name 
lay$label <- ifelse(lay$label == "Disputed (Japan/Russia)", "Japan/Russia", lay$label)

# dev.new()
p1 <- p1 +
  geom_text(
    data=subset(lay, label == "High seas"),
    aes(x=x, y=y, label = label),
    nudge_y = +2,
    hjust = 0.5, size=6
  ) +
  geom_text(
    data=subset(lay, breed_node==FALSE & label != "High seas"),
    aes(x=x, y=y, label = label),
    nudge_y = -1.3,
    # nudge_x = .5,
    hjust = 0,
    angle = -45, size = 5.5
  ) +
  geom_richtext(
    data=subset(lay, breed_node==TRUE & label != "High seas"),
    aes(x=x, y=y, label = label),
    nudge_y = +1,
    hjust = 1,
    angle = -45, size = 6, 
    label.size = 0, label.r=unit(0.75, "lines"), label.color = NA, fill=alpha("white", 0.7) # label boxes
  ) +
  ylim(c(-8, max(lay$y)+2)) +
  # legends
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16) 
  )

p1 + theme(legend.position = "none")


##Save##

# ggsave("figures/test/networks/breed2visit_top5_timeAVG.png",
# width=40, height=30, units="cm", dpi=250)
# ggsave("figures/test/networks/breed2visit_top1_timeSUM.png",
#   width=40, height=30, units="cm", dpi=250)
# ggsave("figures/test/networks/breed2visit_NOEDGES_top5_timeSUM.png",
  # width=40, height=30, units="cm", dpi=250)

if(assign == "UK"){
  ggsave(paste0(master_figs, "networks/breed2visit_top5_timeSUMX.png"),
    width=40, height=30, units="cm", dpi=250)
} else if(assign == "ARG"){
  ggsave(paste0(master_figs, "ARG_assign/networks/breed2visit_top5_timeSUM_abbX.png"),
    width=40, height=30, units="cm", dpi=250)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
## Change country names - shorten or abbreviate ##
lay$label <- ifelse(lay$label == "Disputed (Japan/Russia)", "Japan/Russia", lay$label)


# dev.new()
p +
  geom_text(
    data=subset(lay, label == "High seas"),
    aes(x=x, y=y, label = label),
    nudge_y = + 2.2,
    hjust = 0.5,
    size = 7
  ) +
  geom_text(
    data=subset(lay, breed_node==FALSE & label != "High seas"),
    aes(x=x, y=y, label = label),
    nudge_y = -1.3, 
    hjust = 0,
    size = 5,
    angle = -90
  ) +
  geom_label(
    data=subset(lay, breed_node==TRUE & label != "High seas"),
    aes(x=x, y=y, label = label),
    nudge_y = +2,
    hjust = 0.5,
    size = 7,
    alpha = 0.7, label.size = 0, label.r=unit(0.5, "lines") # label boxes
  ) +
  ylim(c(-8, max(lay$y)+2.2)) +
  theme(legend.position = "none")


## SAVE ## 
# ggsave("figures/test/networks/breed2visit_top5_timeSUM_abb.png",
#   width=40, height=30, units="cm", dpi=250)
# ggsave("figures/networks/breed2visit_top5_timeSUM_abb_nolegend.png",
#   width=40, height=30, units="cm", dpi=250)


## SAVE ##
if(assign == "UK"){
  ggsave(paste0(master_figs, "networks/breed2visit_top5_timeSUM_abbX.png"),
    width=40, height=30, units="cm", dpi=250)
} else if(assign == "ARG"){
  ggsave(paste0(master_figs, "ARG_assign/networks/breed2visit_top5_timeSUM_abbX.png"),
    width=40, height=30, units="cm", dpi=250)
}


## Save data frame of top or all connections ## 

# edgelist_full %>% right_join(nodelist, by=c("origin" = "label"))

edges_topn_summ <- nodelist %>% group_by(label) %>% summarise(breed_rich = first(breed_rich)) %>% right_join(edges_topn, by=c("label"="origin")) %>% dplyr::select(label, breed_rich, jurisdiction, weight, n_spp) %>% rename(origin=label) %>% arrange(origin, desc(weight)) %>% mutate(weight=weight*100)

edgelist_full_summ <- nodelist %>% group_by(label) %>% summarise(breed_rich = first(breed_rich)) %>% right_join(edgelist_full, by=c("label"="origin")) %>% dplyr::select(label, breed_rich, jurisdiction, weight, n_spp) %>% rename(origin=label) %>% arrange(origin, desc(weight)) %>% mutate(weight=weight*100)

if(assign == "UK"){
  write.csv(edges_topn_summ, paste0(master, "summary_tables/network_topconnex_country2country.csv"), row.names = F)
  write.csv(edgelist_full_summ, paste0(master, "summary_tables/network_allconnex_country2country.csv"), row.names = F)
} else if(assign == "ARG"){
  write.csv(edges_topn_summ, paste0(master, "ARG_assign/summary_tables/network_topconnex_country2country.csv"), row.names = F)
  write.csv(edgelist_full_summ, paste0(master, "ARG_assign/summary_tables/network_allconnex_country2country.csv"), row.names = F)
}


