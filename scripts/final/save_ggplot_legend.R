## save legend for any ggplot object (for making plots pretty outside R) ## 

leg <- ggpubr::get_legend(p)  # redundant
# leg <- cowplot::get_legend(p)


# Convert to a ggplot and print
leg <- ggpubr::as_ggplot(leg)
leg

# ggsave("figures/legends/network_rfmo_legend.png", width = 1.5, height=5.5)
# ggsave("figures/legends/network_rfmo_legend.pdf", width = 1.5, height=5.5, dpi=600)
ggsave("figures/legends/network_legend.png", width = 1.5, height=5.5)
ggsave("figures/legends/network_legend.pdf", width = 1.5, height=5.5, dpi=600)

# ggsave("figures/legends/network_rfmo_legend.pdf", width = 1, height=.5, dpi=600)   # abundance figure
# ggsave("figures/legends/legend.pdf", width = 1, height=.5, dpi=600)   # abundance figure
# ggsave("figures/legends/legend.png", width = 1.5, height=.6, dpi=600) # abundance figure
# ggsave("figures/legends/legendX.pdf", width = .6, height=.6, dpi=600)   # richness figure
# ggsave("figures/legends/legendX.png",width = 1.2, height=.8, dpi=600)      # richness figure
# 
