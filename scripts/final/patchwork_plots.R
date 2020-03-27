# Trying to make map layouts in R # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load(patchwork)

m1 <- p # map of fixcout density
m2 <- p # map of species richness
m3 <- p # map of time spent density

lay <- m1 / m2 / m3 + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 35), plot.tag.position = c(0.01, .17))
lay <- m1 / m2 / m3 + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 35), plot.tag.position = c(.95, .95))
ggsave("C:/Users/Martim Bill/Desktop/test/plotD5.png", plot=lay, width=25, height=45, units="cm", dpi=250)

lay <- m1 / m2 / m3 + plot_layout(guides = 'collect') & theme(legend.justification = "left", aspect.ratio=1)
ggsave("C:/Users/Martim Bill/Desktop/test/plotD4.png", plot = lay, width=25, height=45, units="cm", dpi=250)

## Bar charts ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p1 <- p # richness, ordered by breeding richness
p2 <- p # richness , ordered by total richness
p3 <- p # time spent

lay <- (p1 / p2) 
ggsave("C:/Users/Martim Bill/Desktop/test/plotE1.png", plot = lay, width=30, height=40, units="cm", dpi=250)

lay <- (p1 / p2) - p3
ggsave("C:/Users/Martim Bill/Desktop/test/plotE3.png", plot = lay, width=65, height=40, units="cm", dpi=250)

lay <- (p1 / p2) - p3 + plot_layout(guides = 'collect')
ggsave("C:/Users/Martim Bill/Desktop/test/plotE6.png", plot = lay, width=70, height=40, units="cm", dpi=250)

lay + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 55))
lay + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 55), plot.tag.position = c(.0, .97))
lay + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 55), plot.tag.position = c(.95, .95))

ggsave("C:/Users/Martim Bill/Desktop/test/plotE11.png", width=70, height=40, units="cm", dpi=250)

dev.new()
(p1 / p2 + plot_annotation(tag_levels = 'A')) - p3 + plot_annotation(tag_levels = 'A')
