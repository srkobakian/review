# Figures
library(ggplot2)
library(grid)
library(cowplot)

# Import the images
plot1 <- png::readPNG('cancer_map_images/choropleths/21.choropleth.png')
plot2 <- png::readPNG('cancer_map_images/choropleths/3.choropleth.png')
plot3 <- png::readPNG('cancer_map_images/choropleths/25.choropleth.png')
plot4 <- png::readPNG('cancer_map_images/choropleths/aca.choropleth.png')
plot5 <- png::readPNG('cancer_map_images/choropleths/7.choropleth.png')
plot6 <- png::readPNG('cancer_map_images/choropleths/23.choropleth.png')
plot7 <- png::readPNG('cancer_map_images/choropleths/26.choropleth.png')

ggdraw(xlim = c(0,1), ylim = c(0,1)) +
  draw_plot(rasterGrob(plot1), 0, .75, 0.33, .26) +
  draw_plot(rasterGrob(plot2), 0.33, 0.75, 0.33, .25) +
  draw_plot(rasterGrob(plot3), 0.66, 0.5, .33, .5) +
  draw_plot(rasterGrob(plot4), 0, 0.5, .33, .25) +
  draw_plot(rasterGrob(plot5), 0.33, 0.5, .33, .25) +
  draw_plot(rasterGrob(plot6), 0, 0, .66, .5) +
  draw_plot(rasterGrob(plot7), .66, 0, .33, .5) +
  draw_plot(rasterGrob(plot8), .66, 0, .33, .5) +
  draw_plot_label(c("a", "b", "c", "d", "e", "f", "g"), 
    c(0.025, 0.33, 0.66, 0.025, 0.33, 0.025, 0.66), 
    c(0.99, 0.99, 0.99, 0.74, 0.74, 0.46, 0.46), size = 20)

ggsave(filename = "figures/choropleth_grid.png", 
  device = "png", width = 12, height = 10, dpi = 300)




g1 <- png::readPNG("figures/ggcont.png")
g2 <- png::readPNG("figures/ggncont.png")
g3 <- png::readPNG("figures/ggdorl.png")
g4 <- png::readPNG("figures/gghexmap.png")

load("figures/usa_legend.rda")

ggdraw() +
  draw_plot(rasterGrob(g1), 0, .55, 0.5,  0.45) +
  draw_plot(rasterGrob(g2), 0.5, 0.55, 0.5, 0.45) +
  draw_plot(rasterGrob(g3), 0.0, 0.1, 0.5, 0.45) +
  draw_plot(rasterGrob(g4), 0.5, 0.1, 0.5, 0.45) +
  draw_plot(usa_legend, 0, 0, 1, 0.1, scale = 2) +
  draw_plot_label(c("a", "b", "c", "d"), 
    c(0, .5, 0, 0.5), 
    c(1, 1, .5, .5), size = 20)


ggsave(filename = "figures/usa_grid.png", 
  device = "png", width = 12, height = 10, dpi = 300)



aus1 <- png::readPNG("figures/aus_ggcont.png")
aus2 <- png::readPNG("figures/aus_ggncont.png")
aus3 <- png::readPNG("figures/aus_ggdorl.png")
aus4 <- png::readPNG("figures/aus_gghexmap.png")
load("aus_legend.rda")
load("figures/aus_legend.rda")


ggdraw(xlim = c(0,1), ylim = c(0,1.2)) +
  draw_plot(rasterGrob(aus1), 0, .65, 0.5, 0.50) +
  draw_plot(rasterGrob(aus2), 0.5, 0.65, 0.5, 0.50) +
  draw_plot(rasterGrob(aus3), 0.0, 0.1, 0.5, 0.55) +
  draw_plot(rasterGrob(aus4), 0.5, 0.1, 0.5, 0.55) +
  draw_plot(aus_legend, 0, 0, 1, .1) +
  draw_plot_label(c("a", "b", "c", "d"), 
    c(0, .5, 0, 0.5), 
    c(1.15, 1.15, .6, .6), size = 20)

ggsave(filename = "figures/auscartograms.png", device = "png", 
  width = 12, height = 9, dpi = 300)


i1 <- png::readPNG('cancer_map_images/interactivity/3.interactivity.png') # b in choro grid
i2 <- png::readPNG('cancer_map_images/interactivity/15.interactivity.png') # d in choro grid
i3 <- png::readPNG('cancer_map_images/interactivity/4.interactivity.png')
i4 <- png::readPNG('cancer_map_images/interactivity/5.interactivity.png')
i5 <- png::readPNG('cancer_map_images/interactivity/7.interactivity.png') # e in choro grid

ggdraw(xlim = c(0,1), ylim = c(0,1)) +
  draw_plot(rasterGrob(i1), 0,  0.13,   .28,  .83) + #a
  draw_plot(rasterGrob(i2), 0.32, 0.56, .68, .4) + #b
  draw_plot(rasterGrob(i3), 0.32, 0.41, .68, .13) + #c
  draw_plot(rasterGrob(i4), 0.32, 0.14, .68, .25) + #d
  draw_plot(rasterGrob(i5), 0, 0, 1, .13) + #e
  draw_plot_label(c("i", "ii", "iii", "iv", "v"), 
    c(0.01, 0.3, 0.3, 0.3, 0.01), 
    c(0.99, 0.99, 0.57, 0.40, 0.14), size = 20)

ggsave(filename = "figures/interacting.png", device = "png", 
  width = 12, height = 9, dpi = 300)


a1 <- png::readPNG("cancer_map_images/animation/23.animation.png")
a2 <- png::readPNG("cancer_map_images/animation/2.animation.png")

ggdraw(xlim = c(0, 1), ylim = c(0, 0.45)) +
  draw_plot(rasterGrob(a1), 0, 0, 0.3, 0.4) +
  draw_plot(rasterGrob(a2), 0.3, 0, 0.7, 0.4) +
  draw_plot_label(c("I", "II"), 
    c(0, .3), c(0.45, 0.45), size = 20)

ggsave(filename = "figures/animating.png", device = "png", 
  width = 12, height = 7, dpi = 300)