#USA Cancer Data

#  United States Cancer Statistics: Data Visualizations
#The official federal statistics on cancer incidence and deaths, produced by the Centers for Disease Control and Prevention (CDC) and the National Cancer Institute (NCI).

# https://gis.cdc.gov/cancer/USCS/DataViz.html


# United States
# Rate of New Cancers
# Female and Male
# Lung and Bronchus
# 2012-2016

# Export

library(sugarbag)
library(tidyverse)
library(spData)
library(ggthemes)
library(maptools)
library(sf)
library(cartogram)

# read in data
cancer <- read_csv("data/USCSlung.csv") %>% rename(NAME = Area)


# Join polygons to data
data(us_states)
cancer <- left_join(us_states, cancer, by = c("NAME"))
cancer <- st_transform(cancer, 3857)

b <- st_bbox(cancer)

# Histogram
ggplot(cancer, aes(x = total_pop_15)) + geom_histogram() 
ggplot(cancer, aes(x = total_pop_15 )) + geom_density()

title <- ""
  #"Average rate of incidence for lung and bronchus for females and males in the United States 2012-2016, \n where the size of each state has been adjusted for population"

###############################################################################
# Create a choropleth
ggchoro <- ggplot(cancer) + 
  geom_sf(aes(fill = AgeAdjustedRate)) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  ggtitle(title) +
  coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() +
  theme(legend.position ="bottom")
ggsave(filename = "figures/ggchoro.png", device = "png", dpi = 300,  width = 12, height = 6)
ggchoro

###############################################################################
# Cartograms

# Contiguous Cartograms
cont <- cartogram_cont(cancer,
  weight = "total_pop_15") %>% st_as_sf()
ggcont <- ggplot(cont) + 
  geom_sf(aes(fill = AgeAdjustedRate)) + 
  ggtitle(title) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() +   theme(legend.position ="bottom")
ggsave(filename = "figures/ggcont.png", device = "png", dpi = 300,  width = 12, height = 6)
ggcont

###############################################################################
# Non - Contiguous Cartograms
# Needs a scaling factor

cancer <- cancer %>% mutate(sva = sqrt(as.numeric(total_pop_15/AREA)))
cancer %>% 
  ggplot(.) +
  geom_density(aes(x = sva)) + geom_vline(aes(xintercept = 5))

# The state of Vermonet is used as the anchor unit

ncont <- cartogram_ncont(cancer, k = 1/5,
  weight = "total_pop_15") %>% st_as_sf()
ggncont <- ggplot(ncont) + 
  geom_sf(aes(fill = AgeAdjustedRate)) + 
  geom_sf(data=cancer, fill = NA, colour = "grey") +
  ggtitle(title) + 
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() + theme(legend.position ="bottom")
ggsave(filename = "figures/ggncont.png", device = "png", dpi = 300,  width = 12, height = 6)
ggncont


###############################################################################
# Non - Contiguous Dorling Cartograms
dorl <- cartogram_dorling(cancer,
  weight = "total_pop_15") %>% st_as_sf()
ggdorl <- ggplot(dorl) + 
  geom_sf(aes(fill = AgeAdjustedRate)) + 
  ggtitle(title) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() + theme(legend.position ="bottom")
ggsave(filename = "figures/ggdorl.png", device = "png", dpi = 300,  width = 12, height = 6)
ggdorl



# Centroid Cartograms
# cancer <- cancer %>% 
#   sf::st_centroid(geometry) %>% 
#   sf::st_coordinates() %>% as_tibble() %>% bind_cols(cancer, .)
# 
# ggdot <- ggplot(cancer) + 
#   geom_point(aes(X,Y, colour = AgeAdjustedRate)) + 
#   geom_sf(data=cancer, fill = NA, colour = "grey") +
#   ggtitle(title) +
#   scale_colour_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
#   coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
#   theme_void() + theme(legend.position ="bottom")
# ggsave(filename = "figures/ggdot.png", device = "png", dpi = 300,  width = 12, height = 6)
# ggdot


###############################################################################
# Choropleth projections
# 3857, 2163, 4326, 2955
ggchoro1 <- ggplot(st_transform(cancer, 3857)) + 
  geom_sf(aes(fill = AgeAdjustedRate)) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  ggtitle("a. The United States using EPSG: 3857") +
  theme_void()+ guides(fill = FALSE)

ggchoro2 <- ggplot(st_transform(cancer, 2163)) + 
  geom_sf(aes(fill = AgeAdjustedRate)) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  ggtitle("b. The United States using EPSG: 2163") +
  theme_void()+ guides(fill = FALSE)

ggchoro3 <- ggplot(st_transform(cancer, 4326)) + 
  geom_sf(aes(fill = AgeAdjustedRate)) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  ggtitle("c. The United States using EPSG: 4326") +
  theme_void()+ guides(fill = FALSE)

ggchoro4 <- ggplot(st_transform(cancer, 2955)) + 
  geom_sf(aes(fill = AgeAdjustedRate)) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  ggtitle("d. The United States using EPSG: 2955") +
  theme_void() + guides(fill = FALSE)

ggchoroCRS <- gridExtra::grid.arrange(ggchoro1, ggchoro2, ggchoro3, ggchoro4)

ggsave(filename = "figures/ggchoroCRS.png", plot = ggchoroCRS,
  device = "png", dpi = 300,  width = 12, height = 6)


###############################################################################
#Tilegram
library(statebins)

## US example
ggtilegram <- left_join(adat, cancer %>% rename(state = NAME)) %>%
  statebins(
    value_col = "AgeAdjustedRate", 
    name = "AgeAdjustedRate"
  ) +
  labs(title = "") + 
  theme_void() + theme(legend.position ="bottom") +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1)

ggsave(filename = "figures/ggtilegram.png", plot = ggtilegram,
  device = "png", dpi = 300,  width = 12, height = 6)




#####
# Choropleth grid

library(grid)
library(png)
library(gridExtra)

plot1 <- png::readPNG('cancer_map_images/chloropleths/1.png')
plot2 <- png::readPNG('cancer_map_images/chloropleths/14.chloropleth1.png')
plot3 <- png::readPNG('cancer_map_images/chloropleths/15.chloropleth.png')
plot4 <- png::readPNG('cancer_map_images/chloropleths/3.chloropleth.png')
plot5 <- png::readPNG('cancer_map_images/chloropleths/5.chloropleth.png')
plot6 <- png::readPNG('cancer_map_images/chloropleths/6.chloropleth.png')
plot6 <- png::readPNG('cancer_map_images/chloropleths/7.chloropleth.png')
plot7 <- png::readPNG('cancer_map_images/chloropleths/8.chloropleth.png')
plot8 <- png::readPNG('cancer_map_images/chloropleths/20.chloropleth1.png')
plot9 <- png::readPNG('cancer_map_images/chloropleths/21.chloropleth2.png')
plot10 <- png::readPNG('cancer_map_images/chloropleths/23.chloropleth.png')
plot11 <- png::readPNG('cancer_map_images/chloropleths/24.chloropleth.png')
plot12 <- png::readPNG('cancer_map_images/chloropleths/25.chloropleth.png')
plot13 <- png::readPNG('cancer_map_images/chloropleths/26.chloropleth.png')
plot14 <- png::readPNG('cancer_map_images/chloropleths/27.chloropleth.png')
plot15 <- png::readPNG('cancer_map_images/chloropleths/29.chloropleth.png')
plot16 <- png::readPNG('cancer_map_images/chloropleths/30.chloropleth.png')


chloropleth_grid <- gridExtra::grid.arrange(grid::rasterGrob(plot1),grid::rasterGrob(plot2), grid::rasterGrob(plot3), 
  grid::rasterGrob(plot4), grid::rasterGrob(plot5), grid::rasterGrob(plot6),
  grid::rasterGrob(plot7), grid::rasterGrob(plot8), grid::rasterGrob(plot9),
  grid::rasterGrob(plot10),grid::rasterGrob(plot11), grid::rasterGrob(plot12), 
  grid::rasterGrob(plot13), grid::rasterGrob(plot14), grid::rasterGrob(plot15), 
  grid::rasterGrob(plot16), nrow=4)


ggsave(filename = "figures/choropleth_grid.png", plot = choropleth_grid, device = "png", dpi = 300,  width = 12, height = 6)
