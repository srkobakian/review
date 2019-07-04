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

# Create a choropleth
ggchoro <- ggplot(cancer) + 
  geom_sf(aes(fill = AgeAdjustedRate)) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  ggtitle(title) +
  coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() +
  theme(legend.position ="bottom")
ggsave(filename = "figures/ggchoro.png", device = "png", width = 12, height = 6)
ggchoro

# Cartograms
#install.packages("cartogram")
library(cartogram)

# Contiguous Cartograms
cont <- cartogram_cont(cancer,
  weight = "total_pop_15") %>% st_as_sf()
ggcont <- ggplot(cont) + 
  geom_sf(aes(fill = AgeAdjustedRate)) + 
  ggtitle(title) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() +   theme(legend.position ="bottom")
ggsave(filename = "figures/ggcont.png", device = "png", width = 12, height = 6)
ggcont

# Non - Contiguous Cartograms
ncont <- cartogram_ncont(cancer,
  weight = "total_pop_15") %>% st_as_sf()
ggncont <- ggplot(ncont) + 
  geom_sf(aes(fill = AgeAdjustedRate)) + 
  ggtitle(title) + 
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() + theme(legend.position ="bottom")
ggsave(filename = "figures/ggncont.png", device = "png", width = 12, height = 6)
ggncont

# Non - Contiguous Dorling Cartograms
dorl <- cartogram_dorling(cancer,
  weight = "total_pop_15") %>% st_as_sf()
ggdorl <- ggplot(dorl) + 
  geom_sf(aes(fill = AgeAdjustedRate)) + 
  ggtitle(title) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() + theme(legend.position ="bottom")
ggsave(filename = "figures/ggdorl.png", device = "png", width = 12, height = 6)
ggdorl



# Choropleth projections
# 3857, 2163, 4326, 2955
ggchoro1 <- ggplot(st_transform(cancer, 3857)) + 
  geom_sf(aes(fill = AgeAdjustedRate)) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  ggtitle("The United States using ESPG: 3857") +
  theme_void()+ guides(fill = FALSE)

ggchoro2 <- ggplot(st_transform(cancer, 2163)) + 
  geom_sf(aes(fill = AgeAdjustedRate)) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  ggtitle("The United States using ESPG: 2163") +
  theme_void()+ guides(fill = FALSE)

ggchoro3 <- ggplot(st_transform(cancer, 4326)) + 
  geom_sf(aes(fill = AgeAdjustedRate)) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  ggtitle("The United States using ESPG: 4326") +
  theme_void()+ guides(fill = FALSE)

ggchoro4 <- ggplot(st_transform(cancer, 2955)) + 
  geom_sf(aes(fill = AgeAdjustedRate)) +
  scale_fill_distiller(type = "seq", palette = "YlOrRd",  direction = 1) + 
  ggtitle("The United States using ESPG: 2955") +
  theme_void() + guides(fill = FALSE)

ggchoroCRS <- gridExtra::grid.arrange(ggchoro1, ggchoro2, ggchoro3, ggchoro4)

ggsave(filename = "figures/ggchoroCRS.png", plot = ggchoroCRS,
  device = "png", width = 12, height = 6)
