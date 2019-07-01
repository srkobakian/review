#USA Cancer Data

#  United States Cancer Statistics: Data Visualizations
#The official federal statistics on cancer incidence and deaths, produced by the Centers for Disease Control and Prevention (CDC) and the National Cancer Institute (NCI).

# https://gis.cdc.gov/cancer/USCS/DataViz.html


# United States
# Rate of New Cancers
# Female and Male
# Melanoma of the skin
# 2012-2016

# Export

library(sugarbag)
library(tidyverse)

# read in data
cancer <- read_csv("data/USCS.csv") %>% rename(NAME = Area)

library(spData)
library(tmap)
library(maptools)

# Join polygons to data
data(us_states)
cancer <- left_join(us_states, cancer, by = c("NAME"))
us <- sf::as_Spatial(cancer)
us <- spTransform(us, CRS("+init=epsg:3857"))

# Histogram
ggplot(cancer, aes(x = AgeAdjustedRate)) + geom_histogram(binwidth = 1) 
ggplot(cancer, aes(x = AgeAdjustedRate)) + geom_density()


# Create a choropleth
ggchoro <- ggplot(cancer) + geom_sf(aes(fill = AgeAdjustedRate)) + scale_fill_distiller(type = "div", palette = "RdYlBu")
ggsave(filename = "figures/ggchoro.png", device = "png", width = 12, height = 6)

# Cartograms
#install.packages("cartogram")
library(cartogram)

# Contiguous Cartograms
cont <- cartogram_cont(us,
  weight = "AgeAdjustedRate") %>% sf::st_as_sf()
ggcont <- ggplot(cont) + 
  geom_sf(aes(fill = AgeAdjustedRate)) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu")
ggsave(filename = "figures/ggcont.png", device = "png", width = 12, height = 6)

# Non - Contiguous Cartograms
ncont <- cartogram_ncont(us,
  weight = "AgeAdjustedRate") %>% sf::st_as_sf()
ggncont <- ggplot(ncont) + 
  geom_sf(aes(fill = AgeAdjustedRate)) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu")
ggsave(filename = "figures/ggncont.png", device = "png", width = 12, height = 6)


# Non - Contiguous Dorling Cartograms
dorl <- cartogram_dorling(us,
  weight = "AgeAdjustedRate") %>% sf::st_as_sf()
ggdorl <- ggplot(dorl) + 
  geom_sf(aes(fill = AgeAdjustedRate)) + 
  scale_fill_distiller(type = "div", palette = "RdYlBu")
ggsave(filename = "figures/ggdorl.png", device = "png", width = 12, height = 6)
