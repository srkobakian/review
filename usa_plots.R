#USA Cancer Data

# United States Cancer Statistics: Data Visualizations
# The official federal statistics on cancer incidence and deaths, produced by the Centers for Disease Control and Prevention (CDC) and the National Cancer Institute (NCI).

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
library(cowplot)
library(grid)
library(png)
library(gridExtra)

# read in data
cancer <- read_csv("data/USCSlung.csv") %>% rename(NAME = Area)


# Join polygons to data
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
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  ggtitle(title) +
  #coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() +
  theme(legend.position ="bottom")
ggchoro
#ggsave(filename = "figures/ggchoro.png", device = "png", dpi = 300, width = 7, height = 6)

usa_legend <- get_legend(ggchoro)
save(usa_legend, file = "figures/usa_legend.rda")

###############################################################################
# Cartograms

# Contiguous Cartograms
cont <- cartogram_cont(cancer,
  weight = "total_pop_15") %>% st_as_sf()
ggcont <- ggplot(cont) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) + 
  ggtitle(title) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  #coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() + guides(fill = FALSE)

save(cont, file = "data/cont.rda")
ggcont
ggsave(filename = "figures/ggcont.png", device = "png", dpi = 300, width = 7, height = 6)


###############################################################################
# Non - Contiguous Cartograms
# Needs a scaling factor

cancer <- cancer %>% mutate(sva = sqrt(as.numeric(total_pop_15/AREA)))
cancer %>% 
  ggplot(.) +
  geom_density(aes(x = sva)) + geom_vline(aes(xintercept = 5))

# The state of Vermont is used as the anchor unit

ncont <- cartogram_ncont(cancer, k = 1/5,
  weight = "total_pop_15") %>% st_as_sf()
ggncont <- ggplot(ncont) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) + 
  geom_sf(data=cancer, fill = NA, colour = "grey") +
  ggtitle(title) + 
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  #coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() + guides(fill = FALSE)

save(ncont, "data/ncont.rda")
ggncont
ggsave(filename = "figures/ggncont.png", device = "png", dpi = 300, width = 7, height = 6)


###############################################################################
# Non - Contiguous Dorling Cartograms
dorl <- cartogram_dorling(cancer,
  weight = "total_pop_15") %>% st_as_sf()
ggdorl <- ggplot(dorl) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) + 
  ggtitle(title) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  #coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void()+ guides(fill = FALSE)
ggsave(filename = "figures/ggdorl.png", device = "png", dpi = 300, width = 7, height = 6)
ggdorl


###############################################################################

# Centroid Cartograms
# cancer <- cancer %>% 
#   sf::st_centroid(geometry) %>% 
#   sf::st_coordinates() %>% as_tibble() %>% bind_cols(cancer, .)
# 
# ggdot <- ggplot(cancer) + 
#   geom_point(aes(X,Y, colour = AgeAdjustedRate)) + 
#   geom_sf(data=cancer, fill = NA, colour = "grey") +
#   ggtitle(title) +
#   scale_colour_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
#   coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
#   theme_void()+ guides(fill = FALSE)
# ggsave(filename = "figures/ggdot.png", device = "png", dpi = 300, width = 7, height = 6)
# ggdot


###############################################################################
# Choropleth projections
# 3857, 2163, 4326, 2955
ggchoro1 <- ggplot(st_transform(cancer, 3857)) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  ggtitle("a. The United States using EPSG: 3857") +
  theme_void()+ guides(fill = FALSE)

ggchoro2 <- ggplot(st_transform(cancer, 2163)) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  ggtitle("b. The United States using EPSG: 2163") +
  theme_void()+ guides(fill = FALSE)

ggchoro3 <- ggplot(st_transform(cancer, 4326)) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  ggtitle("c. The United States using EPSG: 4326") +
  theme_void()+ guides(fill = FALSE)

ggchoro4 <- ggplot(st_transform(cancer, 2955)) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  ggtitle("d. The United States using EPSG: 2955") +
  theme_void() + guides(fill = FALSE)

ggchoroCRS <- gridExtra::grid.arrange(ggchoro1, ggchoro2, ggchoro3, ggchoro4)

ggsave(filename = "figures/ggchoroCRS.png", plot = ggchoroCRS,
  device = "png", dpi = 300, width = 7, height = 6)


###############################################################################

cancermap <- st_transform(cancer, "+proj=longlat +datum=WGS84 +no_defs")
centroids <- create_centroids(cancermap, "NAME")
grid <- create_grid(centroids = centroids, 
  hex_size = 2.5 ,buffer_dist = 5)
hexmap <- allocate(
  centroids = centroids, hex_grid = grid, 
  sf_id = "NAME", hex_size = 2.5, hex_filter = 5, 
  focal_points = tibble(point = "mean", longitude = mean(centroids$longitude), latitude = mean(centroids$latitude)), 
  verbose = TRUE, width = 30
  )

hexagons <- fortify_hexagon(hexmap, sf_id = "NAME", hex_size = 2.5) %>%  
  left_join(st_drop_geometry(cancer))

hexagons_sf <- hexagons %>% 
  select(NAME, long, lat) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4283) %>%
  group_by(NAME) %>% 
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON")

# Remove geometry of sa3 geographic areas
cancer_ng <- sf::st_drop_geometry(cancer)

hex <- cancer_ng %>% 
  # ensure correct order by ordering alphabetically
  arrange(NAME) %>% 
  mutate(geometry = hexagons_sf$geometry) %>% st_as_sf()


gghexmap <- ggplot(hex) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) + 
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) +
  theme_void()+ guides(fill = FALSE) 

gghexmap
ggsave(filename = "figures/gghexmap.png", plot = gghexmap,
  device = "png", dpi = 300, width = 7, height = 6)



usa_grid <- gridExtra::grid.arrange(ggcont, ggncont, ggdorl, gghexmap, nrow = 2)
ggsave(filename = "figures/usa_grid.png", plot = usa_grid,
  device = "png", dpi = 300, width = 7, height = 6)

###############################################################################


# Tilegram
library(statebins)

## US example
ggtilegram <- cancer %>% rename(state = NAME) %>% 
  statebins(., state_col = "state",
    value_col = "AgeAdjustedRate", 
    name = "AgeAdjustedRate",
    type = "seq", palette = "RdPu",  direction = 1
  ) +
  theme_void() +
  theme(legend.position ="bottom")
ggtilegram
ggsave(filename = "figures/ggtilegram.png", plot = ggtilegram,
  device = "png", dpi = 300, width = 7, height = 6)


# Geofacet
library(geofacet)
library(geogrid)
us_grid <- read_csv("us_grid.csv") %>% filter(col>1)

# read in data
cancer_f <- read_csv("data/USCS_lung_f.csv") %>% mutate(Sex = "F")
cancer_m <- read_csv("data/USCS_lung_m.csv") %>% mutate(Sex = "M")

cancer_mf <- bind_rows(cancer_f, cancer_m) %>% 
    mutate(state = gsub("'", "", Area),
    AgeAdjustedRate = parse_number(AgeAdjustedRate)) %>% 
  select(state, Sex, AgeAdjustedRate)

ggfacet <- ggplot(cancer_mf) + 
  geom_col(aes(x = Sex, y = AgeAdjustedRate, fill = Sex)) + 
  scale_fill_brewer(type = "qual", palette = "Pastel1",  direction = 1) + 
  facet_geo(~ state, grid = us_grid, label = "code") +
  theme_minimal() + 
  theme(legend.position ="bottom",axis.title.y=element_blank() )

ggfacet
ggsave(filename = "figures/ggfacet.png", plot = ggfacet,
  device = "png", dpi = 300, width = 10, height = 8)

p <- plot_grid(rasterGrob(png::readPNG("figures/ggtilegram.png")),
              rasterGrob(png::readPNG("figures/ggfacet.png")), labels = c('A', 'B'), label_size = 20)

ggsave(filename = "figures/gggrids.png", plot = p,
  device = "png", dpi = 300, width = 18, height = 9)
