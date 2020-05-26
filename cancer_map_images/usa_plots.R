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
cancer <- read_csv("~/review/data/USCSlung.csv") 

cancer <-  cancer %>% 
  rename(NAME = Area)


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

invthm <- theme_void() + theme(
  panel.background = element_rect(fill = "transparent", colour = NA), 
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.key = element_rect(fill = "transparent", colour = NA),
  text = element_text(colour = "white", size = 20)
)

# Create a choropleth
ggchoro <- cancer %>% 
  mutate(`Age adjusted rate\n(per 100,000)` = AgeAdjustedRate) %>% 
  ggplot() + 
  geom_sf(aes(fill = `Age adjusted rate\n(per 100,000)`), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  ggtitle(title) +
  #coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  invthm +
  theme(legend.position ="bottom",
    legend.text = element_text(size = 8))
ggchoro
ggsave(filename = "figures/ggchoro.png", device = "png", dpi = 300, width = 7, height = 6, bg = "transparent")

invthm_black <- theme_void() + theme(
  panel.background = element_rect(fill = "transparent", colour = NA), 
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.key = element_rect(fill = "transparent", colour = NA),
  text = element_text(colour = "black", size = 20)
)
ggchoro2 <- cancer %>% 
  mutate(`Age adjusted rate\n(per 100,000)` = AgeAdjustedRate) %>% 
  ggplot() + 
  geom_sf(aes(fill = `Age adjusted rate\n(per 100,000)`), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  ggtitle(title) +
  #coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  invthm_black +
  theme(legend.position ="bottom",
        legend.text = element_text(size = 8))
usa_legend <- get_legend(ggchoro2)
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
  invthm  + guides(fill = FALSE)

save(cont, file = "data/cont.rda")
ggcont
ggsave(filename = "figures/ggcont.png", device = "png", dpi = 300, width = 7, height = 6, bg = "transparent")


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
  invthm  + guides(fill = FALSE)

save(ncont, file = "data/ncont.rda")
ggncont
ggsave(filename = "figures/ggncont.png", device = "png", dpi = 300, width = 7, height = 6, bg = "transparent")


###############################################################################
# Non - Contiguous Dorling Cartograms
dorl <- cartogram_dorling(cancer,
  weight = "total_pop_15") %>% st_as_sf()

cents <- data.frame(NAME = dorl$NAME, centroids = st_coordinates(st_centroid(dorl$geometry))) %>% rename(x = centroids.X, y = centroids.Y)
  
ggdorl <- dorl %>% left_join(us_grid, by = c("NAME" = "name")) %>%
  left_join(cents) %>% 
  ggplot() + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) + 
  #geom_text(aes(x = x, y = y, label = code)) + 
  ggtitle(title) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  #coord_sf(crs = CRS("+init=epsg:3857"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  invthm + guides(fill = FALSE)
ggdorl
ggsave(filename = "figures/ggdorl.png", device = "png", dpi = 300, width = 7, height = 6, bg = "transparent")


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
#   theme_void()+ 
# ggsave(filename = "figures/ggdot.png", device = "png", dpi = 300, width = 7, height = 6, bg = "transparent")
# ggdot


###############################################################################
# Choropleth projections
# 3857, 2163, 4326, 2955
ggchoro1 <- ggplot(st_transform(cancer, 3857)) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  theme_void()
ggsave(filename = "figures/gg3857.png", plot = ggchoro1,
       device = "png", dpi = 300, width = 7, height = 6, bg = "transparent")


ggchoro2 <- ggplot(st_transform(cancer, 2163)) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  theme_void() 
ggsave(filename = "figures/gg2163.png", plot = ggchoro2,
       device = "png", dpi = 300, width = 7, height = 6, bg = "transparent")


ggchoro3 <- ggplot(st_transform(cancer, 4326)) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  theme_void() 
ggsave(filename = "figures/gg4326.png", plot = ggchoro3,
       device = "png", dpi = 300, width = 7, height = 6, bg = "transparent")


ggchoro4 <- ggplot(st_transform(cancer, 2955)) + 
  geom_sf(aes(fill = AgeAdjustedRate), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "RdPu",  direction = 1) + 
  invthm 
ggsave(filename = "figures/gg2955.png", plot = ggchoro4,
       device = "png", dpi = 300, width = 7, height = 6, bg = "transparent")


ggchoros <- plot_grid(ggchoro1, ggchoro2, ggchoro3, ggchoro4,
          labels = "auto", label_size = 20)

ggchoroCRS <- ggdraw() +
  draw_plot(ggchoro1, 0, .5, 0.5, 0.55) +
  draw_plot(ggchoro2, 0.5, 0.5, 0.5, 0.55) +
  draw_plot(ggchoro3, 0.0, 0.0, 0.5, 0.55) +
  draw_plot(ggchoro4, 0.5, 0, 0.5, 0.55) +
  draw_plot(usa_legend, 0, 0, 1, 0.1) +
  draw_plot_label(c("a", "b", "c", "d"), 
                  c(0, .5, 0, 0.5), 
                  c(1, 1, .5, .5), size = 20)

ggsave(filename = "figures/ggchoroCRS.png", plot = ggchoroCRS,
  device = "png", dpi = 300, width = 10, height = 8)

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
  invthm  + guides(fill = FALSE)  #theme_void()  

gghexmap
ggsave(filename = "figures/gghexmap.png", plot = gghexmap,
  device = "png", dpi = 300, width = 7, height = 6, bg = "transparent")

load("data/usa_legend.rda")

usa_gridl <- ggdraw() +
  draw_plot(ggcont, -0.05, 0.55, 0.60, 0.55) +
  draw_plot(ggncont, 0.45, 0.55, 0.60, 0.55, 0.9) +
  draw_plot(ggdorl, -0.05, 0.1, 0.60, 0.55, 0.9) +
  draw_plot(gghexmap, 0.45, 0.1, 0.60, 0.55, 0.7) +
  draw_plot(usa_legend, 0, 0, 1, 0.1) +
  draw_plot_label(c("a", "b", "c", "d"), 
                  c(0, .5, 0, 0.5), 
                  c(1, 1, .55, .55), size = 20)
usa_gridl

ggsave(filename = "figures/usa_grid.png", plot = usa_gridl,
       device = "png", dpi = 300, width = 11, height = 6, bg = "transparent")

###############################################################################


# Tilegram
library(statebins)

## US example
ggtilegram <- cancer %>% rename(state = NAME) %>% 
  statebins(., state_col = "state",
    value_col = "AgeAdjustedRate", 
    name = "AgeAdjustedRate",
    type = "seq", palette = "RdPu",  direction = 1, font_size = 10
  ) +
  invthm + guides(fill= FALSE)
ggtilegram
ggsave(filename = "figures/ggtilegram.png", plot = ggtilegram,
  device = "png", dpi = 300, width = 10, height = 8, bg = "transparent")



# Geofacet
library(geofacet)
library(geogrid)
us_grid <- read_csv("us_grid.csv") %>% filter(col>1)

# read in data
cancer_f <- read_csv("~/review/data/USCS_lung_f.csv") %>% mutate(Sex = "F")
cancer_m <- read_csv("~/review/data/USCS_lung_m.csv") %>% mutate(Sex = "M")

cancer_mf <- bind_rows(cancer_f, cancer_m) %>% as_tibble() %>% 
    mutate(state = gsub("'", "", Area),
    AgeAdjustedRate = parse_number(AgeAdjustedRate))

ggfacet <- ggplot(cancer_mf) + 
  geom_col(aes(x = Sex, y = AgeAdjustedRate, fill = Sex)) + 
  scale_fill_brewer(type = "qual", palette = "Pastel1",  direction = 1) + 
  facet_geo(~ state, grid = us_grid, label = "code") +
  theme_bw() + 
  theme(legend.position ="bottom", 
        axis.title.y = element_blank(),
        axis.text = element_text(colour = "white"),
        strip.text.x = element_text(
          size = 14, color = "black", face = "bold"
        ),
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA))

ggfacet
ggsave(filename = "figures/ggfacet.png", plot = ggfacet,
  device = "png", dpi = 300, width = 15, height = 12, bg = "transparent")

p <- plot_grid(rasterGrob(png::readPNG("figures/ggtilegram.png")),
          rasterGrob(png::readPNG("figures/ggfacet.png")), 
          labels = c('a', 'b'), label_size = 20, nrow = 1)

ggsave(filename = "figures/gggrids.png", plot = p,
  device = "png", dpi = 300, width = 16, height = 8)


library(tilegramsR)

sf_NPR1to1
