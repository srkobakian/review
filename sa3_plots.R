#sa3_plots

# Create a choropleth of Australia
library(sugarbag)
library(tidyverse)
library(readxl)
library(spData)
library(ggthemes)
library(maptools)
library(sf)
library(cartogram)

# Join with cancer data from AIHW
#https://www.aihw.gov.au/getmedia/df6732ee-5246-4c9f-a458-df85c88fc512/aihw-can-108-cancer-SA3-incidence-mortality.xls.aspx

cimar_sa3 <- read_excel("data/cimar-sa3.xls", 
  sheet = "Incidence Persons", range = "A3:Z337") %>%  
  # select lung cancer variables
  select(Code = `...1`,
    Name = `...2`,
    `Total incidence` = `...22`,
    Population = `...23`,
    `Crude rate (per 100,000)` = `...24`,
    `Age-standardised rate (per 100,000)` = `...25`,
    `Rate ratio (relative to all of Australia)` = `...26`
  ) %>% 
  filter(!is.na(Name)) %>% 
  filter(!(Name %in% c("Name", "Australia"))) %>% 
  mutate(Population = as.numeric(Population))


# Filter for desired areas
cimar_sa3 <- cimar_sa3 %>% 
  filter(!(Name %in% 
    c("Christmas Island", "Cocos (Keeling) Islands", "Lord Howe Island", "Jervis Bay")))

# Join with sa3 sf object
sa3lung <- absmapsdata::sa32016 %>% 
  left_join(cimar_sa3, by = c("sa3_name_2016"= "Name")) %>%
  st_as_sf() %>%
  filter(!is.na(Population)) %>%
  filter(!st_is_empty(geometry))

sa3lung <- st_transform(sa3lung, 3112)

b <- st_bbox(sa3lung)

###############################################################################

sa3lung <- sa3lung %>% 
  mutate(`Age-standardised rate (per 100,000)` = as.numeric(`Age-standardised rate (per 100,000)`))
  
# Create a choropleth
aus_ggchoro <- ggplot(sa3lung) + 
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1, na.value = "light grey") + 
  ggtitle(title) +
  theme_void() +
  coord_sf(crs = CRS("+init=epsg:3112"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme(legend.position ="bottom") + 
  labs(fill = "Age-standardised rate\n(per 100,000)")
ggsave(filename = "figures/aus_ggchoro.png", device = "png", dpi = 300,  width = 12, height = 6)
aus_ggchoro


###############################################################################
# Cartograms

# Contiguous Cartograms
cont <- sa3lung %>% 
  cartogram_cont(.,
  weight = "Population") %>% st_as_sf()
aus_ggcont <- ggplot(cont) + 
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`)) + 
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) + 
  coord_sf(crs = CRS("+init=epsg:3112"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() +   theme(legend.position ="bottom")
aus_ggcont
ggsave(filename = "figures/aus_ggcont.png", device = "png", dpi = 300,  width = 12, height = 6)
aus_ggcont


###############################################################################
# Non - Contiguous Cartograms
# Needs a scaling factor

sa3lung <- sa3lung %>% 
  mutate(sva = sqrt(as.numeric(Population)/as.numeric(areasqkm_2016)))
sa3lung %>% 
  ggplot(.) +
  geom_density(aes(x = sva)) + geom_vline(aes(xintercept = 7))

# The sa3 of Albury is used as the anchor unit

ncont <- cartogram_ncont(sa3lung, k = 1/2,
  weight = "Population") %>% st_as_sf() %>% 
  rename(`Age-standardised rate (per 100,000)` = `Age.standardised.rate..per.100.000.`)
aus_ggncont <- ggplot(ncont) + 
  geom_sf(data=sa3lung, fill = NA, colour = "grey", size = 0.001) +
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`), colour = "grey") + 
  coord_sf(crs = CRS("+init=epsg:3112"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) + scale_size_identity() + 
  theme_void() + theme(legend.position ="bottom")
aus_ggncont
ggsave(filename = "figures/aus_ggncont.png", device = "png", dpi = 300,  width = 12, height = 6)


###############################################################################
# Non - Contiguous Dorling Cartograms
dorl <- sa3lung %>% cartogram_dorling(.,
  weight = "Population") %>% st_as_sf()
aus_ggdorl <- ggplot(dorl) + 
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`)) + 
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) + 
  coord_sf(crs = CRS("+init=epsg:3112"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() + theme(legend.position ="bottom")
aus_ggdorl
ggsave(filename = "figures/aus_ggdorl.png", device = "png", dpi = 300,  width = 12, height = 6)


###############################################################################
sa3lungmap <- st_transform(sa3lung, "+proj=longlat +datum=WGS84 +no_defs")
centroids <- create_centroids(sa3lungmap, "sa3_name_2016")
grid <- create_grid(centroids = centroids, 
  hex_size = 0.7,buffer_dist = 5)
hexmap <- allocate(
  centroids = centroids, hex_grid = grid, 
  sf_id = "sa3_name_2016", hex_size = 0.7, hex_filter = 8, 
  focal_points = capital_cities, verbose = TRUE, width = 30
)

hexagons <- fortify_hexagon(hexmap, sf_id = "sa3_name_2016", hex_size = 0.7) %>% 
  left_join(st_drop_geometry(sa3lung))

create_polygons <- function(x){
  polygon <- st_multipoint(as.matrix(bind_rows(x)), dim = "XY") %>% st_cast("POLYGON")
  return(polygon)
}

hexpolygons <- hexagons %>% 
  st_as_sf(., coords = c("long", "lat")) %>% 
  group_by(sa3_name_2016) %>%
  st_cast("POLYGON")
  
hexagons <- sa3lung %>% 
  st_drop_geometry() %>% 
  arrange(sa3_name_2016) %>% 
  mutate(geometry = st_geometry(hexpolygons$geometry)) %>% 
  st_as_sf() %>% st_set_crs(., 3112) %>% 
  ungroup()
  

aus_gghexmap <- ggplot(hexagons) + 
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`), colour = NA) + 
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) +
  #coord_sf(crs = 3112, xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void() + theme(legend.position ="bottom")
aus_gghexmap
ggsave(filename = "figures/aus_gghexmap.png", plot = aus_gghexmap,
  device = "png", dpi = 300,  width = 12, height = 6)


###############################################################################
# Aus grid

aus_grid <- gridExtra::grid.arrange(aus_ggcont, aus_ggncont, aus_ggdorl, aus_gghexmap, nrow = 2)
ggsave(filename = "figures/aus_grid.png", plot = aus_grid,
  device = "png", dpi = 300,  width = 12, height = 6)
