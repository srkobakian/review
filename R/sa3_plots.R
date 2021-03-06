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
library(cowplot)


# Filter for centroids only within longitudes
checkgeom <- function(x){
  xval <- st_coordinates(x)[[2]]
  
  if (xval < 109){return(FALSE)}
  if (xval > 155){return(FALSE)}
  return(TRUE)
}

sa3lung <- absmapsdata::sa32016 %>% 
  filter(!st_is_empty(geometry)) %>%
  filter(checkgeom(x = geometry))
  
sa3lung <- sa3lung %>% 
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE)

sa3lung_fort <- fortify_sfc(sa3lung)

# Join with cancer data from AIHW
#https://www.aihw.gov.au/getmedia/df6732ee-5246-4c9f-a458-df85c88fc512/aihw-can-108-cancer-SA3-incidence-mortality.xls.aspx

cimar <- read_excel("~/review/data/cimar-sa3.xlsx", 
  sheet = "Incidence Persons", range = "A3:Z337")

cimar_sa3 <- cimar %>%  
  # select lung cancer variables
  rename(Code = `...1`,
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
  filter((Name %in% sa3lung$sa3_name_2016))

# Join with sa3 sf object
sa3lung <- sa3lung %>% 
  left_join(cimar_sa3, by = c("sa3_name_2016" = "Name")) %>% 
  st_as_sf() %>%
  #filter(!is.na(Population)) %>%
  mutate(Population = as.numeric(ifelse(is.na(Population), 1, Population)),
         `Age-standardised rate (per 100,000)` = as.numeric(ifelse(is.na(`Age-standardised rate (per 100,000)`),
                                                        1, `Age-standardised rate (per 100,000)`))) %>% 
  filter(!st_is_empty(geometry))

sa3lung <- st_transform(sa3lung, 3112)

b <- st_bbox(sa3lung)
b["xmin"]<- -2181807
b["xmax"]<- 1933081

aus <- absmapsdata::state2016 %>% 
  filter(!(state_name_2016 == "Other Territories"))

###############################################################################

invthm <- theme_void() + theme(
  panel.background = element_rect(fill = "transparent", colour = NA), 
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.key = element_rect(fill = "transparent", colour = NA),
  text = element_text(colour = "white", size = 20),
)

# Create a choropleth
aus_ggchoro <- ggplot(sa3lung) + 
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1, na.value = "light grey") + 
  
  coord_sf(crs = CRS("+init=epsg:3112"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  invthm + theme(legend.position ="bottom") +
  labs(fill = "Age-standardised rate\n(per 100,000)")
aus_ggchoro
ggsave(filename = "figures/aus_ggchoro.png", device = "png",
  bg = "transparent",  dpi = 300,  width = 7, height = 6)


aus_legend <- get_legend(aus_ggchoro)
save(aus_legend, file = "figures/aus_legend.rda")

###############################################################################
# Cartograms
  
# Contiguous Cartograms
cont <- sa3lung %>% 
  cartogram_cont(.,
  weight = "Population", itermax = 60) %>%
  st_as_sf()

save(cont, "data/auscont.rda")
load("data/auscont.rda")
aus_ggcont <- ggplot(cont) + 
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`)) + 
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) + 
  coord_sf(crs = CRS("+init=epsg:3112"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  invthm +  guides(fill=FALSE)
aus_ggcont
ggsave(filename = "figures/aus_ggcont.png", device = "png",  bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
# Non - Contiguous Cartograms
# Needs a scaling factor

sa3lung <- sa3lung %>% 
  mutate(sva = sqrt(as.numeric(Population)/as.numeric(areasqkm_2016)))
sa3lung %>% 
  ggplot(.) +
  geom_density(aes(x = sva)) + geom_vline(aes(xintercept = 7))

ncont <- cartogram_ncont(sa3lung, k = 1/5,
  weight = "Population") %>% st_as_sf() %>% 
  rename(`Age-standardised rate (per 100,000)` = `Age.standardised.rate..per.100.000.`)
aus_ggncont <- ggplot(ncont) + 
  geom_sf(data=aus, fill = NA, colour = "grey", size = 0.01) +
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`), colour = NA) + 
  coord_sf(crs = CRS("+init=epsg:3112"), xlim =
             c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) + 
  invthm +guides(fill=FALSE)
aus_ggncont

perth_ggncont <- ggplot(ncont %>% filter(gcc_name_2016 == "Greater Perth")) + 
  geom_rect(aes(xmin =-1736000, xmax = -1639000, ymin=-3824000, ymax=-3672300), fill = NA, colour = "black") +
  geom_sf(data= sa3lung %>% filter(gcc_name_2016 == "Greater Perth"),
          fill = NA, colour = "grey", size = 0.001) +
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`), colour = NA) + 
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) + invthm +
  guides(fill=FALSE)
perth_ggncont

library(cowplot)
full_ggncont <- ggdraw() + 
  draw_plot(aus_ggncont, 0, 0, 1, 1) +
  draw_plot(perth_ggncont, 0.33, 0.10, 0.25, 0.25)
full_ggncont
ggsave(filename = "figures/aus_ggncont.png", plot = full_ggncont,
       device = "png",   bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
# Non - Contiguous Dorling Cartograms
dorl <- sa3lung %>% filter(cent_long >110, cent_long<155) %>% cartogram_dorling(.,
  weight = "Population", k = 0.1) %>% st_as_sf(crs=CRS("+init=epsg:3112"))
d <- st_bbox(dorl)
aus_ggdorl <- ggplot(dorl) + 
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`)) + 
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) +
  coord_sf(crs = CRS("+init=epsg:3112"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  invthm + guides(fill = FALSE)
aus_ggdorl
ggsave(filename = "figures/aus_ggdorl.png", device = "png", bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
sa3lungmap <- st_transform(sa3lung, "+proj=longlat +datum=WGS84 +no_defs")
centroids <- create_centroids(sa3lungmap, "sa3_name_2016")
lung_neighbours <- st_intersects(sa3lungmap,sa3lungmap)

grid <- create_grid(centroids = centroids, 
  hex_size = 0.75,buffer_dist = 5)
hexmap <- allocate(
  centroids = centroids, hex_grid = grid, use_neighbours = NULL,
  sf_id = "sa3_name_2016", hex_size = 0.75, hex_filter = 8, 
  focal_points = capital_cities, verbose = TRUE, width = 30
)

hexagons <- fortify_hexagon(hexmap, sf_id = "sa3_name_2016", hex_size = 0.75) %>% 
  left_join(st_drop_geometry(sa3lung))

hexagons_sf <- hexagons %>% 
  select(sa3_name_2016, long, lat) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4283) %>%
  group_by(sa3_name_2016) %>% 
  summarise(do_union = FALSE) %>%
  st_cast("POLYGON")

# Remove geometry of sa3 geographic areas
sa3_ng <- sf::st_drop_geometry(sa3lung)

hex <- sa3lung %>% 
  # ensure correct order by ordering alphabetically
  arrange(sa3_name_2016) %>% 
  mutate(geometry = hexagons_sf$geometry)

aus_gghexmap <- ggplot(hex) + 
  geom_sf(data=aus, fill = NA, colour = "grey", size = 0.001) +
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`), colour = NA) + 
  coord_sf(crs = CRS("+init=epsg:3112"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) + scale_size_identity() + 
  invthm +guides(fill=FALSE)
aus_gghexmap
ggsave(filename = "figures/aus_gghexmap.png", plot = aus_gghexmap,
  device = "png",   bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
# Aus grid

aus_grid <- gridExtra::grid.arrange(aus_ggcont, full_ggncont, aus_ggdorl, aus_gghexmap, nrow = 2)
ggsave(filename = "figures/aus_grid.png", plot = aus_grid,
  device = "png",   bg = "transparent", dpi = 300,  width = 7, height = 6)



#####
# Animate
library(gganimate)

sa3lung_order <- fortify_sfc(sa3lung) %>% 
  left_join(hexmap %>% dplyr::select(sa3_name_2016, focal_dist, rownumber)) %>% filter(cent_long>110, cent_long<155)

reveal_points <- ggplot() +
  geom_sf(data=aus, fill = NA, colour = "grey", size = 0.01) +
  geom_point(aes(x = cent_long, cent_lat), 
    data = sa3lung_order %>% group_by(sa3_name_2016) %>% slice(1)) +
 transition_reveal(along = rownumber)

anim_save(reveal_points, reveal_points)

anim <- ggplot(sa3lung_order, aes(long, lat, polygon)) +
   transition_layers(layer_length = 1, transition_length = 2) +
   enter_fade() + enter_grow()

anim_save()