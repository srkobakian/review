# Queensland maps

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

# SA2 populations
#https://www.qgso.qld.gov.au/issues/5496/estimated-resident-population-sa2-qld-2006-2019p.csv
pop <- read_csv("data/estimated-resident-population-sa2-qld-2006-2019p.csv",
                skip = 3) %>% select(SA2_name = "X2", pop = `2011`) %>% 
                drop_na()

# SIR data from Australian Cancer Atlases
SIR <- read_csv("data/SIR Downloadable Data.csv")

SIR <- SIR %>%
  filter(SA2_code >30000, SA2_code <40000) %>% 
  filter(Year == "2010-2014") 

SIR_fem <- SIR %>% 
  filter(Sex_name == "Females") %>%  
  select(Year, SA2_NAME11 = SA2_name,
         p50, Cancer_name) %>% 
  spread(Cancer_name, p50)

SIR_male <- SIR %>%
  filter(Year == "2010-2014") %>% 
  filter(Sex_name == "Males") %>%  
  select(Year, SA2_NAME11 = SA2_name,
         p50, Cancer_name) %>% 
  spread(Cancer_name, p50)

SIR_persons <- SIR %>%
  filter(Year == "2010-2014") %>% 
  filter(Sex_name == "Persons") %>%  
  select(Year, SA2_NAME11 = SA2_name,
         p50, Cancer_name) %>% 
  spread(Cancer_name, p50)
 

# Create a colour scheme
nac_colours <- c("A" = "#33809d",
                 "B" = "#aec6c7",
                 "C" = "#fff4bc",
                 "D" = "#ff9a64",
                 "E" = "#ff3500")

colour_cat <- function(num){
  case_when(
    num < 0.75  ~ "A",
    num >= 0.75 & num < 1  ~ "B",
    num >= 1 & num < 1.25  ~ "C",
    num >= 1.25 & num < 1.5  ~ "D",
    num > 1.5  ~ "E")
} 


# Filter for centroids only within longitudes
checkgeom <- function(x){
  xval <- st_coordinates(x)[[2]]
  
  if (xval < 109){return(FALSE)}
  if (xval > 155){return(FALSE)}
  return(TRUE)
}


nac_colours <- c("A" = "#33809d",
                 "B" = "#aec6c7",
                 "C" = "#fff4bc",
                 "D" = "#ff9a64",
                 "E" = "#ff3500",
                 "NA" = "#D3D3D3")

colours <- c(   "Much lower\nthan average" = "#33809d",
                "Lower than\naverage" = "#aec6c7",
                "Average" = "#fff4bc",
                "Higher than\naverage" = "#ff9a64",
                "Much higher\nthan average" = "#ff3500")
colourdata <- tribble(~data,
                      "Much lower\nthan average",
                      "Lower than\naverage",
                      "Average",
                      "Higher than\naverage",
                      "Much higher\nthan average") %>% 
        mutate(data = factor(data, 
                             levels = c(
                               "Much lower\nthan average",
                               "Lower than\naverage",
                               "Average",
                               "Higher than\naverage",
                               "Much higher\nthan average")))
coloursplot <- ggplot(colourdata) + 
  geom_bar(aes(x=data, 
    fill = data)) +
  scale_fill_manual(values = colours) +
  labs(fill = "Melanoma Cancer (Males):\nAge-standardised rate\n(per 100,000)")
  

sa22011 <- absmapsdata::sa22011 %>% 
  filter(state_name_2011 == "Queensland") %>% 
  filter(!st_is_empty(geometry)) %>%
  filter(checkgeom(x = geometry)) %>% 
  select(sa2_name_2011, state_name_2011, gcc_name_2011, albers_sqkm, geometry)

mel <- SIR_male %>% 
  mutate(p50_col = factor(colour_cat(Melanoma), 
                          levels = c("A", "B", "C", "D", "E")))

sa2_mel <- sa22011 %>% 
  left_join(mel, by = c("sa2_name_2011" = "SA2_NAME11"))

sa2_mel  <- sa2_mel  %>% 
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE)

sa2_mel  <- st_transform(sa2_mel, 3112)

sa2_mel_fort <- fortify_sfc(sa2_mel)

b <- st_bbox(sa2_mel)

queensland <- absmapsdata::state2016 %>% 
  filter((state_name_2016 == "Queensland"))

invthm <- theme_void() + theme(
  panel.background = element_rect(fill = "transparent", colour = NA), 
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.key = element_rect(fill = "transparent", colour = NA),
  text = element_text(colour = "white", size = 20),
)

###############################################################################

# Create a choropleth
qld_ggchoro <- ggplot(sa2_mel_fort) + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon),
                   fill = p50_col), colour = "lightgrey") +
  scale_fill_manual(values = nac_colours) + 
  coord_sf(crs = 3112, 
           xlim = c(b["xmin"], b["xmax"]), 
           ylim = c(b["ymin"], b["ymax"])) +
  guides(fill = FALSE) +
  labs(fill = "Melanoma Cancer (Males):\nAge-standardised rate\n(per 100,000)") +
  invthm

bris_box <- c(xmin = 1808445, ymin = -3297649,        
              xmax = 1877776, ymax = -3190617)

bris_ggchoro <- ggplot(sa2_mel_fort) + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon),
                   fill = p50_col), colour = "lightgrey") +
  scale_fill_manual(values = nac_colours) + 
  coord_sf(crs = 3112, 
           xlim = c(bris_box["xmin"], bris_box["xmax"]), 
           ylim = c(bris_box["ymin"], bris_box["ymax"])) +
  guides(fill = FALSE) +
  labs(fill = "Melanoma Cancer (Males):\nAge-standardised rate\n(per 100,000)") + invthm 
  
qld_ggchoro
ggsave(filename = "figures/qld_ggchoro.png", device = "png",
       bg = "transparent",  dpi = 300,  width = 7, height = 6)

qld_legend <- get_legend(qld_ggchoro)
save(qld_legend, file = "figures/qld_legend.rda")

###############################################################################
# Cartograms

# Contiguous Cartograms
pop <- pop %>% mutate(pop = pop+1)
 
# qldcont <- sa2_mel %>% 
#    left_join(pop, by = c("sa2_name_2011" = "SA2_name")) %>%
#    cartogram_cont(., weight = "pop", prepare = "adjust", 
#                   itermax = 200, threshold  = 0.9) %>%
#    st_as_sf()

#save(qldcont, file = "data/qldcont.rda")
load("data/qldcont.rda")
clim <-st_bbox(qldcont)
  
qld_ggcont <- ggplot(qldcont) + 
  geom_sf(data = sa2_mel, fill = NA, colour = "grey", size = 0.5) +
  geom_sf(aes(fill = p50_col), size = 0.001) +
  scale_fill_manual(values = nac_colours, na.value = "#505050") + 
  coord_sf(crs = 3112, 
           xlim = c(b["xmin"], clim["xmax"]), 
           ylim = c(clim["ymin"], clim["ymax"])) +
  invthm + guides(fill=FALSE) + theme(plot.background = element_rect(fill = "black"))
qld_ggcont
ggsave(filename = "figures/qld_ggcont.png", device = "png",  bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
# Non - Contiguous Cartograms
# Needs a scaling factor

sa2_mel <- sa2_mel %>% 
  left_join(pop, by = c("sa2_name_2011" = "SA2_name")) %>% 
  mutate(sva = sqrt(as.numeric(pop)/as.numeric(albers_sqkm)))

sa2_mel  %>% 
  ggplot(.) +
  geom_density(aes(x = sva)) + geom_vline(aes(xintercept = 4))


ncont <- cartogram_ncont(sa2_mel, k = 1,
                         weight = "pop") %>% 
  st_as_sf() %>% st_transform("+init=epsg:3112 +units=m")


qld_ggncont <- ggplot(ncont) +  
  geom_sf(data=sa2_mel, 
          fill = NA, colour = "grey", size = 0.2) +
  geom_sf(aes(fill = p50_col), size = 0.01) + 
  scale_fill_manual(values = nac_colours) +
  invthm + guides(fill=FALSE)
qld_ggncont


brisbane <- ggplot(ncont %>% filter(gcc_name_2011 == "Greater Brisbane")) +  
  geom_sf(data=sa2_mel %>% filter(gcc_name_2011 == "Greater Brisbane"), 
          fill = NA, colour = "grey", size = 0.01) +
  geom_sf(aes(fill = p50_col), colour=NA) + 
  scale_fill_manual(values = nac_colours) +
coord_sf(crs = CRS("+init=epsg:3112 +units=m"), 
         xlim = c(bris_box["xmin"], bris_box["xmax"]), 
         ylim = c(bris_box["ymin"], bris_box["ymax"])) +
  xlim(bris_box["xmin"], bris_box["xmax"]) +
  ylim(bris_box["ymin"], bris_box["ymax"]) +
  invthm + guides(fill=FALSE)


library(cowplot)
full_ggncont <- ggdraw(qld_ggncont, xlim = c(0,1), ylim = c(0,1)) + 
  draw_plot(brisbane, 0.65, 0.38, 0.25, 0.25) + 
  draw_line(x = c(0.69, 0.76, 0.85), 
            y = c(0.381, 0.12, 0.381)) + 
  draw_line(x = c(0.687, 0.85, 0.85, 0.687, 0.687), 
            y = c(0.381, 0.381, 0.635, 0.635, 0.381))
full_ggncont

ggsave(filename = "figures/qld_ggncont.png", plot = full_ggncont,
       device = "png", bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
# Non - Contiguous Dorling Cartograms
# k = Share of the bounding box of x filled by the larger circle
dorl <- sa2_mel %>% 
  mutate(pop = ifelse(is.na(pop), 1, pop)) %>% 
  cartogram_dorling(., k = 0.1, weight = "pop", m_weight = 1) %>%
  st_as_sf(crs=CRS("+init=epsg:3112"))
d <- st_bbox(dorl)
qld_ggdorl <- ggplot(dorl) + 
  geom_sf(data=sa2_mel, fill = NA, colour = "grey") +
  geom_sf(aes(fill = p50_col), colour = NA) + 
  scale_fill_manual(values = nac_colours) +
  coord_sf(crs = CRS("+init=epsg:3112"),
           xlim = c(d["xmin"]-55000, d["xmax"]), 
           ylim = c(d["ymin"], d["ymax"])) +
  invthm + guides(fill = FALSE)
qld_ggdorl
ggsave(filename = "figures/qld_ggdorl.png", device = "png", plot = qld_ggdorl,
       bg = "transparent", dpi = 300, width = 7, height = 6)


###############################################################################

## Load data
sa2_melmap <- st_transform(sa2_mel, "+proj=longlat +datum=WGS84 +no_defs")

## Create centroids set
centroids <- create_centroids(sa2_melmap, "sa2_name_2011")

## Create hexagon grid
grid <- create_grid(centroids = centroids,
                    hex_size = 0.27,
                    buffer_dist = 5)

## Allocate polygon centroids to hexagon grid points
hex_allocated <- allocate(
  centroids = centroids,
  hex_grid = grid,
  sf_id = "sa2_name_2011",
  ## same column used in create_centroids
  hex_size = 0.27,
  ## same size used in create_grid
  hex_filter = 10,
  focal_points = capital_cities,
  width = 35,
  verbose = FALSE
)

## Prepare to plot
fort_hex <-  fortify_hexagon(data = hex_allocated,
                  sf_id = "sa2_name_2011",
                  hex_size = 0.27) %>%
  left_join(sa2_melmap)
sa2_mel_fort <-
  st_transform(sa2_mel, "+proj=longlat +datum=WGS84 +no_defs") %>% 
  fortify_sfc()
## Make a plot
qld_gghexmap <- ggplot() +
  geom_polygon(
    aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)),  
    fill = NA,  colour = "lightgrey",  data = sa2_mel_fort) +
  geom_polygon(aes( x = long, y = lat, group = hex_id, fill = p50_col),
  data = fort_hex) +
  scale_fill_manual(values = nac_colours) +
  scale_size_identity() + coord_equal() +
  invthm + guides(fill = FALSE)


ggsave(filename = "figures/qld_gghexmap.png", plot = qld_gghexmap,
       device = "png", bg = "transparent", dpi = 300,  width = 7, height = 6)


###############################################################################
# qld grid

qld_grid <- cowplot::plot_grid(
  plot_grid(qld_ggchoro, qld_ggcont,  
            qld_ggncont, qld_ggdorl, 
            labels=c("a", "b", "c", "d"),
            ncol = 2, nrow = 2, label_size = 28) +
    draw_plot(bris_ggchoro, 0.365, 0.7, 0.2, 0.2) + 
    draw_plot(brisbane, 0.365, 0.2, 0.21, 0.2) +
    draw_line(x = c(0.4, 0.37, 0.53), 
              y = c(0.7, 0.55, 0.7), colour = "#505050") + 
    draw_line(x = c(0.4, 0.37, 0.53), 
              y = c(0.2, 0.05, 0.2), colour = "#505050") + 
    draw_line(x = c(0.4, 0.53, 0.53, 0.4, 0.4), 
              y = c(0.7, 0.7, 0.9, 0.9, 0.7), colour = "#505050") +
    draw_line(x = c(0.4, 0.53, 0.53, 0.4, 0.4), 
              y = c(0.2, 0.2, 0.4, 0.4, 0.2), colour = "#505050") ,
  cowplot::get_legend(coloursplot + 
                     theme(legend.position = "bottom",
                           legend.background = 
                           element_rect(fill = "black", colour = NA),
                           legend.text = element_text(colour = "white"),
                           legend.title = element_text(colour = "white"))),
                           ncol=1, rel_heights=c(.8, .2))  +
  theme(plot.background = element_rect(fill = "black", colour = "black"))
qld_grid 


ggsave(filename = "figures/qld_grid.pdf", plot = qld_grid,
       device = "pdf", dpi = 400,  width = 14, height = 14)

###############################################################################

## Australia

SIR <- read_csv("data/SIR Downloadable Data.csv")

SIR <- SIR %>%
  filter(Year == "2010-2014") 

SIR_male <- SIR %>%
  filter(Year == "2010-2014") %>% 
  filter(Sex_name == "Males") %>%  
  select(Year, SA2_NAME11 = SA2_name,
         p50, Cancer_name) %>% 
  spread(Cancer_name, p50)


aus_sa2 <- absmapsdata::sa22011 %>% 
  filter(sa2_name_2011 %in% SIR$SA2_name)

aus_sa2 <- st_transform(aus_sa2, "+proj=longlat +datum=WGS84 +no_defs")

## Load data

## Create centroids set
aus_centroids <- create_centroids(aus_sa2, "sa2_name_2011")

## Create hexagon grid
aus_grid <- create_grid(centroids = aus_centroids,
                    hex_size = 0.2,
                    buffer_dist = 5)

## Allocate polygon centroids to hexagon grid points
aus_allocated <- allocate(
  centroids = aus_centroids,
  hex_grid = aus_grid,
  sf_id = "sa2_name_2011",
  ## same column used in create_centroids
  hex_size = 0.2,
  ## same size used in create_grid
  hex_filter = 20,
  focal_points = capital_cities,
  width = 45,
  verbose = TRUE
)

save(aus_allocated, file = "data/aus_hexmap.rda")
load("data/aus_hexmap.rda")

## Prepare to plot
aus_fort_hex <- fortify_hexagon(data = aus_allocated,
                            sf_id = "sa2_name_2011",
                            hex_size = 0.2) %>% 
  left_join(SIR_male, by = c("sa2_name_2011" = "SA2_NAME11")) %>% 
  mutate(p50_col = factor(colour_cat(Melanoma), 
                          levels = c("A", "B", "C", "D", "E")))

aus_sa2_mel_fort <- aus_sa2 %>%
  fortify_sfc()


colours <- c(   "Much lower\nthan average" = "#33809d",
                "Lower than\naverage" = "#aec6c7",
                "Average" = "#fff4bc",
                "Higher than\naverage" = "#ff9a64",
                "Much higher\nthan average" = "#ff3500")
colourdata <- tribble(~data,
                      "Much lower\nthan average",
                      "Lower than\naverage",
                      "Average",
                      "Higher than\naverage",
                      "Much higher\nthan average") %>% 
  mutate(data = factor(data, 
                       levels = c(
                         "Much lower\nthan average",
                         "Lower than\naverage",
                         "Average",
                         "Higher than\naverage",
                         "Much higher\nthan average")))
coloursplot <- ggplot(colourdata) + 
  geom_bar(aes(x=data, 
               fill = data)) +
  scale_fill_manual(values = colours) +
  labs(fill = "Melanoma Cancer (Males):\nAge-standardised rate\n(per 100,000)")

## Make a plot
aus_gghexmap <- plot_grid(
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), 
                 fill = NA, colour = "#696969", data = aus_sa2_mel_fort) +
    geom_polygon(aes(x = long, y = lat, group = hex_id, fill = p50_col), 
                 data = aus_fort_hex) + 
    scale_fill_manual(values = nac_colours) + 
    scale_size_identity() + coord_equal() +
    invthm + guides(fill=FALSE) +
    theme(plot.background = element_rect(fill = "black", colour = NA)),
  cowplot::get_legend(coloursplot + 
                        theme(legend.position = "bottom")),
  ncol=1, rel_heights=c(.85, .15))
aus_gghexmap

ggsave(filename = "figures/aus_gghexmap.png", plot = aus_gghexmap,
       device = "png", bg = "transparent", dpi = 300,  width = 14, height = 12)

# choropleth display
aus_choro_fort <- aus_sa2_mel_fort %>% 
  left_join(SIR_male, by = c("sa2_name_2011" = "SA2_NAME11")) %>% 
  mutate(p50_col = factor(colour_cat(Melanoma), 
                          levels = c("A", "B", "C", "D", "E")))
aus_choro <- plot_grid(
  ggplot() +
    geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon), fill = p50_col), colour = "#696969", size = 0.0001, data = aus_choro_fort) +
    scale_fill_manual(values = nac_colours) + 
    scale_size_identity() + coord_equal() +
    invthm + guides(fill=FALSE) +
    theme(plot.background = element_rect(fill = "black", colour = NA)),
  cowplot::get_legend(coloursplot + 
                        theme(legend.position = "bottom")),
  ncol=1, rel_heights=c(.85, .15))
aus_choro


ggsave(filename = "figures/aus_ggchoro.png", plot = aus_choro,
       device = "png", bg = "transparent", dpi = 300,  width = 14, height = 12)





# Create static plot - geography
geo_plot <- ggplot(hex %>% filter(poly_type == "geography")) + 
  geom_polygon(aes(x = long, y = lat, fill = p50cat, group = interaction(sa2_name_2011, polygon)), colour = "#808080", size = 0.3) + coord_equal() + invthm +
  scale_fill_manual(values = atlas_colours, na.value = "lightgrey") + guides(fill = FALSE)
ggsave(filename = "figures/melanoma_geography.png", device = "png", geo_plot, dpi = 300, unit = "in", width = 16, height = 12)
# Create static plot - hexagon
hex_plot <- ggplot(hex %>% filter(poly_type == "hex")) +
  geom_polygon(aes(x = long, y = lat, fill = p50cat, group = interaction(sa2_name_2011, polygon)), colour = "#808080", size = 0.1) + coord_equal() + invthm +
  scale_fill_manual(values = atlas_colours, na.value = "lightgrey") + guides(fill = FALSE)
ggsave(filename = "figures/melanoma_hex.png", device = "png", hex_plot, dpi = 300, unit = "in", width = 16, height = 12)

qld_grid <- gridExtra::grid.arrange(qld_ggcont, full_ggncont, qld_ggdorl, qld_gghexmap,  nrow = 2)
ggsave(filename = "figures/qld_grid.png", plot = qld_grid,
       device = "png",   bg = "transparent", dpi = 300,  width = 7, height = 6)

                                    

## 
## 
## #####
## # Animate
## library(gganimate)
## 
## sa2_mel_order <- fortify_sfc(sa2_mel ) %>% 
##   left_join(hexmap %>% dplyr::select(sa3_name_2016, focal_dist, rownumber)) %>% filter## (cent_long>110, cent_long<155)
## 
## reveal_points <- ggplot() +
##   geom_sf(data=aus, fill = NA, colour = "grey", size = 0.01) +
##   geom_point(aes(x = cent_long, cent_lat), 
##              data = sa2_mel_order %>% group_by(sa3_name_2016) %>% slice(1)) +
##   transition_reveal(along = rownumber)
## 
## anim_save(reveal_points, reveal_points)
## 
## anim <- ggplot(sa2_mel_order, aes(long, lat, polygon)) +
##   transition_layers(layer_length = 1, transition_length = 2) +
##   enter_fade() + enter_grow()
## 
## anim_save()## 