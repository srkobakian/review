# Create a choropleth of Australia
library(sugarbag)
library(tidyverse)

# Find the shape file
aus <- file.choose()

# Read in shape file, and simplify
aus_sf <- read_shape(shp_path = aus, simplify = TRUE, keep = 0.05)

# Filter islands
aus_sf <- aus_sf %>% 
  filter(!(SA2_NAME11 %in% 
      c("Christmas Island", "Cocos (Keeling) Islands", "Lord Howe Island")))

# Join with cancer data
SIR <- read_csv("data/SIR Downloadable Data.csv") 

SIR <- SIR %>%
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

aus_sf <- aus_sf %>% 
  left_join(SIR_persons) 

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


mel_data <- aus_sf %>% 
  mutate(p50_col = factor(colour_cat(Melanoma), 
    levels = c("A", "B", "C", "D", "E")))


# Create a choropleth
ggplot(mel_data) + geom_sf(aes(label = SA2_NAME11, fill = p50_col), colour = "white", size = 0.1) + scale_fill_manual(values = nac_colours) + guides(fill = FALSE) + theme_void()
ggsave(filename = "choropleth.png", device = "png", width = 9.8, height = 6.8)
