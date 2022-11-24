# pkgs & data ----
library(tidyverse)
library(osmdata)
library(showtext)
#library(ggmap)
#library(rvest)

museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')

# fonts ----
font_add_google(name = "cormorant garamond",
                family = "Cormorant+Garamond")
main_font <- "Cormorant+Garamond"
showtext_auto()
# wrangling ----
## lake district postcodes begin with CA or LA
## stolen some code from NR to clean up the dates:
## https://github.com/nrennie/tidytuesday/blob/main/2022/2022-11-22/20221122.R

museums_la_ca <- museums %>% 
  separate(Year_opened, into = c("opened1", "opened2"), sep = ":") %>% 
  separate(Year_closed, into = c("closed1", "closed2"), sep = ":") %>% 
  mutate(across(c(opened1, opened2, closed1, closed2), as.numeric), 
         Area_Deprivation_index = factor(Area_Deprivation_index, levels = 1:10)) %>% 
  mutate(across(c(closed1, closed2), ~if_else(.x == 9999, Inf, .x))) %>% 
  mutate(closed = case_when(closed1 == closed2 ~ closed1,
                            closed1 != closed2 ~ round((closed2 + closed1)/2))) %>% 
  mutate(opened = case_when(opened1 == opened2 ~ opened1,
                            opened1 != opened2 ~ round((opened2 + opened1)/2))) %>% 
  rename(deprivation = Area_Deprivation_index) %>% 
  mutate(outcode = word(Postcode)) %>%
  mutate(outcode_number = gsub("[0-9]*$","",outcode)) %>%
  filter(outcode_number %in% c("LA", "CA"),
         closed > 2022)

## grab a map of Cumbria via OpenStreetMap

# plot ----

maintitle <- "Forgot your coat?"

## grab a map via OpenStreetMap

#pale_blue <- "#ADD8E6"
pale_blue <- "#79CBE6"
cumbria_opq <- opq(bbox = 'Cumbria, UK')
lakes <- cumbria_opq %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf()
coastline <- cumbria_opq %>%
  add_osm_feature(key = 'natural', value = 'coastline') %>%
  osmdata_sf()


map_plot <- ggplot() + 
  geom_sf(data = lakes$osm_polygons,
          inherit.aes = FALSE,
          color = pale_blue) +
  geom_sf(data = coastline$osm_lines,
          inherit.aes = FALSE,
          color = pale_blue,
          alpha = 0.5) +
  geom_point(data = museums_la_ca,
             aes(x = Longitude, y = Latitude),
             size = 0.3) +
  theme_void() +
  annotate("text", x = -5, y = 55.1,
           label = maintitle,
           family = main_font, 
           size = 12) +
  annotate("text", x = -5, y = 54.8,
           label = "Map of all currently operating museums in",
           family = main_font, 
           size = 6) +
  annotate("text", x = -5, y = 54.65,
           label = "the Lake District and surrounding areas",
           family = main_font, 
           size = 6) +
  annotate("text", x = -5, y = 54.5,
           label = "according to mappingmuseums.org",
           family = main_font, 
           size = 6) +
  annotate("text", x = -5, y = 54.1,
           label = "#TidyTuesday 22/11/2022 | Data: mappingmuseums.org | @_jcken",
           family = main_font, 
           size = 4) +
  xlim(-6, -2.15) +
  theme(text = element_text(family = main_font),
        plot.background = element_rect(fill = "white", colour = "white"))

# save ----

ggsave(filename = "~/tidytuesday/2022/2022-11-22/2022-11-22.png",
       plot = map_plot, width = 800, height = 500, units = "px")

