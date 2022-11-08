# libraries ----
library(tidyverse)
library(stringr)
library(tidytext)
library(showtext)
library(usmap)
showtext_auto()
# import data & fonts ----
state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

font_add_google(name = "cormorant garamond",
                family = "Cormorant+Garamond")
main_font <- "Cormorant+Garamond"
# wrangling ----

## state_map is external data
state_map <- usmap::us_map() %>%
  mutate(state = tolower(gsub("_", " ", full))) # replace underscore w/ space; all lower case

state_stations <- state_stations %>%
  mutate(id = row_number(),
         state = tolower(gsub("_", " ", state))) %>%
  group_by(state) %>%
  mutate(total_station_state = length(state)) %>%
  ungroup() %>% 
  unnest_tokens(input = format, output = deconstruct_format)


plot_data <- state_stations %>%
  group_by(id) %>%
  mutate(is_target = any(deconstruct_format == "gospel")) %>% 
  ungroup() %>%
  select(!deconstruct_format) %>%
  distinct() %>%
  group_by(state) %>%
  mutate(prop_target = mean(is_target, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(state_map, by = c("state")) %>% 
  select(x, y, prop_target, group, state) %>%
  distinct()
# plot ----
background_col <- "white"
border_col <- "white"
maintitle <- "Can you hear the Good News?"
subtitle <- "Proportion of radio stations containing the word 'Gospel' in the format"
caption <- "#TidyTuesday 8th Nov 2022 | Data: Radio Stations (Wikipedia) | @_jcken"


#legend_range <- range(rock_stations$prop_rock)

legend_range = range(plot_data$prop_target)

xmid <- mean(range(plot_data$x))
ymin <- min(plot_data$y)

p1 <- ggplot(data = plot_data, aes(x = x, y = y, group = group)) + 
  geom_polygon(aes(fill = prop_target)) + 
  scale_fill_gradient(low = "bisque", high =  "red",
                      breaks = legend_range,
                      labels = scales::percent(legend_range),
                      name = "Proportion of Gospel stations") +
  #geom_text(aes(x = xmid, y =  ymin),
  #         label = "") +
  geom_path(colour = border_col) +
  labs(title = maintitle,
       subtitle = subtitle,
       caption = caption) +
  theme_void() +
  theme(
    legend.position = c(0.75,0),
    legend.direction = "horizontal",
    legend.text.align = 0.5,
    legend.title = element_text(size = 30,
                                hjust = 2000),
    legend.text = element_text(size = 40,
                               vjust = 13),
    plot.background = element_rect(background_col),
    plot.title = element_text(margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "cm"),
                              hjust = 0.5, size = 100),
    plot.subtitle = element_text(margin = margin(t = 0, r = 0, b = 1, l = 0, unit = "cm"),
                                 hjust = 0.5, face = "italic", size = 50),
    plot.caption = element_text(size = 30,
                                hjust = 0,
                                vjust = -1),
    plot.margin = margin(t = 0, r = 1, b = 2, l = 1, unit = "cm"),
    text = element_text(family = main_font)
  )
#,
#       text = element_text(family = main_font))
ggsave("~/tidytuesday/2022/2022-11-08/2022-11-08.png",
       plot = p1,
       height = 6,
       width = 8,
       units = "in")

##axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
##axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()