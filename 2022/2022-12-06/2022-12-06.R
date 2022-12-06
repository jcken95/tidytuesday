# pkgs & data ----
## need {vapoRwave} from github
#devtools::install_github("moldach/vapoRwave")
library(tidyverse)
library(showtext)
library(janitor)
library(vapoRwave)
elevators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-06/elevators.csv')
# fonts ----
font_add_google(name = "VT323",
                family = "VT323")
main_font <- "VT323"
showtext_auto()

# wrangling ----

plot_data <- elevators %>% 
  clean_names() %>%
  filter(longitude > -77,
         device_status == "A")  #  seems to be one rogue point

manhattan <- plot_data %>%
  filter(borough == "Manhattan") %>%
  select(latitude, longitude)

manhattan_bb <- tibble(xmin = min(manhattan$latitude), xmax = max(manhattan$latitude),
                       ymin = min(manhattan$longitude), ymax = max(manhattan$longitude))

# plot ----

subtitle <- "Where elevators are active, their density is mostly constant.\nThe exception is the borough of Manhattan, which has a high elevator density."
caption <- "\n#TidyTuesday 6/12/2022 | Data: Elevators | @_jcken"
p <- ggplot(plot_data,
       aes(x = latitude, y = longitude)) +
  stat_bin_2d(bins = 150) +
  new_retro() + 
  scale_fill_gradient(low = "#6F3460", high = "#FF0076") +
  geom_rect(data = manhattan_bb,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax),
            color = "#FFB967", fill = "transparent",
            lty = 2, linewidth = 0.25,
            inherit.aes = FALSE) + 
  ggtitle("Density of Active Elevators in New York",
          subtitle = subtitle) +
  annotate(geom = "text", x = 40.8, y = -74.05, label = "Borough of Manhattan",
           colour = "#FA5F70FF",  family = main_font, size = 8) +
  labs(caption = caption) +
  theme(plot.title = element_text(family = main_font, size = 26, hjust = 0),
        plot.subtitle = element_text(family = main_font, size = 20, hjust = 0, vjust = 1),
        axis.text = element_text(family = main_font),
        legend.text = element_text(family = main_font, size = 22, hjust = -3),
        legend.title = element_text(family = main_font, size = 22, hjust = 0.1, vjust = -3),
        axis.title.x = element_text(family = main_font, size = 22,  hjust = 0.5),
        axis.title.y = element_text(family = main_font, size = 22, angle = 90, hjust = 0.5),
        text = element_text(family = main_font, size = 18, hjust = 1))

# save ---

ggsave(filename = "~/tidytuesday/2022/2022-12-06/2022-12-06.png",
       plot = p,
       width = 1000, height = 1200, units = "px")

