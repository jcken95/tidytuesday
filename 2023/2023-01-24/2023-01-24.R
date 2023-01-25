# data, libs, fns ----
library(tidyverse)
library(showtext)
survivalists <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv")

## included on tudytuesday page but not required
#loadouts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv")
#episodes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv")
#seasons <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv")

# fonts & clrs
bg_clr = "grey20"
bullet_clr = "grey80"
text_clr = "grey80"
bar_clr = "grey80"

showtext_auto()
font_add_google("yantramanav")
main_font <- "yantramanav"

# wrangling ----

plot_data <- survivalists %>%
  group_by(season) %>%
  summarise(min_days = min(days_lasted),
            max_days = max(days_lasted)) %>%
  mutate(range = max_days - min_days)

# plot ----

subtitle <- str_wrap("Minimum number of days survived in Alone appears to be increasing; maximum days survived shows no clear pattern",
                     width = 50)
tag <- "#TidyTuesday | Data: Alone | @_jcken"

bullet_size = 2.2
p <- plot_data %>%
  ggplot() +
  geom_segment(aes( y = season, yend = season,
                    x = min_days, xend = max_days),
               colour = bar_clr,
               linewidth = 3) +
  geom_point(aes(x = min_days, y = season),
             colour = bullet_clr, size = bullet_size) +
  geom_point(aes(x = max_days, y = season),
             colour = bullet_clr, size = bullet_size) +
  annotate("text", x = -10, y = 10, label = "season",
           colour = bullet_text_clr, family = main_font, size = 10) +
  geom_text(aes(x = rep(-10, max(season)), y = season, label = season),
            colour = bullet_text_clr, family = main_font, size = 10) +
  scale_x_continuous(breaks = seq(from = 0, to = max(plot_data$max_days), by = 20),
                     limits = c(-20, max(plot_data$max_days) + 20)) +
  ylim(1, 10) +
  xlab(paste0("Range of number of days survived", "\n", tag)) +
  ylab("") +
  labs(title =  "Are Alone players improving?",
      subtitle = subtitle) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = main_font, size  = 32), 
    plot.subtitle = element_text(hjust = 0.5, family = main_font, size = 18, lineheight = 0.3),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = bg_clr),
    text = element_text(colour = text_clr, family = main_font),
    axis.text = element_text(colour = bullet_text_clr, family = main_font, size = 28),
    axis.text.y = element_blank(),
    axis.title.x = element_text(colour = bullet_text_clr, family = main_font, size = 18, lineheight = 0.3))

# save ----
ggsave(
  paste0(here::here(), "/2023/2023-01-24/2023-01-24.png"),
  p,
  width = 800,
  height = 800 * sqrt(2),
  units = "px")
      
