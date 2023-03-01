# packages & data ----
library(tidyverse)
library(waffle)
library(ggtext)
library(showtext)

afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')

# fonts & functions ----

showtext_auto()
font_add_google(name = "cormorant garamond",
                family = "Cormorant+Garamond")
main_font <- "Cormorant+Garamond"

# wrangling ----http://127.0.0.1:34385/graphics/plot_zoom_png?width=1025&height=773

label_count = afrisenti %>%
  group_by(language_iso_code) %>% 
  count(label) %>%
  mutate(n_prop = n / sum(n)) %>%
  ungroup() %>%
  rename(language_label_count = n) %>%
  left_join(languages)

overall <- afrisenti %>%
  count(label)

total_n <- nrow(afrisenti)

# plot ----

## colour at titles
clrs <- c("#FC8D62", "#8DA0CB", "#E78AC3")

main_text <- glue::glue(
  "{total_n} tweets from 14 African languages were categorised as
    <span style = 'color:{clrs[1]};'>positive</span>,
    <span style = 'color:{clrs[2]};'>neutral</span>,
    or <span style = 'color:{clrs[3]};'>negative</span>.")

tag_text <- "#TidyTuesday 28/02/2023 | Data: AfriSenti | @_jcken"

## start

faceted <- label_count %>% 
  ggplot(aes(values = language_label_count, fill = label)) +
  geom_waffle(n_rows = 10, size = 0.1, flip = TRUE,
              make_proportional = TRUE, colour = "white") +
  facet_wrap(~ language, ncol = 7,
             labeller = label_wrap_gen(20)) +
  scale_fill_manual(
    name = NULL,
    values = c("positive" = "#FC8D62", 
               "neutral" = "#8DA0CB",
               "negative" = "#E78AC3")) +
    #labels = c("positive", "neutral", "negative")) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        text = element_text(family = main_font,
                            size = 16,
                            lineheight = 0.3))

text_panel <- ggplot() +
  annotate(
    "richtext",
    x = 0, y = 0,
    label = main_text,
    hjust = 0,
    vjust = 0.5,
    size = 8,
    family = main_font,
    label.color = NA) +
  ggtitle("AfriSenti: A Twitter Sentiment Analysis Benchmark for African Languages") +
  xlim(0, 5) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5,
                                  family = main_font,
                                  size = 24),
        text = element_text(lineheight = 0.1))

tag_panel <-  ggplot() +
  annotate(
    "richtext",
    x = 0, y = 0,
    label = tag_text,
    hjust = -0.3,
    vjust = 1,
    size = 8,
    family = main_font,
    label.color = NA) +
  xlim(0, 5) +
  theme_void()

p <- patchwork::wrap_plots(text_panel,
                           faceted,
                           tag_panel,
                           ncol = 1,
                           heights = c(0.5, 3, 1))

ggsave(
  filename = here::here("2023/2023-02-28/2023-02-28.png"),
  plot = p, 
  height = 800, 
  width = 1200,
  unit = "px")

