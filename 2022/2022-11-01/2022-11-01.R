# Libraries ----
library(tidyverse)
library(tidytext)
library(lubridate)
# My functions ----
get_decade <- function(the_date) {
  ## dont use if data spans more than 100 years
  ## because decades will repeat
  year <- lubridate::year(the_date)
  dec <- floor(year / 10) * 10 ## just removes last digit
  dec
}
get_decade_vec <- Vectorize(get_decade)

# Load data ----
horror_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')

# Wranging ----
vampire_words =  c("Vampire", "vampire", "Dracula", "dracula")
# First find if vampire_words is in any of the text fields
horror_movies <- horror_movies %>%
  mutate(decade = get_decade_vec(release_date))
is_vampire <- horror_movies %>%
  # remove cols I don't need - keep things light
  select(id, original_title, title , overview, tagline) %>%
  unite(original_title, title , overview, tagline,
        col = all_text, sep = " ") %>%
  unnest_tokens(words, all_text) %>%
  mutate(vampire_present = words %in% vampire_words) %>%
  select(id, vampire_present) %>%
  distinct()
num_vampire_movies <- sum(is_vampire$vampire_present)
mean_vampire_movies <- mean(is_vampire$vampire_present)

horror_movies <- horror_movies %>%
  group_by(decade) %>%
  mutate(movie_in_decade  = length(id)) %>%
  ungroup()

all_movie_decade <- horror_movies %>%
  select(decade, movie_in_decade) %>%
  distinct()

vampire_movies <- horror_movies %>%
  left_join(is_vampire,  by  = "id") %>%
  filter(vampire_present) %>%
  group_by(decade) %>%
  mutate(vampire_in_decade  = length(id)) %>%
  ungroup()

all_vampire_decade <- vampire_movies %>%
  select(decade, vampire_in_decade) %>%
  distinct()


all_movie_decade <- all_movie_decade %>%
  left_join(all_vampire_decade, by = "decade") %>% 
  mutate(prop_vampire = vampire_in_decade / movie_in_decade,
         above_avg = if_else(prop_vampire > mean(prop_vampire),
                             emoji::emoji("vampire"),
                             emoji::emoji("garlic")))

# plot ----

# colours
main_col <- "#40006f"
second_col <- "#E89128"
# set up annotations
arrow_vampire <- data.frame(x1 = 1960, x2 = 1965,
                            y1 = 0.04, y2  = 0.06)
arrow_garlic <- data.frame(x1 = 2015, x2 = 2005,
                           y1 = 0.06, y2  = 0.045)
my_emoji <- c(emoji::emoji("garlic"), emoji::emoji("vampire")) # (FALSE, TRUE)
title_font <- "serif"
subtitle <- "Movies with 'Vampire' or 'Dracula' mentioned in the title, overview or tagline are\ndecreasingly common and at an all time low.\n\n#TidyTuesday 1 Nov 2022 | Data: The Movie Database | @_jcken"

p <- all_movie_decade %>%
  ggplot() +
  geom_point(aes(x = decade, y = prop_vampire,
                 shape = above_avg), size = 18) +
  scale_shape_manual(values = my_emoji) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1), limits = c(0, 0.1),
                     labels = scales::percent)+
  scale_x_continuous(breaks = seq(from = 1950, to = 2020, by = 10)) +
  xlab("Decade") +
  ylab("Percentage of movies with vampires") + 
  ggtitle("Out for the Count!",
          subtitle = subtitle) +
  geom_curve(mapping = aes(x = x1, y = y1, xend = x2, yend = y2),
             data = arrow_vampire,
             arrow = grid::arrow(type = "open", length = unit(0.05, "npc")),
             colour = second_col, curvature = -0.3) +
  geom_text(aes(x = x1, y = y1 - 0.01),
            data = arrow_vampire,
            label = "Vampires indicate higher than\naverage proportion\nof vampire movies",
            colour = second_col, family = "serif") +
  geom_curve(mapping = aes(x = x1, y = y1, xend = x2, yend = y2),
             data = arrow_garlic,
             arrow = grid::arrow(type = "open", length = unit(0.05, "npc")),
             colour = second_col, curvature = -0.2) +
  geom_text(aes(x = x1 - 1, y = y1 + 0.01),
            data = arrow_garlic,
            label = "Garlic indicates lower\nthan average proportion\nof vampire movies",
            colour = second_col, family = "serif") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(main_col),
    plot.title = element_text(hjust = 0.5),
    title = element_text(colour = second_col, size = 30,
                         family = title_font),
    plot.subtitle = element_text(face = "italic", size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.x = element_text(colour = second_col, size = 12),
    axis.text.y = element_text(colour = second_col, size = 12),
    legend.position = "none",
    text = element_text(colour = second_col, family = "serif"),
    axis.ticks.x = element_blank(),
    #axis.ticks.y = element_line(colour = second_col)
  )
p



ggsave(filename ="~/tidytuesday/2022/2022-11-01/2022-11-01.png", plot = p,
       width = 2500, height = 2000, unit = "px")
