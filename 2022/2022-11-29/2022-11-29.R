# pkgs & data ----
library(tidyverse)
library(stringr)
library(ggimage)
library(ggflags)
library(countrycode) ## turns names to 2 letter codes; handy for ggflags
library(showtext)
wcmatches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

# functions ----

get_rank <- function(stage) {
  ## assign an ordinal rank to each group stage
  case_when(
    stage == "DNQ" ~ 0,
    stage == "Group" ~ 1,
    stage == "Round of 16" ~ 2,
    stage == "Quarterfinals" ~ 3,
    stage == "Semifinals" ~ 4,
    stage == "Final round" ~ 5,
    stage == "Final" ~ 5
  )
}


# fonts ----
font_add_google(name = "russo one",
                family = "russo+one")
main_font <- "russo+one"
showtext_auto()
# wrangling ----



the_years <- wcmatches %>%
  select(year) %>%
  distinct()

last_england_games <- wcmatches %>%
  filter(stage != "Third place") %>% ## want to see who knocked Eng out => rm 3rd place games
  mutate(game_id = row_number(),
         stage = if_else(
           word(stage, 1) == "Group",
           "Group",
           stage)) %>%
  group_by(row_number()) %>% 
  mutate(england_game = (home_team  == "England" | away_team == "England")) %>% 
  ungroup() %>%
  filter(england_game) %>%
  group_by(year) %>%
  mutate(last_england_game = max(date)) %>%
  ungroup() %>%
  group_by(last_england_game) %>%
  mutate(is_last = (date == last_england_game)) %>%
  ungroup() %>%
  filter(is_last) %>%
  mutate(knockout = case_when(
    year == 1966 ~ "Champions",
    stage == "Group" ~ "Group",
    TRUE ~ if_else(
      home_team == "England",
      away_team,
      home_team))) %>%
  select(year, stage, home_team, away_team, knockout) %>%
  full_join(the_years, by = "year") %>%
  mutate(knockout = if_else(is.na(knockout), "DNQ", knockout),
         stage = if_else(is.na(stage), "DNQ", stage), # years England did not qualify (for any reason)
         rank = get_rank(stage))

group_games <- last_england_games %>% filter(knockout == "Group")
dnq <- last_england_games %>% filter(knockout == "DNQ")
knockout_games <- last_england_games %>%
  filter( !(knockout %in% c("Group", "Champions", "DNQ"))) %>%
  mutate(country_code = tolower(countrycode(knockout, "country.name", "iso2c")))
champions <- last_england_games %>%
  filter(knockout == "Champions") 
img_path <- "~/tidytuesday/2022/2022-11-29/moore.png"

y_labs <- tibble(
  rank = 0:5,
  x = rep(min(the_years$year) - 4, 6),
  lab = c("DNQ", "Group", "R16", "Quarter final", "Semi final", "Final")
)

# plot ----

clrs <- list(
  green = "#7CFC00",
  text_col = "white"
)

dot_size = 6
flag_size = 6
letter_size = 10


title <- "Who sent us home?"
subtitle <- "Flags show which team knocked England out of the FIFA Men's world cup.\nIf England did not qualify, a D is used, exit at the group stage is represented by a G.\nYou all know what happened in 1966.\n#TidyTuesday 29/11/22 | Data: FIFA World Cup | @_jcken"


wc_plot <- ggplot() +
  geom_point(data = last_england_games,
             aes(x = year, y = rank),
             pch = 19, size = dot_size) +
  ggflags::geom_flag(data = knockout_games,
                     aes(x = year, y = rank, country = country_code),
                     size = flag_size) +
  geom_point(data = dnq,
             aes(x = year, y = rank),
             pch = "D", colour = clrs$text_col,
             size = letter_size) +
  geom_point(data = group_games,
             aes(x = year, y = rank),
             pch = "G", colour = clrs$text_col,
             size = letter_size) +
  geom_image(data = champions,
             aes(x = year, y = rank, image = img_path),
             size = 0.15, by = "height") +
  geom_text(data = y_labs,
            aes(x = x - 26, y = rank + 0.35, label = lab),
            size = 10, family  = main_font, hjust =  0) +
  scale_x_continuous(breaks = the_years$year,
                     limits = c(1900, 2028)) +
  scale_y_continuous(breaks = 0:5, limits = c(-1, 6)) +
  ggtitle(title,
          subtitle = subtitle) +
  theme_minimal() +
  theme(text = element_text(family = main_font),
        plot.background = element_rect(colour = clrs$green, fill = clrs$green),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(hjust = 0.65, size = 20),
        axis.text.x = element_text(angle = 45, size = 12),
        plot.title = element_text(family = main_font, size = 40, hjust = 0.5),
        plot.subtitle = element_text(family = main_font, size = 14, hjust = 0.5))
# save ----

ggsave(filename = "~/tidytuesday/2022/2022-11-29/2022-11-29.png",
       plot = wc_plot, width = 1200 * 0.75,  height = 1600 * 0.75, units = "px")

