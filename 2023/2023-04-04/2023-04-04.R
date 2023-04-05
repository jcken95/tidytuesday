# data & libraries ----
library(tidyverse)
library(ggsankey)
library(showtext)

soccer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv')

# fonts & colours ----

font_add_google(name = "Fugaz One", family = "main")
showtext_auto()


green <- "#6DBB63"
txt <- rgb(1, 1, 1, alpha = 0.9)

# functions ---

recode <- function(x) {
  case_when(
    x == "ITR" ~ "Kick off",
    x == "HTR" ~ "Half time",
    x == "FTR" ~ "Full time",
    is.na(x) ~ NA) %>%
    ordered(levels = c("Kick off", "Half time", "Full time"))
}

# wrangling ----

plot_data <- soccer %>%
  select(HTR, FTR) %>%
  mutate(ITR = "D") %>%
  make_long(ITR, HTR,  FTR) %>%
  mutate(x = recode(x),
         next_x = recode(next_x))

results = tibble(
  y = c(-200, 0, 200),
  x = 0,
  result = c("Lose", "Draw", "Win"))
  
# plot ----

subtitle <- "A team who is winning at half time will probably win the match.\nIf there is a draw at half time, the final result is much harder to predict."
caption <- "#TidyTuesday 04/04/2023 | Data: EPL | @_jcken"
result_size <- 6

plot_data %>%
  ggplot(aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node)) +
  geom_sankey(fill = "white",
              alpha = 0.95) +
  annotate(geom  = "text", x = 3.4, y = 200,
           label = "Home Win", colour = txt,
           family = "main", size = result_size) +
  annotate(geom  = "text", x = 3.4, y = 0,
           label = "Draw", colour = txt,
           family = "main", size = result_size) +
  annotate(geom  = "text", x = 3.4, y = -200, 
           label = "Home Lose", colour = txt,
           family = "main", size = result_size) +
  theme_minimal() +
  coord_cartesian(xlim = c(1.5, 3.3)) +
  labs(x = NULL, y = NULL,
       title = "They think it's all over ... it usually is at half time",
       subtitle = subtitle,
       caption = caption) +
  theme(legend.position = "none",
        text = element_text(family = "main"),
        plot.title = element_text(hjust = 0.5, colour = "white", size = 20),
        plot.subtitle = element_text(hjust = 0.5, colour = "white",
                                     lineheight = 0.5),
        plot.caption = element_text(hjust = 0.5, colour = "white"),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = txt, family = "main",
                                   size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = green, colour = green),
        plot.background = element_rect(fill = green)) 

# save ----

ggsave("2023/2023-04-04/2023-04-04.png",
       width = 800,
       height = 800, 
       units = "px")
