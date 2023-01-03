# packages ----

library(tidyverse)
library(tidytext)
library(SemNetCleaner)
library(showtext)
# data ----

thesis <- readr::read_csv("thesis.csv")

# fonts + clrs ----
showtext_auto()
font_add_google("Permanent Marker")
main_font <- "Permanent Marker"

# clrs lifted from vapoRwave
#scales::show_col(vapoRwave:::newRetro_palette)

bg <- "#4A354F"
txt <- "#FF4373"
bars <- "#9239F6"
# wranging ----

thesis <- thesis %>%
  anti_join(stop_words, by = "word") %>%  # remove stop words
  filter(is.na(as.numeric(word))) %>% # remove numbers
  filter( !(word %in% c("table", "equation", "align", "chapter"))) %>%  # filter out some common latex environments
  group_by(word) %>%
  mutate(word = singularize(word[1])) %>%
  mutate(n_word = length(word)) %>%
  ungroup() 

top_ten_words <- thesis %>%
  select(word, n_word) %>%
  distinct() %>% 
  arrange(-n_word) %>%
  slice(1:10) %>%
  mutate(rank = 1:10)

# plot(s) ----
  
top_ten_words %>%
  ggplot() +
  geom_col(aes(y = reorder(word, rank), x = n_word),
           colour = bars, fill = bars, width = 0.5) +
  geom_text(
    mapping = aes(x = 5, y = word, label = toupper(word)),
    hjust = 0,
    nudge_x = 0.25,
    color = txt,
    size = 8,
    family = main_font
  ) +
  geom_text(
    mapping = aes(x = n_word, y = reorder(word, rank), label = n_word),
    hjust = 1,
    nudge_x = -5,
    color = txt,
    size = 8,
    family = main_font
  ) +
  annotate("text", x = 75, y = 7.6,
           label = "abbreviation of Gaussian Process",
           family = main_font,
           colour = txt,
           size = 5
  ) +
  scale_y_discrete(breaks = NULL) +
  labs(title = "Thesis Wrapped",
          subtitle = "Top ten most used words (adjusting for plurals) in my PhD thesis",
          caption = "#TidyTuesday 03/01/2023 | Data: bring your own! | @_jcken") +
  xlab("Word Count") +
  ylab("") +
  theme(
    text = element_text(family = main_font, colour = txt),
    plot.title = element_text(hjust = 0.5, size = 36),
    plot.subtitle = element_text(hjust = 0.5, size = 20),
    plot.caption = element_text(hjust = 0.5, size = 16),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = bg),
    panel.background = element_rect(fill = bg, colour = bg),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(family = main_font, colour = txt, size = 16),
    axis.ticks = element_line(colour = txt)
    )

# save ----

ggsave("2023-01-03.png",
       height = 1600,
       width= 1000,
       units = "px"
       )

