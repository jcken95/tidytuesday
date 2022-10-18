library(tidyverse)
library(tidytext)
library(showtext)
showtext_auto()
# Load data ----
episodes <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv")
dialogue <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv")

nrc_sentiment <- get_sentiments("nrc")
# Wrangling ----

# extract words
all_words <- dialogue %>%
  select(!c(raw_text, stage_direction)) %>% # remove ugly cols
  pivot_longer(cols = dialogue, values_to = "dialogue") %>% # Give each word its own column
  select(!name) %>%
  unnest_tokens(word, dialogue) %>%
  drop_na() %>%
  anti_join(stop_words)
# give each episode a unique ID (just order them by row num)
episode_number <- all_words %>%
  select(episode, season) %>%
  distinct() %>%
  mutate(episode_no = row_number())
# attach unique ID
all_words <- all_words %>%
  left_join(episode_number, by = c("episode", "season"))
# count number swear words
# use the words collated in lexicon::profanity_arr_bad
all_words <- all_words %>%
  group_by(episode_no) %>%
  mutate(n_swears = sum(word %in% lexicon::profanity_arr_bad)) %>%
  ungroup()
all_words <- all_words %>%
  left_join(episode_number)

episode_writer <- episodes %>%
  select(season, episode, written_by) %>%
  group_by(written_by) %>%
  mutate(n_episode_writer = length(episode)) %>%
  ungroup()

writer_swears <- all_words %>%
  left_join(episode_writer) %>%
  select(season, episode, episode_no, n_swears, written_by) %>%
  distinct()

writer_swears <- writer_swears %>%
  group_by(written_by) %>%
  mutate(total_swears = sum(n_swears),
         total_episodes = length(season)) %>% #total num of episodes by writer
  ungroup() %>%
  select(written_by, total_swears, total_episodes) %>%
  distinct() %>%
  mutate(mean_swears = total_swears / total_episodes)

## make an initial column for tider labels
writer_swears <- writer_swears %>%
  mutate(initials = c("D Bros", "JM", "JD", "AT", "JNL", "DP & D Bros",
                      "PD", "KT", "WB", "CG", "CS"))
# want to order from least to most swear per episode
plotting_order <- writer_swears$initials[sort(
  writer_swears$mean_swears, index.return = TRUE)$ix]
# Make plot ----
## Set some plotting params ----
very_pale_red <- rgb(1, 0, 0, alpha = 0.2)
pale_red <- rgb(1, 0, 0, alpha = 0.4)
dark_red <- rgb(1, 0, 0, alpha = 0.8)
font_add_google(name = "Libre Baskerville", family = "specimen")
myfont <- "specimen"
subtitle <- "Bar chart showing the average number of swear words per episode\nof the 11 'Stranger Things' writers.\nTidy Tuesday 18 Oct 2022 | Data: 8flix |  @_jcken"

p <- writer_swears %>%
  ggplot(aes(x = initials, y = mean_swears)) +
  geom_col(colour = dark_red, fill = pale_red) +
  xlab("") +
  ylab("Mean number profanities per episode") +
  theme_minimal() +
  scale_x_discrete(limits = plotting_order) +
  scale_y_continuous(breaks = c(10, 20, 30), minor_breaks = c(10, 20, 30)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.8,
                                   colour = dark_red, face = "bold",
                                   family = myfont),
        axis.text.y = element_text(color = dark_red),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = very_pale_red), #very_pale_red
        panel.background = element_rect(fill = "grey20", colour = "grey20"),
        plot.background = element_rect("grey20"),
        title = element_text(colour = dark_red, family = myfont, face = "bold"),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(face = "italic")) +
  labs(title = "WHICH STRANGER THINGS WRITERS\n USE THE STRONGEST LANGUAGE?",
       subtitle = subtitle) 
p
