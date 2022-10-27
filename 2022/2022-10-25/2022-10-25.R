library(tidyverse)
# load data ----
bakers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/bakers.csv')
challenges <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/challenges.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/episodes.csv')
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/ratings.csv')

# wrangling & functions ----
RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)
is_even <- function(x) {
  if (x %% 2  == 0) {
    return(TRUE)
  } else{
    return(FALSE)
  }
}

is_even2 <- Vectorize(is_even)

ratings <- ratings %>% 
  mutate(ep_id = row_number())

ratings_pre_8 <- ratings %>%
  filter(series <  8)
ratings_post_8 <- ratings %>%
  filter(series >= 8)

series <- episodes %>%
  select(episode, series) %>%
  mutate(ep_id = row_number()) %>%
  group_by(series) %>%
  mutate(mean_ep = mean(ep_id)) %>%
  ungroup() %>%
  mutate(y = rep(0, length(episode)),
         name = paste0("Series ", series))

# colours ----
main_col <- "grey20"#RColorBrewer::brewer.pal(n  = 8, name = "Set2")[1]
second_col <- RColorBrewer::brewer.pal(n  = 8, name = "Set2")[4]
pts1 <- RColorBrewer::brewer.pal(n  = 8, name = "Set2")[4]
pts2 <- RColorBrewer::brewer.pal(n  = 8, name = "Set2")[3]

subtitle <- "Viewer numbers under the BBC showed clear growth; there is no statistical\nevidence to suggest viewer numbers are growing on Channel 4\n#TidyTuesday 25 Oct 2022 | Data: {bakeoff} | @_jcken"
ratings %>% 
  ggplot(
    aes(x = ep_id, y = viewers_7day)) +
  geom_point(aes(colour = is_even2(series))) +
  geom_smooth(
    method = "lm",
    mapping = aes(x = ep_id, y = viewers_7day),
    data = ratings_pre_8,
    colour = second_col,
    se = FALSE) +
  scale_colour_manual(values = c(pts1, pts2)) +
  geom_smooth(method = "lm",
              mapping = aes(x = ep_id, y = viewers_7day),
              data = ratings_post_8,
              colour = second_col,
              se = FALSE) +
  geom_text(
    mapping = aes(x = mean_ep, y = y),
    label = series$name, colour = second_col,  size = 3.5,
    data = series) +
  ylab("Viewing figures after 7 days\n(millions of people)") +
  xlab("Episode grouped by series") +
  ggtitle("GBBO viewership on Channel 4 is stagnant",
          subtitle = subtitle) +
  geom_segment(
    aes(x = 64.5, xend = 64.5, y = 0.5, yend = 16),
    colour = second_col,
    lty = 2,
    lwd = 0.1) +
  geom_segment(
    mapping = aes(x = 85,
                  xend = 85,
                  y = 6.5,
                  yend = 9),
    arrow = arrow(type = "open",
                  length = unit(0.05, "inches")),
    size = 0.3,
    colour = second_col) + 
  annotate(
    geom  = "text",
    x = 85, y = 5.5,
    label = "95% CI for slope \nparameter contains 0",
    colour = second_col,
    size = 5) +
  geom_segment(
    mapping = aes(x = 47,
                  xend = 63.5,
                  y = 5,
                  yend = 7),
    arrow = arrow(type = "open",
                  length = unit(0.05, "inches")),
    size = 0.3,
    colour = second_col) + 
  annotate(
    geom  = "text",
    x = 47, y = 4,
    label = "GBBO moved to Channel 4\nafter series 7",
    colour = second_col,
    size = 5) + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = second_col),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = main_col, colour = main_col),
    plot.background = element_rect(main_col),
    title = element_text(face = "bold", second_col, size = 16),
    plot.subtitle = element_text(face = "italic", size = 12),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    legend.position = "none",
    text = element_text(colour = second_col),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(colour = second_col))

## save as 800(w) x 600(h) .png
