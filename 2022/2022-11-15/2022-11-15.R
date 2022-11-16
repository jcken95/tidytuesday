# packages ----
library(tidyverse)
library(lubridate)
library(showtext)
library(ColourblindR)
library(patchwork)
library(directlabels)
# load data & functions ----#

image_alt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/image_alt.csv')
color_contrast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/color_contrast.csv')
ally_scores <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/ally_scores.csv')

## part of the data set, but not used in my plot

#bytes_total <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv')
#speed_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/speed_index.csv')

make_name_y <- function(data, colname) {
  data %>%
    rename(y = all_of(colname)) %>%
    select(measure, client, date, y, timestamp)
}

minmax <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  (x - min_x)/(max_x - min_x)
}

my_theme <- theme()

# fonts ----

font_add_google(name = "Raleway", family = "raleway")
showtext_auto()
main_font <- "raleway"

# wrangling ----

join_by <- c("measure", "client", "date", "y", "timestamp")

plot_data <- make_name_y(image_alt, "percent") %>%
  full_join(make_name_y(color_contrast, "percent"), by = join_by) %>%
  full_join(make_name_y(ally_scores, "p50"), by = join_by) %>%
  filter(client == "mobile") %>%
  mutate(date = as_date(date))

# make plot ----
p <- list()
my_theme <- theme(text = element_text(family = main_font),
                  panel.grid = element_blank(),
                  legend.position = "none",
                  plot.title = element_text(size = 32),
                  axis.title = element_text(size = 24),
                  axis.text = element_text(size = 24))


title <- "Making charts more accessible is easy!"
subtitle <- "{ColourblindR} makes adapting graphics for colourblindness straightforward"
caption <- "#TidyTuesday 15/11/2022 | Data: httparchive.org | @_jcken"

line_labels <- tibble(
  measure = unique(plot_data$measure),
  x = rep(2022, 3),
  y = c(20, 40, 60)
)

# default scheme
base_graph <- ggplot(plot_data,
                     aes(x = date, y = y, colour = measure, group = measure)) +
  geom_line() +
  geom_dl(aes(label = measure),
          method = list(dl.trans(x = x - 2, y = y + 0.2), "last.points",
                        cex = 2))  +
  ylab("measure") +
  ylim(c(0, 100)) +
  my_theme

# default
p[[1]] <- base_graph + ggtitle("Default")

# trita scheme
p[[2]] <- base_graph +
  ggtitle("Trita") +
  theme_trita("colour")

# deutera scheme
p[[3]] <- base_graph +
  ggtitle("Deutera") +
  theme_deutera("colour")

# prota scheme
p[[4]] <- base_graph +
  ggtitle("Prota") +
  theme_prota("colour")

p_full <- patchwork::wrap_plots(p) +
  plot_annotation(title = title,
                  subtitle = subtitle,
                  caption = caption,
                  theme = theme(
                    text  = element_text(family = main_font),
                    plot.title = element_text(size = 80, hjust = 0.5),
                    plot.subtitle = element_text(size = 40, hjust = 0.5),
                    plot.caption = element_text(hjust = 0.5, size = 40)
                  )
  )
p_full
# save plot ----
ggsave(
  filename = "~/tidytuesday/2022/2022-11-15/2022-11-15.png",
  plot = p_full,
  width = 8, height = 6,
  units = "in")

