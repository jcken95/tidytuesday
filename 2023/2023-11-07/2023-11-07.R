# packages & data ---
library("tidyverse")
library("patchwork")
library("PrettyCols")
library("ggrepel")
library("showtext")

source("2023/2023-11-07/functions.R")
house <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-07/house.csv")

# fonts & colours ----
pal <- PrettyColsPalettes$Fun[[1]][1:2]

showtext_auto()
font_add_google("newsreader")
main_font <- "newsreader"

# wrangling ----

# Lots of "junk" names, let's remove as many as we can think of
junk_names <- c(
  "VOID", "BLANK", "SCATTERING", "MISCELLANEOUS", "UNDERVOTES",
  "INDEPENDENT", "EXHAUSTED", "VOTEFOREDDIECOM"
)

candidate_names <- house %>%
  filter(candidate != "WRITEIN") %>%
  mutate(first_name = word(candidate, 1)) %>%
  # remove punctuation
  mutate(first_name = gsub("[[:punct:] ]+", "", first_name)) %>%
  filter(nchar(first_name) > 2) %>%
  filter(!first_name %in% c(junk_names)) %>%
  group_by(year) %>%
  count(first_name) %>%
  # arrange not needed - just for my own interest
  arrange(year, -n) %>%
  ungroup() %>%
  mutate(first_name = tolower(first_name))


pairs <- tribble(
  ~pair, ~name,
  1, "jack",
  1, "sally",
  2, "sid",
  2, "nancy",
  3, "bruce",
  3, "robin",
  4, "bonnie",
  4, "clyde"
) %>%
  nest(.by = pair, .key = "chosen_names") %>%
  select(-pair)


all_plots <- map(
  .x = pairs$chosen_names,
  .f = ~ plot_fn(.x) + scale_colour_manual(values = pal)
)

plotwise_theme <- theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    text = element_text(family = main_font),
    axis.text = element_text(family = main_font)
  )


# Right now this is somze wizardy I got off stackoverflow for "global" x and y labs using patchwork
# https://stackoverflow.com/questions/73173079/r-how-can-i-add-global-spanning-x-and-y-axes-to-a-grid-of-ggplot-plots-with-pat

p_axis <- ggplot() +
  labs(
    y = "Proportion of candidates with a given first name",
    x = "Year of election"
  ) +
  theme(text = element_text(family = main_font),
        axis.title = element_text(family = main_font, size = 16))
x_axis <- cowplot::get_plot_component(p_axis, "xlab-b")
y_axis <- cowplot::get_plot_component(p_axis, "ylab-l")

design <- "
FAB
FCD
#EE
"

p <- c(all_plots, list(x_axis, y_axis)) %>%
  patchwork::wrap_plots(
    heights = c(20, 20, 1),
    widths = c(1, 25, 25),
    design = design
  ) &
  plotwise_theme

# end wizardry

st_string <- str_wrap(
  paste0(
    "Each subplot shows the proportion of US election candidates with ",
    "a name from a famous pair or couple over time"
  ),
  70
)

p <- p + plot_annotation(
  title = "Famous pairs & couples as US election candidates",
  subtitle = st_string,
  caption = "#TidyTuesday 07/11/2023 | plot: @_jcken | data: US House Election Results",
  theme = theme(
    text = element_text(family = main_font),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, lineheight = 0.4, size = 16),
    plot.caption = element_text(hjust = 0.5),
    plot.background = element_rect(colour = "white")
  )
)

# save ----

ggsave(
  plot = p,
  filename = here::here("2023/2023-11-07/2023-11-07.png"),
  height = 1200, width = 1000, units = "px"
)

