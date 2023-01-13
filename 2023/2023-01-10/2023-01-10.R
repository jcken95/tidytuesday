# packages & data ----

library(tidyverse)
library(janitor)
library(broom)
library(showtext)

feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

# fonts & colours ----
showtext_auto()
font_add_google("Cormorant")
main_font <- "Cormorant"

neg <- "#EB5353"
pos <- "#36AE7C"
bg <- "grey90"
# wrangling ----

habitat <- site_data %>%
  select(loc_id, "hab_dcid_woods":"hab_marsh") %>%
  drop_na() %>%
  distinct()

model_data <- feederwatch %>%
  clean_names() %>% 
  filter(valid == 1) %>%
  left_join(habitat, by = "loc_id") %>%
  select(obs_id, how_many, loc_id, "hab_dcid_woods":"hab_marsh") %>%
  group_by(obs_id) %>%
  mutate(across("hab_dcid_woods":"hab_marsh", ~ max(.x))) %>% 
  ungroup() %>%
  distinct() %>% 
  select(how_many, hab_dcid_woods:hab_marsh)

model_summary <- lm(how_many ~ ., data = model_data) %>%
  tidy() %>%
  slice(-1) %>%
  mutate(abs_estimate = abs(estimate),
         is_positive = (estimate > 0),
         term = str_sub(term, 5),
         term = str_replace(term, "_", " "))
  
# plot ---- 
x0 <- max(model_summary$abs_estimate)

title <- "Which habitats attract the most birds?"
subtitle <- "Mean effect of habitat type on number of observed birds in descending order"


p <- model_summary %>%
  ggplot() + 
  ggtitle(title,
         subtitle) +
  geom_linerange(aes(y = reorder(term, estimate), 
                     xmin = 0, xmax = estimate,
                     colour = !is_positive), linewidth = 1) +
  scale_color_manual(values = c(pos, neg)) +
  geom_text(
    mapping = aes(x = 0, y = term, label = term),
    hjust = 0.5,
    nudge_y = 0.4,
    size = 4,
    family = main_font) +
  annotate("text", x = 1.25, y = 7,
           label = "#TidyTuesday 10/01/2023 | Data: feederwatch.org | @_jcken",
           family = main_font, angle = 90, color = "grey60") +
  xlab("Number of birds we expect to see above overall mean") +
  ylab("") +
  xlim(-x0, x0) +
  scale_y_discrete(breaks = NULL) +
  theme_minimal() +
  theme(legend.position = "n",
        plot.title = element_text(hjust = 0.5,
                                  family = main_font,
                                  size = 20),
        plot.subtitle = element_text(hjust = 1,
                                     family = main_font,
                                     size = 14),
        text = element_text(family = main_font),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = bg),
        panel.background = element_rect(fill = bg, colour = bg),
        axis.text.x = element_text(size = 16))
# save ----

ggsave("~/tidytuesday/2023/2023-01-10/2023-01-10.png",
       plot = p,
       width = 600,
       height = 800,
       unit = "px")
