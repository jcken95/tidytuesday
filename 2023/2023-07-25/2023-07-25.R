# Packages & data ----
library(tidyverse)
library(brms)
library(tidybayes)
library(showtext)
options(mc.cores = 2)
scurvy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')

# Fonts ----

font_add_google(name = "cormorant garamond",
                family = "Cormorant+Garamond")
main_font <- "Cormorant+Garamond"
showtext_auto()

# Wrangling ----

scurvy <- scurvy %>%
  mutate(treatment = treatment, 
         fit_for_duty = if_else(fit_for_duty_d6  == "1_yes", 1, 0),
         .keep = "none")

brm_prior <- c(
  set_prior("normal(0, 1)", class = "b"),
  set_prior("normal(0, 1)", class = "Intercept")
)

fit <- brm(
  # sampling model
  fit_for_duty ~ treatment,
  data = scurvy,
  family = bernoulli(),
  # prior
  prior = brm_prior,
  # mcmc pars
  chains = 4,
  iter = 500000,
  warmup = 1000,
  thin = 100)

is(fit)

plot_data = fit %>%
  tidy_draws() %>%
  mutate(intercept = b_Intercept, .by = ".draw") %>%
  pivot_longer(
    cols = starts_with("b_"),
    names_to = "term",
    values_to = "value") %>%
  group_by(term) %>%
  mutate(term_mean = mean(value)) %>%
  mutate(effect = if_else(
    term == "b_Intercept",
    intercept,
    intercept + value
  ), by = c(".draw")) %>%
  ungroup() %>%
  mutate(term = if_else(
    term == "b_Intercept",
    "cider",
    str_remove(term, "b_treatment")
  ),
  term = str_replace_all(term, "_", " "),
  term = str_to_title(term))
# plot ----

ci_widths <- c(43, 58, 83, 98) / 100
sub_string <- str_wrap("In 1757, James Lind applied six treatments to twelve participants in a randomised controlled trial. The plot shows posterior estimates of logistic regression coefficients, where the covariate is the treatment, and the outcome is whether or not the participant was deemed fit to work after six days of treatment. Posterior estimates indicate treatments are typically ineffective, but there is some evidence to suggest the citrus treatment worked!")
bar_labels = plot_data %>%
  select(term) %>%
  distinct()
midpoint <- mean(plot_data$effect)
eps <- 3
x_range <- range(midpoint - eps, midpoint + eps)



p = plot_data %>% 
  ggplot(aes(y = term, x = effect)) +
  stat_interval(.width = ci_widths) +
  geom_text(aes(x = midpoint, y = term, label = term),
            nudge_y = 0.4, data = bar_labels, family = main_font) +
  xlim(x_range) +
  ylab("") +
  xlab("Posterior estimate of effect size") +
  guides(colour = guide_legend(
    title = "Posterior probability",
    reverse = TRUE,
    keywidth = 0.5,
    override.aes = list(linewidth = 2)
  )) +
  ggtitle("Which Scurvy Treatment is Most Effective?",
          subtitle = sub_string) +
  scale_colour_brewer(palette = "Oranges") +
  theme_minimal() +
  theme(
    text = element_text(family = main_font),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 10, b = 20),
                                 lineheight = 0.5),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 16),
    legend.position = "bottom"
  )
ggsave(
  filename = here::here("2023/2023-07-25/2023-07-25.png"),
  plot = p,
  width = 2, height = 4,
  units = "in",
)

