# packages & data ----
library(tidyverse)
library(corrr)
library(ggtext)
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')


# functions & clrs ---

clrs <- PrettyCols::prettycols("PurpleGreens")
clrs <- c(clrs[4], clrs[6])
bg_clr <- "grey40"

mytheme <- theme(plot.background = element_rect(fill = bg_clr, colour = bg_clr),
                 legend.position = "none") 
void_ts_plot <- function(input_data, stock, line_colour) {
  input_data %>%
    filter(stock_symbol == stock) %>%
    ggplot(aes(x = date, y = close)) +
    geom_line(colour = line_colour) +
    theme_void() +
    ggtitle(stock) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

is_ibm_clr <- function(stock, clrs) {
  if(stock == "IBM") {
    return(clrs[1])
  } else{
    return(clrs[2])
  }
}
 
# wrangling ----

plot_data <- big_tech_stock_prices %>%
  select(stock_symbol, date, close) %>%
  mutate(close = log(close))

correlations <- plot_data %>%
  pivot_wider(names_from = stock_symbol, values_from = close) %>%
  select(-date) %>%
  corrr::correlate()

# plot ----

subtitle <- glue::glue(
  "<b>On the log scale, the log closing price for 13 out of 14 big tech companies<br>are
  <span style = 'color:{clrs[2]};'>positively correlated</span> with one another,
  the exception is IBM which is<br><span style = 'color:{clrs[1]};'>negatively correlated</span>
  with the other 13 big tech companies.</b>"
)

network_plot <- correlations %>%
  corrr::network_plot(colors = clrs,
                      min_cor = 0) +
  ggtitle(toupper("One of these stocks is not like the others")) +
  annotate(
    'richtext',
    x = -0.25, y = 0.21,
    label = subtitle,
    hjust = 0,
    vjust = 0,
    size = 3,
    fill = bg_clr,
    label.color = bg_clr) +
  mytheme +
  theme(plot.title = element_text(hjust = 0.22, face = "bold"))


stocks <- unique(plot_data$stock_symbol)
ts_plots <- purrr::map(.x = stocks, 
                      ~void_ts_plot(plot_data, .x, is_ibm_clr(.x, clrs)) +
                        mytheme)
ts_plots_all <- patchwork::wrap_plots(ts_plots, ncol = 1) 

p = patchwork::wrap_plots(network_plot, ts_plots_all,
                          widths = c(8, 1)) 
# save ----
ggsave("2023/2023-02-07/2023-02-07.png",
       plot = p,
       width = 2000, height = 2000,
       units = "px")
