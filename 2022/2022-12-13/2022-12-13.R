# libs and data ----
library(tidyverse)
library(lubridate)
library(gridExtra)
library(grid)
library(showtext)
library(RColorBrewer)

state_retail <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/state_retail.csv',  col_types = "cciciiccc")
coverage_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/coverage_codes.csv')

# functions ----
arima_one_step_mean <- function(input_data, pred_date) {
  # pred_date is the date at which I want to predict what would happen
  #  under an AR(1) model
  ts_data <- input_data %>%
    filter(date < pred_date)
  ar1_pred <- forecast::Arima(ts_data$change_yoy, order= c(1, 0, 0), method = "ML") %>%
    forecast::forecast(h=1)
  ar1_pred$mean[1]
}

arima_one_step_se <- function(input_data, pred_date) {
  # pred_date is the date at which I want to predict what would happen
  #  under an AR(1) model
  ts_data <- input_data %>%
    filter(date < pred_date)
  ar1_pred <- forecast::Arima(ts_data$change_yoy, order= c(1, 0, 0), method = "ML") %>%
    forecast::forecast(h=1)
  as.numeric( (ar1_pred$upper[,1] - ar1_pred$lower[,1]) / (2 * qnorm(.5 + ar1_pred$level[1] / 200)) )
}


# fonts ----
font_add_google(name = "raleway",
                family = "Raleway")
main_font <- "Raleway"
showtext_auto()
# wrangling ----

## I will fit an AR(1) model to the data; pacf(plot_data$change_yoy)
## suggests dependence holds for only 1 lag

plot_data <- state_retail %>%
  filter(state_abbr == "USA" & subsector == "total") %>%
  mutate(date = paste0("01/", month, "/", year),
         date = dmy(date),
         change_yoy = as.numeric(change_yoy),
         change_yoy_se = as.numeric(change_yoy_se)) %>%
  mutate(lwr = change_yoy - 3 * change_yoy_se,
         upr = change_yoy + 3 * change_yoy_se) %>%
  select(change_yoy, change_yoy_se, date, lwr, upr)



predictions <- tibble(mean = rep(NA, nrow(plot_data) - 12), std_err = rep(NA, nrow(plot_data) - 12), date = plot_data$date[-(1:12)])

predictions <- predictions %>%
  mutate(mean = unlist(map(date, ~ arima_one_step_mean(plot_data, .x))),
         std_err = unlist(map(date, ~ arima_one_step_se(plot_data, .x))),
         lwr_pred = mean - 3 * std_err,
         upr_pred = mean + 3 * std_err) %>%
  left_join(plot_data, by = "date") %>%
  mutate(abs_implausibility = abs(mean - change_yoy) / sqrt(std_err ^ 2 + change_yoy_se ^ 2),
         is_plausible = abs_implausibility  <  3)

plausible_pred <- predictions %>% filter(is_plausible)
implausible_pred <- predictions %>% filter(!is_plausible)

# plot ----

main_col <- brewer.pal(3, "Dark2")[1]
second_col <- brewer.pal(3, "Dark2")[2]
date_range <- c(as.Date("2015-01-01"), last(all_dates))

arrow_data <- data.frame(x1 = as.Date("2019-06-01"),  x2 = as.Date("2019-06-01"),
                         y1 = 15, y2 = 3)

arrow_model <- data.frame(x1 = as.Date("2022-01-01"),  x2 = as.Date("2022-01-01"),
                         y1 = 55, y2 = 43)

data_plot <- ggplot() + 
  geom_line(data = predictions, mapping = aes(x = date, y = mean),
            color = second_col) +
  geom_ribbon(data = predictions, aes(x = date, ymin = lwr_pred, ymax = upr_pred),
              alpha = 0.3, fill = second_col) +
  geom_line(data = plot_data, mapping = aes(x = date, y = change_yoy),
            colour = main_col) +
  geom_ribbon(data = plot_data, aes(x = date, ymin = lwr, ymax = upr),
              alpha = 0.3, fill = main_col) +
  geom_point(data = implausible_pred, aes(x = date, y = change_yoy),
             pch = 19, size = 4, colour = main_col) +
  labs(y = "Year on year\npercentage change",
       x = "Date") +
  ggtitle("These Unpresendented Times",
          subtitle = "Year on year percentage change for total USA monthly retail sales") +
  annotate("text", x = as.Date("2019-06-01"), y = 30,
           label = "Observed data +/- 3 std. errs.\nNo predictions are made for\nthe first year of data",
           colour = main_col, family = main_font, size = 8) +
  annotate("text", x = as.Date("2022-01-01"), y = 65,
           label = "AR(1) model\n mean +/- 3 std. errs.",
           colour = second_col, family = main_font, size = 8) +
  geom_curve(mapping = aes(x = x1, y = y1, xend = x2, yend = y2),
             data = arrow_data,
             arrow = arrow(type = "open", length = unit(0.05, "npc")),
             colour = main_col, curvature = 0.2) +
  geom_curve(mapping = aes(x = x1, y = y1, xend = x2, yend = y2),
             data = arrow_model,
             arrow = arrow(type = "open", length = unit(0.05, "npc")),
             colour = second_col, curvature = -0.2) +
  theme_minimal() +
  theme(text = element_text(family = main_font, size = 28, colour = main_col),
        plot.title = element_text(hjust = 0.5, size = 40),
        plot.subtitle = element_text(hjust = 0.5, size = 28),
        panel.grid = element_blank(),
        axis.text = element_text(colour = main_col))
data_plot
side_text <- "Pukelsheim's 3 sigma rule states a continuous, unimodal random variable is further than 3 standard\ndeviations away from its mean with probability <0.05.\n\nObserved data which are sufficiently far from the AR(1) mean prediciton are highlighted with a bullet.\n\n#TidyTuesday 13/12/2022 | Data: US Census Bureau | @_jcken"

additional_text <- ggplot() +
  annotate("text", x = as.Date("2018-01-01"), y = 30, 
           label = side_text, family = main_font, colour = main_col,
           size = 8) +
  theme_void()

p <- arrangeGrob(data_plot, additional_text,
            ncol = 1,
            heights = c(2, 1)) 

grid.arrange(p)
# save ----

png("~/tidytuesday/2022/2022-12-13/2022-12-13.png",
    width = 1600,  height = 900, units = "px")
grid.arrange(p)
dev.off()


# REMEMBER FOR FUTURE
# ggsave() doesn't play nicely with arrangeGrob? or showtext_auto and arrangeGrob?
# see https://stackoverflow.com/a/32065806/13256171
