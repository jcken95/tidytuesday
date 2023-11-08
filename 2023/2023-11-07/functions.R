#' @param all_names a tibble of first names of each candidate with the year and total
#' must have column names \code{n}, \code{year}, \code{first_name}
all_names_per_year <- function(all_names) {
  names_per_year <- all_names %>%
    group_by(year) %>%
    summarise(total = sum(n))
  names_per_year
}

#' @inheritParams all_names_per_year
#' @param chosen_names character vector of chosen names to investigate
name_proportion_per_year <- function(all_names, chosen_names) {
  if (!is.null(nrow(chosen_names))) {
    chosen_names <- as_vector(chosen_names)
    names(chosen_names) <- NULL
  }

  all_years <- seq(
    from = min(all_names$year),
    to = max(all_names$year),
    by = 2 ## elections every otheryear
  )

  names_per_year <- all_names_per_year(all_names)
  year_name_grid <- expand.grid(
    first_name = chosen_names,
    year = all_years
  )

  candidate_names %>%
    filter(first_name %in% chosen_names) %>%
    right_join(year_name_grid) %>%
    mutate(n = if_else(is.na(n), 0, n)) %>%
    left_join(names_per_year) %>%
    mutate(annual_proportion = n / total, .by = "first_name")
}

#' @inheritParams name_proportions_per_year
plot_fn <- function(chosen_names) {
  line_data <- name_proportion_per_year(candidate_names, chosen_names)
  label_data <- filter(line_data, year == max(year))
  plot_title <- chosen_names %>%
    as_vector() %>%
    str_to_title() %>%
    str_c(collapse = " & ")

  ggplot() +
    geom_line(
      data = line_data,
      mapping = aes(
        x = year,
        y = annual_proportion,
        group = first_name,
        colour = first_name
      )
    ) +
    geom_label_repel(
      data = label_data,
      mapping = aes(
        x = year,
        y = annual_proportion,
        label = str_to_title(first_name),
        colour = first_name,
        segment.colour = "transparent"
      ),
      family = main_font,
      face = "bold",
      label.size = NA,
      fill = NA,
      nudge_x = 9
    ) +
    ggtitle(plot_title) +
    ylab("") +
    xlab("") +
    scale_x_continuous(breaks = seq(from = 1970, to = 2020, by = 10))
}
