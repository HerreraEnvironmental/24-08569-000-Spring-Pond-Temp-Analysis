library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

roll_consecutive <- function(date, value, fun, align = "right") {
  stopifnot(length(date) == length(value))
  n <- length(value)
  if (n < 7L) {
    return(rep(NA_real_, n))
  }

  zoo::rollapply(
    seq_len(n),
    width = 7L,
    align = align,
    fill = NA_real_,
    FUN = function(i) {
      dates_i <- as.Date(date[i])
      values_i <- value[i]
      complete_dates <- length(unique(dates_i)) == 7L &&
        as.integer(max(dates_i) - min(dates_i)) == 6L
      enough_values <- sum(!is.na(values_i)) >= min_valid_days_7day
      if (!complete_dates || !enough_values) {
        return(NA_real_)
      }
      fun(values_i, na.rm = TRUE)
    }
  )
}

min_valid_days_7day <- 7L

temp_data <- bind_rows(
  read_csv(
    'inputs\\Water_Temp.SPING_NORTH@Spring_Pond.EntireRecord.csv',
    skip = 14
  ) %>%
    mutate(Locator = 'Spring-North'),
  read_csv(
    'inputs\\Water_Temp.SPRING_DEEP@Spring_Pond.EntireRecord.csv',
    skip = 14
  ) %>%
    mutate(Locator = 'Spring-Deep'),
  read_csv(
    'inputs\\Water_Temp.SPRING_OUT@Spring_Pond.EntireRecord.csv',
    skip = 14
  ) %>%
    mutate(Locator = 'Spring-Out')
)


daily_summary <- temp_data %>%
  group_by(Locator, Date = as_date(`Timestamp (UTC-08:00)`)) %>%
  summarise(n = n(), DayCoverage = n / (24 * 60 / 5), MaxT = max(Value))


# Temperature: seven-day mean of daily maximum temperature -----
temp_7day <- daily_summary %>%
  filter(DayCoverage > 0.8) %>% #require 80% coverage
  dplyr::arrange(Date, .by_group = TRUE) |>
  dplyr::mutate(
    Temp7DayMean = roll_consecutive(Date, MaxT, mean, align = "right")
  ) |>
  dplyr::ungroup()

temp_7day %>%
  ggplot(aes(x = Date, y = Temp7DayMean, col = Locator)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 16, lty = 2, col = 'red') +
  scale_y_continuous('7-day Average of Daily Maximum Temperature (deg C)') +
  scale_x_date('', date_labels = '%B', limits = as.Date(c('2026-06-01', NA))) +
  theme(legend.position = 'bottom')

ggsave('outputs/sevDADmax.png', width = 6, height = 4, scale = 1.2)


temp_7day %>%
  select(-n, -DayCoverage, -Year, -Month) %>%
  group_by(Date) %>%
  tidyr::pivot_wider(
    names_from = Locator,
    values_from = c(MaxT, Temp7DayMean)
  ) %>%
  arrange(Date) %>%
  write_csv('outputs/sevDADmax.csv')
