library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

source("DRIP_query.R")

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

air_temp <- read.csv("inputs/Hydrology_PUQEW.csv", fileEncoding = "latin1") %>%
  mutate(datetime = parse_datetime(`Collect.Date..local.`, format = "%d-%b-%Y %H:%M"))

daily_summary <- temp_data %>%
  filter(Locator!="26a-KingCounty") %>%
  group_by(Locator, Date = as_date(datetime)) %>%
  summarise(n = n(), DayCoverage = n / (24 * 60 / 5), MaxT = max(Value))

daily_summary_26a <- temp_data %>%
  filter(Locator=="26a-KingCounty") %>%
  group_by(Locator, Date = as_date(datetime)) %>%
  summarise(n = n(), DayCoverage = n / (24 * 60 / 15), MaxT = max(Value))

daily_summary <- rbind(daily_summary, daily_summary_26a)
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
  geom_hline(yintercept = 17.5, lty = 2, col = 'red') +
  annotate("text", x=as.Date("2026-06-01"), y = 16, label = "Core Summer\nSalmonid Habitat", hjust = "left", size = 3) +
  annotate("text", x=as.Date("2026-06-01"), y = 17.5, label = "Salmonid Spawning, Rearing,\nand Migration", hjust = "left" ,size = 3) +
  scale_y_continuous('7-day Average of Daily Maximum Temperature (deg C)') +
  scale_x_date('', date_labels = '%B', limits = as.Date(c('2026-06-01', NA))) +
  scale_color_manual(values = c("SPRING-DEEP"="#04A1FF","SPRING-NORTH"="#3ECDA3","SPRING-OUT"="#FFC42E","26a-KingCounty"= "#5C4E2C")) +
  theme(legend.position = 'bottom')

ggsave('outputs/sevDADmax.png', width = 6, height = 4, scale = 1.2)


temp_7day %>%
  select(-n, -DayCoverage) %>%
  group_by(Date) %>%
  tidyr::pivot_wider(
    names_from = Locator,
    values_from = c(MaxT, Temp7DayMean)
  ) %>%
  arrange(Date) %>%
  write_csv('outputs/sevDADmax.csv')

temp_data %>%
  ggplot(aes(x = datetime, y = Value, col = Locator)) +
  geom_line() +
  geom_line(data = air_temp, aes(x = datetime, y = `Air.Temperature...C.`), color = "black") +
  theme_bw() +
  scale_y_continuous('Temperature') +
  scale_x_date('', date_labels = '%B', limits = as.Date(c('2026-06-01', NA))) +
  scale_color_manual(values = c("SPRING-DEEP"="#04A1FF","SPRING-NORTH"="#3ECDA3","SPRING-OUT"="#FFC42E","26a-KingCounty"= "#5C4E2C")) +
  theme(legend.position = 'bottom')

# percent complete --------------------------------------------------------
completeness <- temp_data %>%
  filter(Locator != "26a-KingCounty") %>%
  group_by(Locator) %>%
  summarize(n = n(),
    start_date = min(datetime),
    end_date = max(datetime),
    total_minutes = as.numeric(difftime(end_date, start_date, units = "mins")),
    n_intervals = total_minutes/5,
    perc_complete = n/n_intervals)

# summary stats ------------------------------------------------------------
daily_summary_stats <- temp_data %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Locator, Date) %>%
  summarize(
    min = min(Value, na.rm=T),
    mean = mean(Value, na.rm=T),
    max = max(Value, na.rm=T)
    ) %>%
  left_join(temp_7day) %>%
  mutate(across(min:max, ~ ifelse(DayCoverage < 0.8, NA, .x))) %>%
  select(-c(n, DayCoverage, MaxT)) %>%
  pivot_wider(names_from = Locator, values_from = c(min, mean, max, Temp7DayMean)) %>%
  select(Date, grep("SPRING-DEEP", colnames(.), value = TRUE), grep("SPRING-NORTH", colnames(.), value = TRUE),
    grep("SPRING-OUT", colnames(.), value = TRUE), grep("26a-KingCounty", colnames(.), value = TRUE)) %>%
  arrange(Date)
# write as csv and paste into formatted excel doc
write_csv(daily_summary_stats, "outputs/daily_summary.csv")
