discharge_26a <- read.csv("inputs/26a_historic_discharge_temp.csv", fileEncoding = "latin1")

summer_discharge <- discharge_26a %>% 
  mutate(date = mdy(Collect.Date..local.),
    month = month(date),
    year = as.integer(year(date))) %>%
  filter(month>5, month<10) %>%
  group_by(year) %>%
  summarize(Discharge = mean(Discharge..cfs., na.rm=TRUE),
    Temperature = mean(Water.Temperature...C., na.rm = TRUE)) %>%
  pivot_longer(c(2,3), names_to = "parameter")

summer_discharge %>%
  ggplot(aes(x = year, y = value, col = parameter)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(name = "") +
  scale_y_continuous('June-Sept Mean Discharge (cfs)\nand Mean Temperature (deg C)') +
  scale_color_manual(values = c("Discharge"="#04A1FF","Temperature"="#FFC42E")) +
  theme(legend.position = 'bottom',
    legend.title = element_blank())

ggsave('outputs/discharge_temperature.png', width = 6, height = 4, scale = 1.2)
