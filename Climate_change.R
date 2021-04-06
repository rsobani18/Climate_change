library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  min()

temp_carbon %>%
  filter(!is.na(carbon_emissions) & year %in% c(1751, 2014)) %>%
  select(carbon_emissions)

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  min()

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(temp_anomaly) & year %in% c(1880, 2018)) %>%
  select(year, temp_anomaly)

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line()

p + geom_hline(aes(yintercept = 0), col = "blue")

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle ("Temperature anomaly relative to 20th century mean, 1880 - 2018") +
  geom_text(aes(x= 2000, y = 0.05, label = "20th century mean"), col = "blue")

p <- p + geom_line(aes(year, land_anomaly), col = "red") +
  geom_line(aes(year, ocean_anomaly), col = "blue")
p

#greenhouse gases
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0 - 2000")

#carbon emissions
temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line()

#historic co2
historic_co2 %>%
  ggplot(aes(year, co2)) +
  scale_x_continuous(limits = c(-800000,-775000))+
  geom_line(aes(col=source))
