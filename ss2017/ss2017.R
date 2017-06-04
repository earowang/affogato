## ---- load
library(tidyverse)
library(plotly)
library(leaflet)
library(lubridate)
# Read the data files in 
pedestrian <- read_rds("ped_counts_oct16.rds")
locations <- read_rds("ped_loc_oct16.rds")

## ---- leaflet
melb_map <- locations %>% 
  leaflet() %>% 
  setView(lng = mean(locations$Longitude), 
          lat = mean(locations$Latitude),
          zoom = 14) %>% 
  addTiles() %>%
  addMarkers( ~ Longitude, ~ Latitude, popup = ~ Sensor_Name) 
melb_map 

## ---- ped-print
pedestrian <- pedestrian %>% 
  filter(Date_Time >= as_date("2016-02-01"),
         Date_Time <= as_date("2016-02-29"))
pedestrian

## ---- line
stations <- pedestrian %>% 
  filter(Sensor_ID %in% c(3, 6, 9, 13)) %>% 
  mutate(Hour = hour(Date_Time),
         Weekday = wday(Date, label = TRUE),
         Working = if_else(Weekday %in% c("Sun", "Sat"), "N", "Y"),
         Working = factor(Working, levels = c("N", "Y")))
wk_levels <- levels(stations$Weekday)[c(2:7, 1)]
stations <- stations %>% 
  mutate(Weekday = factor(Weekday, levels = wk_levels))
# 4 major stations
stations_levels <- c("Melbourne Central", 
                     "Flagstaff Station",
                     "Southern Cross Station", 
                     "Flinders Street Station Underpass")
stations <- stations %>% 
  mutate(Sensor_Name = factor(Sensor_Name, levels = stations_levels),
         Hour = Hour)

gg_ts <- stations %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts, colour = Sensor_Name)) +
  geom_line() +
  facet_grid(Sensor_Name ~ ., scales = "free_y") +
  xlab("Date Time") +
  ylab("Hourly Counts") +
  theme(legend.position = "none")
ggplotly(gg_ts)

## ---- facet
gg_line <- stations %>% 
  ggplot(aes(x = Hour, y = Hourly_Counts, colour = Sensor_Name, group = Date)) +
  geom_line() +
  ylab("Hourly Counts") +
  facet_grid(Sensor_Name ~ Weekday, scales = "free_y") +
  theme(legend.position = "none")
ggplotly(gg_line)
