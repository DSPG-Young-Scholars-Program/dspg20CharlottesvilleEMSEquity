library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(leaflet)

# run renaming_columns.R first
ems <- charlottesville_renamed

markers <- ems %>% select(`scene_gps_latitude`, `scene_gps_longitude`)
leaflet(ems) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~`scene_gps_longitude`,
    lat = ~`scene_gps_latitude`,
    radius = 3,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.2
  )
