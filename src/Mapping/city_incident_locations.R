library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(leaflet)
library(ggmap)
library(RColorBrewer)


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

# create heatmap
# select location data from ems
coords.data <- ems %>% select(scene_gps_latitude, scene_gps_longitude)
# define bounds of cville/alb
map_bounds <- c(left = -78.9, bottom = 37.67, right = -78.14, top = 38.34)

coords.map <- get_stamenmap(map_bounds, zoom = 15, maptype = "toner-lite")
coords.map <- ggmap(coords.map, extent = "device", legend = "none")
coords.map <- coords.map +
  stat_density2d(data = coords.data,
                 aes(x = scene_gps_longitude, y = scene_gps_latitude, fill=..level.., alpha=..level..),
                 geom="polygon")
coords.map <- coords.map + scale_fill_gradientn(colors=rev(brewer.pal(7, "Spectral")))
coords.map

