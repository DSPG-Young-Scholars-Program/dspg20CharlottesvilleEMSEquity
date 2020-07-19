library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)

source(here::here("src", "Profiling", "joining_albemarle_charlottesville.R"))


neighborhoods <- read_sf(here::here("data", "original", "neighborhoods", "planning_area_06_04_2020.shp")) %>%
  st_transform(crs = 4326)


charlottesville_sp <- charlottesville %>%
  filter(!is.na(scene_gps_latitude), !is.na(scene_gps_longitude)) %>%
  st_as_sf(coords = c("scene_gps_longitude", "scene_gps_latitude"), remove = FALSE, crs = 4326)


joined <- st_join(neighborhoods, charlottesville_sp, join = st_contains)

joined %>%
  group_by(NAME) %>%
  summarize(n = n()) %>%
  ggplot() +
  geom_sf(aes(fill = n)) +
  theme_minimal()



summed_data <- joined %>%
  group_by(NAME) %>%
  summarize(n = n())

range(summed_data$n)

color_scale <- colorBin("BuPu", c(0,7000), c(0, 1000, 2000, 3000, 4000, 7000))

summed_data %>%
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~color_scale(n),
              label = ~NAME) %>%
  addLegend("bottomright", pal = color_scale, values = ~n,
            title = "Number of Rows",
            opacity = .8)
