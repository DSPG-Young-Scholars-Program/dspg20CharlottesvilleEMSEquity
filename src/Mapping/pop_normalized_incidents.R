library(tidycensus)
library(tidyr)
library(sf)
library(leaflet)
library(glue)
library(lubridate)

source(here::here("src", "Profiling", "joining_albemarle_charlottesville.R"))


acs_total_pop_tract <- get_acs(geography = "tract",
                                   year = 2018,
                                   variables = c(total_population = "DP05_0001"),
                                   state = "VA",
                                   county = c("albemarle", "charlottesville")) %>%
  pivot_wider(names_from = variable,
              values_from = c(estimate, moe),
              names_glue = "{variable}_{.value}")

acs_total_pop_tract_sp <- tigris::tracts(state = "VA", county = c("charlottesville", "albemarle"),
               cb = TRUE, year = 2018, class = "sf") %>%
  st_transform(crs = 4326) %>%
  left_join(acs_total_pop_tract, by = "GEOID")


ems_full_sp <- ems_full %>%
  distinct %>%
  filter(!is.na(scene_gps_latitude), !is.na(scene_gps_longitude)) %>%
  st_as_sf(coords = c("scene_gps_longitude", "scene_gps_latitude"), remove = FALSE, crs = 4326)


joined <- st_join(acs_total_pop_tract_sp, ems_full_sp, join = st_contains)

summed_data <- joined %>%
  group_by(NAME.y, total_population_estimate) %>%
  count() %>%
  mutate(rate_per_1000 = round(n/total_population_estimate * 1000)) %>%
  ungroup() %>%
  st_as_sf()

range(summed_data$rate_per_1000)
hist(summed_data$rate_per_1000)

color_scale <- colorBin("BuPu", c(0,1600), c(0, 200, 400, 700, 1000, 2000))

summed_data %>%
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~color_scale(rate_per_1000),
              label = ~map(glue("{NAME.y}<br/>
                                Incident Rate Per 1000: {rate_per_1000}"), htmltools::HTML)) %>%
  addLegend("bottomright", pal = color_scale, values = ~rate_per_1000,
            title = "Incident Rate Per 1000",
            opacity = .8)
