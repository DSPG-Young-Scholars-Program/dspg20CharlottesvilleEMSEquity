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


city_border <- tigris::counties(state = "VA",
                                cb = TRUE, year = 2018, class = "sf") %>%
  filter(COUNTYFP == 540) %>%
  st_transform(crs = 4326)

ems_full_sp <- ems_full %>%
  distinct %>%
  filter(!is.na(scene_gps_latitude), !is.na(scene_gps_longitude)) %>%
  st_as_sf(coords = c("scene_gps_longitude", "scene_gps_latitude"), remove = FALSE, crs = 4326)

joined <- st_join(acs_total_pop_tract_sp, ems_full_sp, join = st_contains)


total_days <- as.numeric(range(ems_full_sp$incident_date)[2] - range(ems_full_sp$incident_date)[1])

summed_data <- joined %>%
  group_by(NAME.y, total_population_estimate) %>%
  count() %>%
  mutate(rate_per_1000 = round(n/total_population_estimate * 1000 / total_days *365)) %>%
  ungroup() %>%
  st_as_sf()

range(summed_data$rate_per_1000)
hist(summed_data$rate_per_1000)

BAMMtools::getJenksBreaks(summed_data$rate_per_1000, 5)
quantile(summed_data$rate_per_1000, seq(0, 1, 0.2))
color_scale <- colorBin("BuPu", c(0,1600), c(0, 75, 125, 200, 400, 600))


summed_data %>%
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~color_scale(rate_per_1000),
              label = ~map(glue("{NAME.y}<br/>
                                Incident Rate Per 1000: {rate_per_1000}"), htmltools::HTML)) %>%
  addPolygons(data = city_border,
              color = "#222222", weight = 3, smoothFactor = 0.5,
              fill = NA,
              fillOpacity = 0) %>%
  addLegend("bottomright", pal = color_scale, values = ~rate_per_1000,
            title = "Yearly Incident Rate Per 1000",
            opacity = .8)



period <- ymd("2020-03-01")
pre_period <- joined %>%
  filter(incident_date < period)

post_period <- joined %>%
  filter(incident_date >= period)

days_pre <- as.numeric(range(pre_period$incident_date)[2] - range(pre_period$incident_date)[1])
days_post <- as.numeric(range(post_period$incident_date)[2] - range(post_period$incident_date)[1] )

pre_period_summed <- pre_period %>%
  group_by(NAME.y, total_population_estimate) %>%
  count() %>%
  mutate(rate_per_1000 = (n/total_population_estimate * 1000/days_pre)) %>%
  ungroup() %>%
  st_as_sf()

post_period_summed <- post_period %>%
  group_by(NAME.y, total_population_estimate) %>%
  count() %>%
  mutate(rate_per_1000 = (n/total_population_estimate * 1000/days_post)) %>%
  ungroup() %>%
  st_as_sf()


range(pre_period_summed$rate_per_1000)
range(post_period_summed$rate_per_1000)

hist(pre_period_summed$rate_per_1000)
hist(post_period_summed$rate_per_1000)

BAMMtools::getJenksBreaks(c(pre_period_summed$rate_per_1000, post_period_summed$rate_per_1000), 7)
color_scale <- colorBin("BuPu", c(0, 1.6), c(0, 0.07, .26, .42, .58, .80, 1.0, 1.6))

pre_period_summed %>%
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~color_scale(rate_per_1000),
              label = ~map(glue("{NAME.y}<br/>
                                Incident Rate Per 1000: {rate_per_1000}"), htmltools::HTML),
              group = "Pre 2020") %>%
  addPolygons(data = post_period_summed,
              color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~color_scale(rate_per_1000),
              label = ~map(glue("{NAME.y}<br/>
                                Incident Rate Per 1000: {rate_per_1000}"), htmltools::HTML),
              group = "In 2020") %>%
  addPolygons(data = city_border,
              color = "#222222", weight = 3, smoothFactor = 0.5,
              fillOpacity = 0) %>%
  addLegend("bottomright", pal = color_scale, values = ~rate_per_1000,
            title = "Incident Rate Per 1000",
            opacity = .8) %>%
  addLayersControl(baseGroups = c("Pre 2020", "In 2020"),
                   options = layersControlOptions(collapsed = FALSE))


diff_period_summed <- post_period_summed %>%
  mutate(rate_diff = rate_per_1000 - pre_period_summed$rate_per_1000,
         rate_ratio = 100*(rate_per_1000 / pre_period_summed$rate_per_1000 - 1))


hist(diff_period_summed$rate_diff)
color_scale <- colorBin("BuPu", c(-0.6,0.1), c(-0.6, -0.3, -0.15, -0.10, -0.05, 0.1), reverse = TRUE)


diff_period_summed %>%
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~color_scale(rate_diff),
              label = ~map(glue("{NAME.y}<br/>
                                Change in Incident Rate Per 1000: {round(rate_diff, 2)}"), htmltools::HTML)) %>%
  addPolygons(data = city_border,
              color = "#222222", weight = 3, smoothFactor = 0.5,
              fill = NA) %>%
  addLegend("bottomright", pal = color_scale, values = ~rate_diff,
            title = "Change in Daily Incident Rate Per 1000",
            opacity = .8)


hist(diff_period_summed$rate_ratio)
BAMMtools::getJenksBreaks(diff_period_summed$rate_ratio, 5)
color_scale <- colorBin("PuRd", 100 * (c(0.2,1.2) - 1), 100 * (c(0.2, 0.5, 0.65, 0.8, 1, 1.2) - 1), reverse = TRUE)

diff_period_summed %>%
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~color_scale(rate_ratio),
              label = ~map(glue("{NAME.y}<br/>
                                Change in Incident Rate Per 1000: {round(rate_ratio, 2)}"), htmltools::HTML)) %>%
  addPolygons(data = city_border,
              color = "#222222", weight = 3, smoothFactor = 0.5,
              fill = NA) %>%
  addLegend("bottomright", pal = color_scale, values = ~rate_ratio,
            title = htmltools::HTML("Percent Change in Daily Incident Rate<br>After March 1st 2020"),
            opacity = .8,
            labFormat = labelFormat(suffix = "%", between = " to "))
