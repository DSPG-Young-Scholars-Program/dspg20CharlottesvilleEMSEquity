
library(lubridate)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(leaflet.extras)
library(tigris)
library(leaflet.mapboxgl)

## Mapbox API token
token <- Sys.getenv("MAPBOX_TOKEN")
options(mapbox.accessToken = token)

#source(here::here("src", "Profiling", "joining_albemarle_charlottesville.R"))
new_ems_data <- readr::read_csv(here("data", "working", "ems_clean_data.csv"))

#
#
# Kathryn's Code to get Response Regions -----------------------------------------------------------------------------------------------
#
#

rescue_day_polys <- st_read(here("data", "final", "response_regions", "rescue_day_regions.geojson"))
rescue_night_polys <- st_read(here("data", "final", "response_regions", "rescue_night_regions.geojson"))
fire_polys <- st_read(here("data", "final", "response_regions", "fire_regions.geojson"))


#
#
# Incident Point Data Prep ----------------------------------------------------------------------------------------------------------------------------------
#
#

## This can be folded into the cleaning script and later removed
new_ems_data <- new_ems_data %>%
  ## Breaking down time of day info
  mutate(psap_time_of_day = ymd_hms(paste("2020-01-01", strftime(incident_psap_call_date_time, format="%H:%M:%S"), sep = " ")),
         psap_hour = hour(psap_time_of_day),
         psap_minute = minute(psap_time_of_day)) %>%
  ## Categorizing primary impressions manually:
  mutate(primary_impression_category = case_when(str_detect(situation_provider_primary_impression_code_and_description, "lcohol") ~ "Abuse of Alcohol",
                                                 str_detect(situation_provider_primary_impression_code_and_description, "Abuse") ~ "Abuse of Substance",
                                                 str_detect(situation_provider_primary_impression_code_and_description, "Allergic") ~ "Allergic Reaction",
                                                 str_detect(situation_provider_primary_impression_code_and_description, "OB") ~ "OB",
                                                 str_detect(situation_provider_primary_impression_code_and_description, "Endocrine") ~ "Endocrine",
                                                 str_detect(situation_provider_primary_impression_code_and_description, "GI") ~ "GI/GU",
                                                 str_detect(situation_provider_primary_impression_code_and_description, " - ") ~ str_extract(situation_provider_primary_impression_code_and_description, "[^-]+"), 
                                                 !str_detect(situation_provider_primary_impression_code_and_description, " - ") ~ str_extract(situation_provider_primary_impression_code_and_description, "[^\\(]+")),
         primary_impression_category = trimws(primary_impression_category))

#
#
# Generic Incident Point Mapping Function -------------------------------------------------------------------------------------------
#
#

## Function to plot color of incidents for given variable and break it down across another grouping variable:
map_incidents <- function(data,
                          group_var, ## Variable to split into overlay toggle groups
                          scale_var = NULL, ## Variable to color incident points by. Default just plots the points with no color scale
                          palette = NULL, ## Color palette for scale_var. Not needed if scale_var not provided
                          thin_n = 5000, ## Number of rows to sample from original data
                          overlay = TRUE ## Control whether you want checkboxes or radio buttons for overlay. Defauls to checkboxes
) {
  
  ## Thin data by sampling rows
  map_data <- data %>% 
    sample_n(thin_n)
  
  ## Base map
  map <- map_data %>%
    leaflet() %>%
    #addProviderTiles("CartoDB.Positron") %>%
    addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
    addPolygons(data = rescue_day_polys,
                fillOpacity = 0,
                weight = 1,
                opacity = 0.5,
                color = "black",
                group = "Rescue Day") %>%
    addPolygons(data = rescue_night_polys,
                fillOpacity = 0,
                weight = 1,
                opacity = 0.5,
                color = "black",
                group = "Rescue Night") %>%
    addPolygons(data = fire_polys,
                fillOpacity = 0,
                weight = 1,
                opacity = 0.5,
                color = "black",
                group = "Fire")
  
  ## If no scale variable provided, just plot points
  if (length(scale_var) == 0) {
    
    ## Add layer for each agency to allow for toggling
    for (group_level in unique(map_data[[group_var]])) {
      
      filt_data <- map_data[map_data[[group_var]] == group_level,]
      
      map <- map %>%
        addCircleMarkers(data = filt_data,
                         lng = ~scene_gps_longitude,
                         lat = ~scene_gps_latitude,
                         fillColor = "red",
                         fillOpacity = 0.8,
                         opacity = 0,
                         weight = 0,
                         radius = 3,
                         group = group_level)
    }

    ## otherwise plot with color scale for the scale variable
  } else {
    
    ## Add layer for each agency to allow for toggling
    for (group_level in unique(map_data[[group_var]])) {
      
      filt_data <- map_data[map_data[[group_var]] == group_level,]
      
      map <- map %>%
        addCircleMarkers(data = filt_data,
                         lng = ~scene_gps_longitude,
                         lat = ~scene_gps_latitude,
                         fillColor = ~palette(map_data[[scale_var]]),
                         fillOpacity = 0.8,
                         opacity = 0,
                         weight = 0,
                         radius = 3,
                         label = ~map_data[[scale_var]],
                         group = group_level)
    }
  }
  
  if (overlay == TRUE) {
    ## Add layers control for toggling
    map <- map %>% 
      addLayersControl(
        overlayGroups = unique(map_data[[group_var]]),
        baseGroups = c("Rescue Day", "Rescue Night", "Fire"),
        options = layersControlOptions(collapsed = TRUE)
      )
  } else {
    ## Add layers control for toggling
    map <- map %>% 
      addLayersControl(
        baseGroups = unique(map_data[[group_var]]),
        baseGroups = c("Rescue Day", "Rescue Night", "Fire"),
        options = layersControlOptions(collapsed = TRUE)
      )
  }
  
  if (length(scale_var) != 0) {
    map <- map %>% 
      addLegend(
        position = "bottomright",
        pal = palette,
        values = map_data[[scale_var]]
      )
  }
  
  map
  
}


#
#
# Response Time Map by Agency ------------------------------------------------------------------------------------------------------------------
#
#

## Filter incident data to exclude outliers that were disrupting the color scale
pal_data <- incident_points %>% filter(total_unit_response_time < 50)

## Color palette for response times
resp_time_pal <- colorBin("Reds", domain = range(pal_data$total_unit_response_time, na.rm=TRUE), bins = 8)

## Map response times by agency
map_incidents(pal_data, scale_var = "total_unit_response_time", group_var = "agency_name", palette = resp_time_pal, thin_n = 1000)


#
#
# Age Distribution ------------------------------------------------------------------------------------------------------------------
#
#

## Map of incident points (thinned by sampling)
map_data <- incident_points %>% filter(!is.na(patient_age), patient_age < 100)

age_pal <- colorBin("Purples", domain = range(map_data$patient_age, na.rm=TRUE), bins = 8)

map_incidents(map_data, group_var = "agency_name", scale_var = "patient_age", palette = age_pal, thin_n = 1000)

#
#
# Map by Call Type ------------------------------------------------------------------------------------------------------------------
#
#

## Would prefer this to be a heatmap displaying the relative rate of these incidents to some baseline.

impressions <- incident_points %>% filter(!is.na(primary_impression_category))

map_incidents(impressions, group_var = "primary_impression_category", thin_n = 10000, overlay = FALSE)

#
#
# Time of Day Maps ------------------------------------------------------------------------------------------------------------------------------------
#
#

## Discret-ize the time of day scale for toggling
tmp <- incident_points %>%
  filter(!is.na(psap_time_of_day)) %>%
  mutate(psap_time_of_day_discrete = case_when(psap_time_of_day < "2020-01-01 06:00:00 UTC" ~ "Early Morning",
                                               psap_time_of_day < "2020-01-01 12:00:00 UTC" ~ "Morning",
                                               psap_time_of_day < "2020-01-01 18:00:00 UTC" ~ "Afternoon",
                                               psap_time_of_day < "2020-01-01 24:00:00 UTC" ~ "Evening"),
         psap_time_of_day_discrete = as.factor(psap_time_of_day_discrete)) %>%
  filter(total_unit_response_time < 50)

## Map by time of day
map <- map_incidents(tmp, "total_unit_response_time", "psap_time_of_day_discrete", resp_time_pal, 5000, overlay = FALSE)

## Change basemap for nighttime layers for fun (maybe should change later if we actually use something like this)
map %>%
  addProviderTiles("CartoDB.DarkMatter", group = as.factor("Early Morning")) %>%
  addProviderTiles("CartoDB.DarkMatter", group = as.factor("Evening"))

