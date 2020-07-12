
library(lubridate)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(leaflet.extras)
library(tigris)


source(here::here("src", "Profiling", "joining_albemarle_charlottesville.R"))

#
#
# Kathryn's Code to get Response Regions -----------------------------------------------------------------------------------------------
#
#

# ...

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

## Filter out as many double-counted incident addresses as possible:
incident_points <- new_ems_data %>%
  group_by(incident_date, response_incident_number) %>%
  distinct() %>%
  filter(!is.na(response_incident_number)) %>%
  #mutate(across(.cols = c("scene_gps_longitude", "scene_gps_latitude"), .fns = as.numeric)) ## This is slower
  mutate(scene_gps_longitude = as.numeric(scene_gps_longitude),
         scene_gps_latitude = as.numeric(scene_gps_latitude),
         total_unit_response_time = as.numeric(total_unit_response_time),
         patient_age = as.numeric(patient_age)) %>%
  ungroup()

#
#
# Generic Incident Point Mapping Function -------------------------------------------------------------------------------------------
#
#

## Function to plot color of incidents for given variable and break it down across another grouping variable:
map_incidents <- function(data,
                          scale_var, ## Variable to color incident points by
                          group_var, ## Variable to split into overlay toggle groups
                          palette, ## Color palette for scale_var
                          thin_n = 5000 ## Number of rows to sample from original data
) {
  
  ## Thin data by sampling rows
  map_data <- data %>% 
    sample_n(thin_n)
  
  ## Base map
  map <- map_data %>%
    leaflet() %>%
    addProviderTiles("CartoDB.Positron")
  
  ## Add layer for each agency to allow for toggling
  for (group_level in unique(map_data[[group_var]])) {
    
    #print(group_level)
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
  
  ## Add layers control for toggling
  map <- map %>% 
    addLayersControl(
      overlayGroups = unique(map_data[[group_var]]),
      options = layersControlOptions(collapsed = TRUE)
    ) %>% 
    addLegend(
      position = "bottomright",
      pal = palette,
      values = map_data[[scale_var]]
    )
  
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
map_data <- incident_points %>% filter(!is.na(patient_age))

age_pal <- colorBin("Purples", domain = range(map_data$patient_age, na.rm=TRUE), bins = 8)

map_incidents(map_data, scale_var = "patient_age", group_var = "agency_name", palette = age_pal, thin_n = 1000)

#
#
# Response Time Map by Call Type ------------------------------------------------------------------------------------------------------------------
#
#




#
#
# Time of Day Maps ------------------------------------------------------------------------------------------------------------------------------------
#
#








