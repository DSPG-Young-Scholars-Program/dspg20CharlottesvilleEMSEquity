library(lubridate)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(sf)

options(mc.cores = 3)

response_time_data <- vroom::vroom(here::here("data", "working", "first_unit_at_scene_data.csv")) %>%   # replace with reading in first_unit_at_scene.csv once rivanna fixed
  ungroup() # just in case its still grouped

neighborhoods <- read_sf(here::here("data", "original", "neighborhoods", "planning_area_06_04_2020.shp")) %>%
  st_transform(crs = 4326)




covid_start_date <- ymd("2020-03-01")


# this is taking a really long time and I don't know why. Something to do with dates.
prepared_data <- response_time_data %>%
  mutate(response_time_hundreths_of_minutes = as.integer(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes * 100)) %>%  # make times integers for ease of modeling
  mutate(after_covid = incident_date >= ymd("2020-03-01")) %>%  # calling after covid anytime after March 1st
  mutate(time_of_day = hour(incident_psap_call_date_time) + minute(incident_psap_call_date_time) / 60 + second(incident_psap_call_date_time) / 3600) %>%
  mutate(patient_first_race = gsub("(.*?)\\|.*", "\\1", patient_race_list))


prepared_data_sp <- prepared_data %>%
  filter(!is.na(scene_gps_longitude), !is.na(scene_gps_latitude)) %>%
  st_as_sf(coords = c("scene_gps_longitude", "scene_gps_latitude"),
           remove = FALSE,
           crs = 4326)

prepared_data_neighborhoods <- neighborhoods %>%
  st_join(prepared_data_sp, join = st_contains)

set.seed(451)

basic_linear_model <- prepared_data %>%
  select(response_time_hundreths_of_minutes,
         patient_age,
         patient_first_race,
         patient_gender,
         response_vehicle_type,
         after_covid) %>%
  stan_glm(response_time_hundreths_of_minutes ~ after_covid*(patient_age + patient_first_race +patient_gender + response_vehicle_type),
           data = .,
           family = "poisson",
           chains = 3, iter = 2000,
           sparse = TRUE)

mcmc_trace(basic_linear_model)
mcmc_dens_overlay(basic_linear_model)


