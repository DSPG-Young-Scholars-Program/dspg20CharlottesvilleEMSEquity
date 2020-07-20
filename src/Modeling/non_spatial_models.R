library(lubridate)
library(rstanarm)
library(bayesplot)
library(dplyr)

response_time_data <- vroom::vroom(here::here("data", "working", "first_unit_at_scene_data.csv")) %>%   # replace with reading in first_unit_at_scene.csv once rivanna fixed
  ungroup() # just in case its still grouped
covid_start_date <- ymd("2020-03-01")


# this is taking a really long time and I don't know why. Something to do with dates.
prepared_data <- response_time_data %>%
  mutate(response_time_hundreths_of_minutes = as.integer(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes * 100)) %>%  # make times integers for ease of modeling
  mutate(after_covid = incident_date >= ymd("2020-03-01")) %>%  # calling after covid anytime after March 1st
  mutate(time_of_day = hour(incident_psap_call_date_time) + minute(incident_psap_call_date_time) / 60 + second(incident_psap_call_date_time) / 3600) %>%
  mutate(patient_first_race = gsub("(.*?)\\|.*", "\\1", patient_race_list))

set.seed(451)

basic_linear_model <- prepared_data %>%
  select(response_time_hundreths_of_minutes,
         patient_age,
         patient_first_race,
         patient_gender,
         response_vehicle_type) %>%
  stan_glm(response_time_hundreths_of_minutes ~ patient_age + patient_first_race +patient_gender + response_vehicle_type,
           data = .,
           family = "poisson",
           chains = 4, iter = 1000,
           sparse = TRUE)

mcmc_trace(basic_linear_model)
mcmc_dens_overlay(basic_linear_model)
