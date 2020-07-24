####################################################################################
# Script to do all of our modeling work for heirarchical (mixed effects) models
####################################################################################



####################################################################################
# Load Data, Libraries, and Set Options
####################################################################################

library(lubridate)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(stringr)
library(sf)
library(tigris)


options(mc.cores = 10)

prepared_data_regions <- read_sf(here::here("data", "final", "response_time_model_data_prepared_sp.geojson"))

####################################################################################
# Begin Modeling
####################################################################################

set.seed(451)

neighbor_model_bayes_no_interact <- prepared_data_regions %>%
  stan_glmer(log_trans_dispatch_time ~ after_covid + (patient_age +
                                                               patient_first_race_collapsed +
                                                               patient_gender +
                                                               possible_impression_category_collapsed +
                                                               response_vehicle_type_collapsed +
                                                               time_of_day) +
                                                (1|NAME),
           data = .,
           family = "gaussian",
           chains = 10, iter = 5000,
           sparse = FALSE,
           open_progress = TRUE,
           verbose = TRUE,
           QR = TRUE) # speeds up evaluation

save(neighbor_model_bayes_no_interact, file = here::here("data", "working", "model_objects", "neighbor_model_bayes_no_interact.RData"))

neighbor_model_bayes_yes_interact <- prepared_data_regions %>%
  stan_glmer(log_trans_dispatch_time ~ after_covid * (patient_age +
                                                               patient_first_race_collapsed +
                                                               patient_gender +
                                                               possible_impression_category_collapsed +
                                                               response_vehicle_type_collapsed +
                                                               time_of_day) +
                                                (1|NAME),
           data = .,
           family = "gaussian",
           chains = 10, iter = 5000,
           sparse = FALSE,
           open_progress = TRUE,
           verbose = TRUE,
           QR = TRUE)

save(neighbor_model_bayes_yes_interact, file = here::here("data", "working", "model_objects", "neighbor_model_bayes_yes_interact.RData"))

# load(here::here("src", "Modeling", "model_objects", "glm_full_no_interaction.RData"))


#############################################################
# Check for spatial autocorrelation in residuals
#############################################################
#
# model_res <- residuals(basic_linear_model)
#
# modeled_data <- prepared_data %>%
#   filter(across(c(response_time_hundreths_of_minutes,
#                   patient_age,
#                   patient_gender,
#                   response_vehicle_type_collapsed,
#                   after_covid,
#                   impression_category,
#                   possible_impression_category_collapsed,
#                   patient_first_race_collapsed,
#                   time_of_day), ~!is.na(.x))) %>%
#   mutate(residuals = model_res) %>%
#   filter(!is.na(scene_gps_latitude)) %>%
#   sample_frac(0.20)
#
#
# incident_distances <- as.matrix(dist(cbind(modeled_data$scene_gps_latitude, modeled_data$scene_gps_longitude)))
# incident_distances <- 1/incident_distances
# diag(incident_distances) <- 0
# incident_distances[is.infinite(incident_distances)] <- 0
#
#
#
# ape::Moran.I(modeled_data$residuals, incident_distances)


## There is no global spatial autocorrelation.
