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
           QR = TRUE,
           prior = normal(0, 2.5),
           prior_intercept = normal(0, 10),
           prior_aux = exponential(1)) # speeds up evaluation

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
           QR = TRUE,
           prior = normal(0, 2.5),
           prior_intercept = normal(0, 10),
           prior_aux = exponential(1))

save(neighbor_model_bayes_yes_interact, file = here::here("data", "working", "model_objects", "neighbor_model_bayes_yes_interact.RData"))

