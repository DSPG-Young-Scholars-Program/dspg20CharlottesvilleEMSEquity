####################################################################################
# Script to do all of our non spatial modeling work
####################################################################################



####################################################################################
# Load Data, Libraries, and Set Options
####################################################################################

library(lubridate)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(stringr)
library(spdep)



options(mc.cores = 10)

prepared_data <- sf::st_read(here::here("data", "final", "response_time_model_data_prepared_sp.geojson")) %>%
  sf::st_drop_geometry()

####################################################################################
# Begin Modeling
####################################################################################

set.seed(451)

basic_model_bayes_no_interact <- prepared_data %>%
  stan_glm(log_trans_dispatch_time ~ after_covid + (patient_age +
                                                                 patient_first_race_collapsed +
                                                                 patient_gender +
                                                                 possible_impression_category_collapsed +
                                                                 response_vehicle_type_collapsed +
                                                                 time_of_day),
           data = .,
           family = "gaussian",
           chains = 10, iter = 2000,
           sparse = FALSE,
           open_progress = TRUE,
           verbose = TRUE,
           QR = TRUE,
           prior = normal(0, 2.5), # note that these priors will be transformed before use. To access final priors use prior_summary(my_model)
           prior_intercept = normal(0, 10),
           prior_aux = exponential(1)) # speeds up evaluation

save(basic_model_bayes_no_interact, file = here::here("data", "working", "model_objects", "basic_model_bayes_no_interact.RData"))

basic_model_bayes_yes_interact <- prepared_data %>%
  stan_glm(log_trans_dispatch_time ~ after_covid * (patient_age +
                                                               patient_first_race_collapsed +
                                                               patient_gender +
                                                               possible_impression_category_collapsed +
                                                               response_vehicle_type_collapsed +
                                                               time_of_day),
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

save(basic_model_bayes_yes_interact, file = here::here("data", "working", "model_objects", "basic_model_bayes_yes_interact.RData"))


basic_model_freq_no_interact <- prepared_data %>%
  glm(log_trans_dispatch_time ~ after_covid + (patient_age +
                                                            patient_first_race_collapsed +
                                                            patient_gender +
                                                            possible_impression_category_collapsed +
                                                            response_vehicle_type_collapsed +
                                                            time_of_day),
      data = .,
      family = "gaussian")

save(basic_model_freq_no_interact, file = here::here("data", "working", "model_objects", "basic_model_freq_no_interact.RData"))

basic_model_freq_yes_interact <- prepared_data %>%
  glm(log_trans_dispatch_time ~ after_covid * (patient_age +
                                                          patient_first_race_collapsed +
                                                          patient_gender +
                                                          possible_impression_category_collapsed +
                                                          response_vehicle_type_collapsed +
                                                          time_of_day),
      data = .,
      family = "gaussian")

save(basic_model_freq_yes_interact, file = here::here("data", "working", "model_objects", "basic_model_freq_yes_interact.RData"))

