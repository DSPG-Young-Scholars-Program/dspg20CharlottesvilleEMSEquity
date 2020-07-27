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
           QR = TRUE) # speeds up evaluation

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
      QR = TRUE)

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



# load(here::here("src", "Modeling", "model_objects", "glm_full_no_interaction.RData"))
#
#
# #############################################################
# # Check for spatial autocorrelation in residuals
# #############################################################
#
# resid_bayes_no <- residuals(basic_model_bayes_no_interact)
# resid_bayes_yes <- residuals(basic_model_bayes_yes_interact)
# resid_freq_no <- residuals(basic_model_bayes_no_interact)
# resid_freq_yes <- residuals(basic_model_bayes_yes_interact)
#
#
# augemented_data <- prepared_data %>%
#   mutate(resid_bayes_no = resid_bayes_no,
#          resid_bayes_yes = resid_bayes_yes,
#          resid_freq_no = resid_freq_no,
#          resid_freq_yes = resid_freq_yes) %>%
#   sample_frac(0.01)
#
#
#
# incident_distances <- as.matrix(dist(cbind(augemented_data$scene_gps_latitude, augemented_data$scene_gps_longitude)))
# incident_distances <- 1/incident_distances
# diag(incident_distances) <- 0
# incident_distances[is.infinite(incident_distances)] <- 0
#
#
#
# ape::Moran.I(augemented_data$resid_bayes_no,(incident_distances))
#
# local_moran <- localmoran(augemented_data$resid_bayes_no,
#                           mat2listw(incident_distances),
#                           alternative = "two.sided")
#
#
# tmp <- augemented_data %>%
#   mutate(local_moran = local_moran[,1])
#
# tmp %>%
#   filter(local_moran < -100000) %>%
#   st_as_sf(coords = c("scene_gps_longitude", "scene_gps_latitude")) %>%
#   ggplot() +
#   geom_sf(aes(color = log(local_moran))) +
#   scale_color_gradient2()
#
#
# augemented_data %>%
#   st_as_sf(coords = c("scene_gps_longitude", "scene_gps_latitude")) %>%
#   filter(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes < 20) %>%
#   ggplot() +
#   geom_sf(aes(color = incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes))
#
# tmp %>%
#   filter(local_moran > 100000)
#
# ggplot(tibble(local_moran = local_moran[,1])) +
#   geom_histogram(aes(x = local_moran), binwidth = 0.05, boundary = 0) +
#   lims(x = c(-20, 20))
