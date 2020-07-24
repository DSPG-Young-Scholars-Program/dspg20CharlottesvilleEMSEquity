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

response_time_data <- vroom::vroom(here::here("data", "working", "first_unit_at_scene_impressions_categorized.csv")) %>%   # replace with reading in first_unit_at_scene.csv once rivanna fixed
  ungroup() # just in case its still grouped


####################################################################################
# Prepare Data for Modeling
####################################################################################


covid_start_date <- ymd("2020-02-15")


race_lookup_table <- tibble(patient_first_race = c("american indian or alaska native",
                                                   "asian",
                                                   "black or african american",
                                                   "hispanic or latino",
                                                   "native hawaiian or other pacific islander",
                                                   "white",
                                                   NA),
                            patient_first_race_collapsed = c("other",
                                                             "other",
                                                             "black or african american",
                                                             "other",
                                                             "other",
                                                             "white",
                                                             NA))

vehicle_type_lookup_table <- tibble(response_vehicle_type = c("ambulance",
                                                              "fire apparatus",
                                                              "quick response vehicle (non-transport vehicle other than fire apparatus)",
                                                              "other",
                                                              "personal vehicle",
                                                              "crash truck or other specialty vehicle",
                                                              NA),
                                    response_vehicle_type_collapsed = c("ambulance",
                                                                        "fire apparatus",
                                                                        "other",
                                                                        "other",
                                                                        "other",
                                                                        "other",
                                                                        NA))


possible_impression_lookup_table <- tribble(~possible_impression_category, ~possible_impression_category_collapsed,
                                            "abuse of substance", "abuse of substance",
                                            "behavioral", "behavioral",
                                            "cv", "cv",
                                            "endocrine", "endocrine",
                                            "environment", "other",
                                            "gi/gu", "gi/gu",
                                            "infectious", "infectious",
                                            "injury", "injury",
                                            "missing", "missing",
                                            "neuro", "neuro",
                                            "ob", "other",
                                            "other", "other",
                                            "pain", "pain",
                                            "respiratory", "respiratory")

# this is taking a really long time and I don't know why. Something to do with dates.
prepared_data <- response_time_data %>%
  mutate(response_time_hundreths_of_minutes = as.integer(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes * 100)) %>%  # make times integers for ease of modeling
  mutate(after_covid = incident_date >= ymd("2020-03-01")) %>%  # calling after covid anytime after March 1st
  mutate(time_of_day = hour(incident_psap_call_date_time) + minute(incident_psap_call_date_time) / 60 + second(incident_psap_call_date_time) / 3600) %>%
  mutate(patient_first_race = gsub("(.*?)\\|.*", "\\1", patient_race_list)) %>%
  left_join(race_lookup_table, by = "patient_first_race") %>%
  mutate(patient_gender = ifelse(str_detect(patient_gender,"unknown\\.*"),
                                 NA,
                                 patient_gender)) %>%
  left_join(vehicle_type_lookup_table, by = "response_vehicle_type") %>%
  left_join(possible_impression_lookup_table, by = "possible_impression_category") %>%
  select(response_time_hundreths_of_minutes,
         patient_age,
         patient_gender,
         response_vehicle_type_collapsed,
         after_covid,
         impression_category,
         possible_impression_category_collapsed,
         patient_first_race_collapsed,
         time_of_day,
         scene_gps_latitude,
         scene_gps_longitude,
         incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes) %>% # include lat and long for comparison with neighborhood data
  mutate(across(everything(), ~ifelse(is.na(.x) & !is.numeric(.x),
                                      "missing",
                                      .x))) %>% # for categorical variables simply add missing as a category
  na.omit() %>%  # removing because these will be implicitly thrown out by models
  filter(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes != 0) %>%
  mutate(log_trans_dispatch_time = log(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes)) %>%
  filter(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes < 25)

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

save(basic_model_bayes_no_interact, file = here::here("src", "Modeling", "model_objects", "basic_model_bayes_no_interact.RData"))

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

save(basic_model_bayes_yes_interact, file = here::here("src", "Modeling", "model_objects", "basic_model_bayes_yes_interact.RData"))


basic_model_freq_no_interact <- prepared_data %>%
  glm(log_trans_dispatch_time ~ after_covid + (patient_age +
                                                            patient_first_race_collapsed +
                                                            patient_gender +
                                                            possible_impression_category_collapsed +
                                                            response_vehicle_type_collapsed +
                                                            time_of_day),
      data = .,
      family = "gaussian")

save(basic_model_freq_no_interact, file = here::here("src", "Modeling", "model_objects", "basic_model_freq_no_interact.RData"))

basic_model_freq_yes_interact <- prepared_data %>%
  glm(log_trans_dispatch_time ~ after_covid * (patient_age +
                                                          patient_first_race_collapsed +
                                                          patient_gender +
                                                          possible_impression_category_collapsed +
                                                          response_vehicle_type_collapsed +
                                                          time_of_day),
      data = .,
      family = "gaussian")

save(basic_model_freq_yes_interact, file = here::here("src", "Modeling", "model_objects", "basic_model_freq_yes_interact.RData"))



# load(here::here("src", "Modeling", "model_objects", "glm_full_no_interaction.RData"))
#
#
# #############################################################
# # Check for spatial autocorrelation in residuals
# #############################################################

resid_bayes_no <- residuals(basic_model_bayes_no_interact)
resid_bayes_yes <- residuals(basic_model_bayes_yes_interact)
resid_freq_no <- residuals(basic_model_bayes_no_interact)
resid_freq_yes <- residuals(basic_model_bayes_yes_interact)


augemented_data <- prepared_data %>%
  mutate(resid_bayes_no = resid_bayes_no,
         resid_bayes_yes = resid_bayes_yes,
         resid_freq_no = resid_freq_no,
         resid_freq_yes = resid_freq_yes) %>%
  sample_frac(0.01)



incident_distances <- as.matrix(dist(cbind(augemented_data$scene_gps_latitude, augemented_data$scene_gps_longitude)))
incident_distances <- 1/incident_distances
diag(incident_distances) <- 0
incident_distances[is.infinite(incident_distances)] <- 0



ape::Moran.I(augemented_data$resid_bayes_no,(incident_distances))

local_moran <- localmoran(augemented_data$resid_bayes_no,
                          mat2listw(incident_distances),
                          alternative = "two.sided")


tmp <- augemented_data %>%
  mutate(local_moran = local_moran[,1])

tmp %>%
  filter(local_moran < -100000) %>%
  st_as_sf(coords = c("scene_gps_longitude", "scene_gps_latitude")) %>%
  ggplot() +
  geom_sf(aes(color = log(local_moran))) +
  scale_color_gradient2()


augemented_data %>%
  st_as_sf(coords = c("scene_gps_longitude", "scene_gps_latitude")) %>%
  filter(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes < 20) %>%
  ggplot() +
  geom_sf(aes(color = incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes))

tmp %>%
  filter(local_moran > 100000)

ggplot(tibble(local_moran = local_moran[,1])) +
  geom_histogram(aes(x = local_moran), binwidth = 0.05, boundary = 0) +
  lims(x = c(-20, 20))
