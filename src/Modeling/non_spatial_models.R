library(lubridate)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(sf)
library(splines)
library(stringr)



options(mc.cores = 10)

response_time_data <- vroom::vroom(here::here("data", "working", "first_unit_at_scene_impressions_categorized.csv")) %>%   # replace with reading in first_unit_at_scene.csv once rivanna fixed
  ungroup() # just in case its still grouped

neighborhoods <- read_sf(here::here("data", "original", "neighborhoods", "planning_area_06_04_2020.shp")) %>%
  st_transform(crs = 4326)




covid_start_date <- ymd("2020-03-17")
degree <- 5
knots <- 5
df <- knots + degree

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
  left_join(possible_impression_lookup_table, by = "possible_impression_category")





prepared_data_sp <- prepared_data %>%
  filter(!is.na(scene_gps_longitude), !is.na(scene_gps_latitude)) %>%
  st_as_sf(coords = c("scene_gps_longitude", "scene_gps_latitude"),
           remove = FALSE,
           crs = 4326)

prepared_data_neighborhoods <- neighborhoods %>%
  st_join(prepared_data_sp, join = st_contains)

set.seed(451)



basic_model_bayes_no_interact <- prepared_data %>%
  select(response_time_hundreths_of_minutes,
         patient_age,
         patient_gender,
         response_vehicle_type_collapsed,
         after_covid,
         impression_category,
         possible_impression_category_collapsed,
         patient_first_race_collapsed,
         time_of_day) %>%
  stan_glm(response_time_hundreths_of_minutes ~ after_covid + (patient_age +
                                                                 patient_first_race_collapsed +
                                                                 patient_gender +
                                                                 possible_impression_category_collapsed +
                                                                 response_vehicle_type_collapsed +
                                                                 time_of_day),# + response_vehicle_type_collapsed + possible_impression_category + time_of_day),
           data = .,
           family = "poisson",
           chains = 10, iter = 2000,
           sparse = FALSE,
           open_progress = TRUE,
           verbose = TRUE,
           QR = TRUE)

basic_model_bayes_yes_interact <- prepared_data %>%
  select(response_time_hundreths_of_minutes,
         patient_age,
         patient_gender,
         response_vehicle_type_collapsed,
         after_covid,
         impression_category,
         possible_impression_category_collapsed,
         patient_first_race_collapsed,
         time_of_day) %>%
  stan_glm(response_time_hundreths_of_minutes ~ after_covid * (patient_age +
                                                            patient_first_race_collapsed +
                                                            patient_gender +
                                                            possible_impression_category_collapsed +
                                                            response_vehicle_type_collapsed +
                                                            time_of_day),# + response_vehicle_type_collapsed + possible_impression_category + time_of_day),
      data = .,
      family = "poisson",
      chains = 10, iter = 2000,
      sparse = FALSE,
      open_progress = TRUE,
      verbose = TRUE,
      QR = TRUE)


basic_model_freq_yes_interact <- prepared_data %>%
  select(response_time_hundreths_of_minutes,
         patient_age,
         patient_gender,
         response_vehicle_type_collapsed,
         after_covid,
         impression_category,
         possible_impression_category_collapsed,
         patient_first_race_collapsed,
         time_of_day) %>%
  glm(response_time_hundreths_of_minutes ~ after_covid * (patient_age +
                                                          patient_first_race_collapsed +
                                                          patient_gender +
                                                          possible_impression_category_collapsed +
                                                          response_vehicle_type_collapsed +
                                                          time_of_day),# + response_vehicle_type_collapsed + possible_impression_category + time_of_day),
      data = .,
      family = "poisson")

save(basic_model_bayes_no_interact, file = here::here("src", "Modeling", "model_objects", "basic_model_bayes_no_interact.RData"))
load(here::here("src", "Modeling", "model_objects", "glm_full_no_interaction.RData"))


basic_model_bayes_no_interact <- basic_linear_model



#############################################################
# Check for spatial autocorrelation in residuals
#############################################################

model_res <- residuals(basic_linear_model)

modeled_data <- prepared_data %>%
  filter(across(c(response_time_hundreths_of_minutes,
                patient_age,
                patient_gender,
                response_vehicle_type_collapsed,
                after_covid,
                impression_category,
                possible_impression_category_collapsed,
                patient_first_race_collapsed,
                time_of_day), ~!is.na(.x))) %>%
  mutate(residuals = model_res) %>%
  filter(!is.na(scene_gps_latitude)) %>%
  sample_frac(0.20)


incident_distances <- as.matrix(dist(cbind(modeled_data$scene_gps_latitude, modeled_data$scene_gps_longitude)))
incident_distances <- 1/incident_distances
diag(incident_distances) <- 0
incident_distances[is.infinite(incident_distances)] <- 0



ape::Moran.I(modeled_data$residuals, incident_distances)


## There is no global spatial autocorrelation.
