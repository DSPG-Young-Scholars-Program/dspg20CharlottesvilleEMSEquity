library(lubridate)
library(dplyr)
library(stringr)
library(sf)
library(tigris)

response_time_data <- vroom::vroom(here::here("data", "working", "first_unit_at_scene_impressions_categorized.csv")) %>%   # replace with reading in first_unit_at_scene.csv once rivanna fixed
  ungroup() # just in case its still grouped

neighborhoods <- read_sf(here::here("data", "original", "neighborhoods", "planning_area_06_04_2020.shp")) %>%
  st_transform(crs = 4326)

census_tracts <- tracts(state = "VA",
                        county = "Albemarle",
                        cb = TRUE,
                        year = 2018,
                        class = "sf") %>%
  st_transform(crs = 4326)

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

prepared_data <- response_time_data %>%
  filter(!(is.na(patient_age) & is.na(patient_gender) & is.na(patient_race_list))) %>%
  mutate(response_time_hundreths_of_minutes = as.integer(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes * 100)) %>%  # make times integers for ease of modeling
  mutate(after_covid = incident_date >= covid_start_date) %>%  # calling after covid anytime after March 1st
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
         incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes) %>%
  mutate(across(everything(), ~ifelse(is.na(.x) & !is.numeric(.x),
                                      "missing",
                                      .x))) %>% # for categorical variables simply add missing as a category
  na.omit() %>%  # removing because these will be implicitly thrown out by models %>%
  filter(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes != 0) %>%
  mutate(log_trans_dispatch_time = log(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes)) %>%
  filter(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes < 25)

neighborhoods_small <- neighborhoods %>%
  select(NAME) %>%
  mutate(GEOID = NA)

census_tracts_small <- census_tracts %>%
  select(NAME, GEOID)

spatial_regions <- bind_rows(neighborhoods_small,
                             census_tracts_small)

prepared_data_sp <- prepared_data %>%
  st_as_sf(coords = c("scene_gps_longitude", "scene_gps_latitude"),
           crs = 4326,
           remove = FALSE)

prepared_data_regions <- st_join(spatial_regions,
                                 prepared_data_sp,
                                 join = st_contains) # with this we lose ~ 600 observations that were outside either region


readr::write_csv(prepared_data, here::here("data", "final", "response_time_model_data_prepared.csv"))
st_write(prepared_data_regions, here::here("data", "final", "response_time_model_data_prepared_sp.geojson"))
