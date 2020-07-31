library(dplyr)

unit_level_obs <- vroom::vroom(here::here("data", "working", "ems_clean_data_unit_observation.csv"))
patient_level_obs <- vroom::vroom(here::here("data", "working", "ems_clean_data.csv"))


names(unit_level_obs)

joined_unit_level_obs <- unit_level_obs %>%
  select(response_incident_number,
         incident_date,
         incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes,
         response_ems_unit_call_sign,
         response_vehicle_type) %>%
  full_join(patient_level_obs, by = c("response_incident_number",
                                      "incident_date")) %>%
  as_tibble()


joined_distinct <- joined_unit_level_obs %>%
  as_tibble() %>%
  select(-incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes.y,
         -response_ems_unit_call_sign.y,
         -response_vehicle_type.y) %>%
  rename(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes = incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes.x,
         response_ems_unit_call_sign = response_ems_unit_call_sign.x,
         response_vehicle_type = response_vehicle_type.x) %>%
  distinct()


lowest_response_vehicles <- joined_distinct %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date) %>%
  slice_min(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes,
            n = 1) %>%
  ungroup()

readr::write_csv(lowest_response_vehicles,
                 here::here("data", "working", "first_unit_at_scene_data.csv"))
