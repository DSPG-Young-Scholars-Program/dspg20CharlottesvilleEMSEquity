
library(dplyr)
library(stringr)
library(here)

new_ems_data <- readr::read_csv(here("data", "working", "ems_clean_data.csv"))

## These cases prevent separation of units because of incompatible numbers of units and times recorded within the row
diff_length_cases <- new_ems_data %>% 
  filter(str_detect(response_ems_unit_call_sign, "\\|")) %>%
  group_by(response_incident_number, response_ems_unit_call_sign,  incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes, total_unit_response_time, response_vehicle_type) %>%
  summarize(n_units = str_count(response_ems_unit_call_sign, "\\|") + 1,
            n_notified = str_count(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes, "\\|") + 1,
            n_totals = str_count(total_unit_response_time, "\\|") + 1,
            n_vehicle_types = str_count(response_vehicle_type, "\\|") + 1) %>%
  filter(!(n_units == n_notified & n_notified == n_totals & n_totals == n_vehicle_types), n_notified != 1, n_totals != 1, n_units != 1, n_vehicle_types != 1)

## Ugly, but this manual inspection reveals the rest of the problematic incidents. Will just remove for now but long-term may want to save a unit-level dataset
## in addition to patient-level
exclude_cases <- c(diff_length_cases$response_incident_number, 
                   "2017-00010471", "2018-00013833", "2016-00013865", 
                   "2017-00003843", "2017-00012054", "2018-00009978", 
                   "2018-00011371", "2019-00003988", "2020-00001945", "2020-00005896")

units_unrolled <- new_ems_data %>%
  filter(!(response_incident_number %in% exclude_cases)) %>%
  separate_rows(response_ems_unit_call_sign, incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes, total_unit_response_time, response_vehicle_type, sep = "\\|") %>%
  select(response_ems_unit_call_sign, incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes, total_unit_response_time, response_vehicle_type)

## Need to figure out why dispatch notified time is longer than total unit response time
## then can distinguish drive time from loadout time.


# readr::write_csv(units_unrolled, here("data", "working", "unit_level_clean_data.csv"))

