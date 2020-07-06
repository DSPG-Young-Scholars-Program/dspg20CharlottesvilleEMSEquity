library(stringr)
library(tidyr)

source(here::here("src","Profiling","joining_albemarle_charlottesville.R"))



prepared_charlottesville <- charlottesville %>%
  as_tibble() %>%
  mutate(situation_provider_secondary_impression_description_and_code = ifelse(situation_provider_secondary_impression_description_and_code %in% c("Not Applicable", "Not Recorded"), NA, situation_provider_secondary_impression_description_and_code)) %>%
  mutate(scene_incident_street_address = str_replace(toupper(scene_incident_street_address), "AVENUE", "AVE") %>%
           str_replace(r"(\.)", "") %>%
           ifelse((. == "-1" | . == "0" | . == "0 <UNKNOWN>" | . == "1"), NA, .) %>%
           str_replace(r"(^0 )", "") %>%
           str_replace("STREET", "ST") %>%
           str_replace(r"(^[0-9]+$)", NA_character_) %>%
           str_replace("CIRCLE", "CIR")) %>%
  distinct() %>%
  filter(response_incident_number != "0") %>%  # remove bogus response incident number
  select(response_incident_number, response_ems_unit_call_sign, everything()) # reorder columns for ease of reading


filled_grouping_columns <- prepared_charlottesville %>%
  group_by(response_incident_number) %>%
  mutate(across(c(outcome_external_report_number,
                  scene_incident_street_address,
                  patient_age,
                  patient_gender,
                  patient_race_list),
                function(column) {
                  if(length(unique(patient_age[!is.na(patient_age)])) <= 1 &
                     length(unique(patient_gender[!is.na(patient_gender)])) <= 1 &
                     length(unique(patient_race_list[!is.na(patient_race_list)])) <= 1 &
                     length(unique(scene_incident_street_address[!is.na(scene_incident_street_address)])) <= 1 &
                     length(unique(outcome_external_report_number[!is.na(outcome_external_report_number)])) <= 1) {

                    column = ifelse(length(unique(column[!is.na(column)])) == 0, NA, unique(column[!is.na(column)])) # catch where all NA
                  } else {
                    column
                  }
                })) %>%
  ungroup() %>%
  distinct()

filled_charlottesville <- filled_grouping_columns %>%
  group_by(response_incident_number, outcome_external_report_number, scene_incident_street_address, patient_age, patient_gender, patient_race_list) %>%
  fill(3:ncol(.), .direction = "updown") %>%
  ungroup() %>%
  distinct()


combined_sec_impress <- filled_charlottesville %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list) %>%
  mutate(situation_provider_secondary_impression_description_and_code = paste0(unique(situation_provider_secondary_impression_description_and_code[!is.na(situation_provider_secondary_impression_description_and_code)]), collapse = "|")) %>%
  ungroup() %>%
  distinct() %>%
  mutate(situation_provider_secondary_impression_description_and_code = ifelse(situation_provider_secondary_impression_description_and_code == "", NA, situation_provider_secondary_impression_description_and_code))



combined_departments <- combined_sec_impress %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list) %>%
  mutate(across(c("response_ems_unit_call_sign",
                  "incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes",
                  "total_unit_response_time",
                  "scene_incident_location_type",
                  "agency_name",
                  "incident_crew_member_full_name_list",
                  "incident_crew_member_level_list"
  ),
  function(column) {
    comb_col = paste0(unique(column[!is.na(column)]), collapse = "|")
    ifelse(comb_col == "", NA, comb_col)
  })) %>%
  ungroup() %>%
  mutate(response_ems_unit_call_sign = ifelse(response_ems_unit_call_sign == "", NA, response_ems_unit_call_sign)) %>%
  distinct()

readr::write_csv(combined_departments, here::here("data", "working", "deduplicated_data_wip.csv"))
