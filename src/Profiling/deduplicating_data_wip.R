library(stringr)
library(tidyr)
library(naniar)

source(here::here("src","Profiling","joining_albemarle_charlottesville.R"))

##############################################################################
# Working with new combined data
##############################################################################

# considering one observation to be a unique combination of the following:
#
# response_incident_number,
# outcome_external_report_number,
# scene_incident_street_address,
# patient_age,
# patient_gender,
# patient_race_list,
# disposition_destination_name_delivered_transferred_to,
# incident_date

# change column types, clean up address names, force all text to lowercase, and remove invalid data

enforce_numeric <- c("scene_gps_latitude",
                     "scene_gps_longitude",
                     "incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes",
                     "patient_initial_blood_glucose_level",
                     "patient_initial_carbon_dioxide_co2_level",
                     "situation_complaint_duration",
                     "patient_initial_pulse_oximetry",
                     "patient_last_pulse_oximetry",
                     "patient_last_blood_glucose_level",
                     "patient_initial_pain_scale_score",
                     "patient_last_pain_scale_score",
                     "patient_initial_body_temperature_in_fahrenheit",
                     "patient_last_body_temperature_in_fahrenheit",
                     "total_unit_response_time")

na_pattern <- r"(\"*[Nn]ot [Aa]pplicable\"*|\"* *[Nn]ot [Rr]ecorded\"*|\"*[Nn]/[Aa]\"*|\"*[Nn]ot [Aa]pplicable\"*|\"*[Nn]ot [Rreporting]\"*)"

ems_prepared <- new_ems_data %>%
  as_tibble() %>%
  mutate(scene_incident_street_address = str_replace(toupper(scene_incident_street_address), "AVENUE", "AVE") %>%
           str_replace(r"(\.)", "") %>%
           ifelse((. == "-1" | . == "0" | . == "0 <UNKNOWN>" | . == "1"), NA, .) %>%
           str_replace(r"(^0 )", "") %>%
           str_replace("STREET", "ST") %>%
           str_replace(r"(^[0-9]+$)", NA_character_) %>%
           str_replace("CIRCLE", "CIR")) %>%
  distinct() %>%
  filter(response_incident_number != "0") %>%  # remove bogus response incident number
  select(response_incident_number, response_ems_unit_call_sign, everything()) %>%  # reorder columns for ease of reading
  mutate(across(.cols = all_of(enforce_numeric), .fns = as.numeric),
         cardiac_arrest_date_time = mdy_hms(cardiac_arrest_date_time)) %>% # enforce data types for columns
  mutate(across(where(is.character), tolower)) %>%  # turn all characters to lowercase
  mutate(across(everything(),  ~ifelse(str_detect(.x, na_pattern), NA, .x))) %>%
  filter(across(where(is.character), ~(!grepl("test", .x))))


# only fill demographic data and other grouping data if there are no mismatches in any of the grouping variables

ems_filled_grouping_columns <- ems_prepared %>%
  group_by(response_incident_number,
           incident_date) %>%
  mutate(across(c(outcome_external_report_number,
                  scene_incident_street_address,
                  patient_age,
                  patient_gender,
                  patient_race_list),
                function(column) {
                  if(n_distinct(patient_age, na.rm = TRUE) <= 1 &
                     n_distinct(patient_gender, na.rm = TRUE) <= 1 &
                     n_distinct(patient_race_list, na.rm = TRUE) <= 1 &
                     n_distinct(scene_incident_street_address, na.rm = TRUE) <= 1 &
                     n_distinct(outcome_external_report_number, na.rm = TRUE) <= 1 &
                     n_distinct(disposition_destination_name_delivered_transferred_to, na.rm = TRUE) <= 1) {

                    column = ifelse(n_distinct(column, na.rm = TRUE) == 0, NA, unique(column[!is.na(column)])) # catch where all NA
                  } else {
                    column
                  }
                })) %>%
  ungroup() %>%
  distinct()


# remove pcr numbers when a patient already has an MCR
tmp <- ems_filled_grouping_columns %>%
  group_by(response_incident_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date) %>%
  mutate(outcome_external_report_number = ifelse((n() > 1) & sum(outcome_external_report_type == "hospital medical record number (mrn)") > 0,
                                                 outcome_external_report_number[which(outcome_external_report_type == "hospital medical record number (mrn)")][1],
                                                 outcome_external_report_number))


ems_filled_grouping_columns %>%
  filter(response_incident_number == "2017-00002906") %>%
  group_by(response_incident_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date) %>%
  filter((n() > 1))

# fill missing values when an existing value exists within the unit of observation

ems_filled <- ems_filled_grouping_columns %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date) %>%
  fill(3:ncol(.), .direction = "updown") %>%
  ungroup() %>%
  distinct()



# turn list columns into real lists to help with merging soon

enforce_list_quoted_comma <- c("injury_cause_of_injury_description_and_code_list",
                               "situation_provider_secondary_impression_description_and_code_list",
                               "situation_primary_complaint_statement_list",
                               "situation_secondary_complaint_statement_list",
                               "patient_medication_given_description_and_rxcui_codes_list",
                               "patient_attempted_procedure_descriptions_and_codes_list")

enforce_list_text_enclosed_comma <- c("patient_barriers_to_patient_care_list",
                                      "patient_medical_surgical_history_list",
                                      "patient_alcohol_drug_use_indicators_list",
                                      "incident_protocols_used_list",
                                      "patient_mental_status_assessment_findings_list",
                                      "patient_neurological_assessment_findings_list",
                                      "patient_skin_assessment_findings_list",
                                      "patient_head_assessment_findings_list",
                                      "patient_eye_assessment_findings_list",
                                      "patient_medication_given_rxcui_codes_list",
                                      "incident_crew_member_full_name_list",
                                      "incident_crew_member_level_list")

enforce_list_text_one_side_comma <- c("patient_race_list")


enforce_list <- c(enforce_list_quoted_comma, enforce_list_text_enclosed_comma, enforce_list_text_one_side_comma)

ems_listed <- ems_filled %>%
  mutate(across(.cols = all_of(enforce_list_quoted_comma),
                .fns = ~str_split(.x, pattern = r"((?<=\"),(?= {0,1}\"))")),
         across(.cols = all_of(enforce_list_text_enclosed_comma),
                .fns = ~str_split(.x, pattern = r"((?<=\S),(?=\S))")),
         across(.cols = all_of(enforce_list_text_one_side_comma),
                .fns = ~str_split(.x, pattern = r"((?<=\S), )"))) %>%
  mutate(across(.cols = all_of(enforce_list),
                .fns = ~map(.x, ~map_chr(.x, ~str_trim(str_replace_all(.x, "\"", "")), side = "both"))))



# Collapse departments into one
department_vars <- c("response_ems_unit_call_sign",
                     "incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes",
                     "total_unit_response_time",
                     "scene_incident_location_type",
                     "agency_name",
                     "incident_complaint_reported_by_dispatch",
                     "response_vehicle_full_address",
                     "response_vehicle_type")

ems_collapsed <- ems_listed %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date) %>%
  mutate(across(.cols = all_of(department_vars),
         .fns = function(column) {
           comb_col = paste0(unique(column[!is.na(column)]), collapse = "|")
           ifelse(comb_col == "", NA, comb_col)
         })) %>%
  ungroup() %>%
  distinct()



# Collapse all list columns into a single cell

ems_fully_collapsed <- ems_collapsed %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date) %>%
  mutate_at(enforce_list, function(x) {
      combined <- unique(flatten(x))
      pasted <- paste0(combined[!is.na(combined)], collapse = "|")
      ifelse(pasted == "", NA, pasted)
    }) %>%
  ungroup() %>%
  distinct()

ems_primary_combined <- ems_fully_collapsed %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date) %>%
  mutate(across(c(situation_provider_primary_impression_code_and_description,
                  situation_chief_complaint_anatomic_location),
                function(column) {
                  comb_col = paste0(unique(column[!is.na(column)]), collapse = "|")
                  ifelse(comb_col == "", NA, comb_col)
                })) %>%
  ungroup() %>%
  distinct()

# remove pipes that were accidentaly placed at the beginning or end of a string
ems_pipes_trimmed <- ems_primary_combined %>%
  mutate(across(.cols = all_of(c(enforce_list,
                         department_vars,
                         "situation_provider_primary_impression_code_and_description",
                         "situation_chief_complaint_anatomic_location")),
                .fns = ~str_replace_all(.x, r"(^\||\|$)", "")))


# write.csv(ems_primary_combined, here::here("data", "working", "current_deduplicated_data.csv"))
