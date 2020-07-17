library(stringr)
library(tidyr)
library(lubridate)

source(here::here("src","Profiling","joining_albemarle_charlottesville.R"))

##############################################################################
# Working with new combined data, this time considering a unit responding as the unit of analysis
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
# response_ems_unit_call_sign

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


# Attempts to fill missing data in grouping columns without combining unique people
# Looks across:
# outcome_external_report_number,
# scene_incident_street_address,
# patient_age,
# patient_gender,
# patient_race_list,
# disposition_destination_name_delivered_transferred_to
#
# If any of them have more than one non-NA value within a set of unique response_incident_number, incident_date,
# it leaves them the same, assuming those represent different people. If each has at most one non-NA value, then
# NA's are populated with existing values to ensure these are counted as a single person.

ems_filled_grouping_columns <- ems_prepared %>%
  group_by(response_incident_number,
           incident_date,
           response_ems_unit_call_sign) %>%
  mutate(across(c(outcome_external_report_number,
                  scene_incident_street_address,
                  patient_age,
                  patient_gender,
                  patient_race_list,
                  disposition_destination_name_delivered_transferred_to),
                function(column) {
                  if(n_distinct(patient_age, na.rm = TRUE) <= 1 &
                     n_distinct(patient_gender, na.rm = TRUE) <= 1 &
                     n_distinct(patient_race_list, na.rm = TRUE) <= 1 &
                     n_distinct(scene_incident_street_address, na.rm = TRUE) <= 1 &
                     n_distinct(outcome_external_report_number, na.rm = TRUE) <= 1 &
                     n_distinct(disposition_destination_name_delivered_transferred_to, na.rm = TRUE) <= 1)
                  {
                    column = ifelse(n_distinct(column, na.rm = TRUE) == 0, NA, unique(column[!is.na(column)])) # catch where all NA
                  } else {
                    column
                  }
                })) %>%
  ungroup() %>%
  distinct()


# Sometimes a patient will have both a patient care report and a medical record number.
# This removes patient care reports when a medical record number exists.
ems_pcr_removed <- ems_filled_grouping_columns %>%
  group_by(response_incident_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date,
           response_ems_unit_call_sign) %>%
  mutate(outcome_external_report_number = ifelse((n() > 1) & sum(outcome_external_report_type == "hospital medical record number (mrn)") > 0,
                                                 outcome_external_report_number[which(outcome_external_report_type == "hospital medical record number (mrn)")][1],
                                                 outcome_external_report_number),
         outcome_external_report_type = ifelse((n() > 1) & sum(outcome_external_report_type == "hospital medical record number (mrn)") > 0,
                                               outcome_external_report_type[which(outcome_external_report_type == "hospital medical record number (mrn)")][1],
                                               outcome_external_report_type)) %>%
  ungroup() %>%
  distinct()




# When some rows within a set of
# response_incident_number,
# outcome_external_report_number,
# scene_incident_street_address,
# patient_age,
# patient_gender,
# patient_race_list,
# disposition_destination_name_delivered_transferred_to,
# incident_date
# response_ems_unit_call_sign
# have missing data when other rows within the observation have data, this fills the missing date with the existing data

ems_filled <- ems_pcr_removed %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date,
           response_ems_unit_call_sign) %>%
  fill(3:ncol(.), .direction = "updown") %>%
  ungroup() %>%
  distinct()



# Turn columns where an entry might be a vector of things, such as a list of crew names
# into a list column where each cell is a character vector

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



# Combines a list columns back into character columns by combining all different sets of values within an observation
# and removing duplicates. Thus if a list started out out of order from another list within the same observation, they will
# become identical after this operation

ems_fully_collapsed <- ems_listed %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date,
           response_ems_unit_call_sign) %>%
  mutate_at(enforce_list, function(x) {
    combined <- unique(flatten(x))
    pasted <- paste0(combined[!is.na(combined)], collapse = "|")
    ifelse(pasted == "", NA, pasted)
  }) %>%
  ungroup() %>%
  distinct()

# combines cases where primary_impression had two different values
ems_primary_combined <- ems_fully_collapsed %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date,
           response_ems_unit_call_sign) %>%
  mutate(across(c(situation_provider_primary_impression_code_and_description,
                  situation_chief_complaint_anatomic_location),
                function(column) {
                  comb_col = paste0(unique(column[!is.na(column)]), collapse = "|")
                  ifelse(comb_col == "", NA, comb_col)
                })) %>%
  ungroup() %>%
  distinct()

# remove pipes | that were accidentaly placed at the beginning or end of a string
ems_pipes_trimmed <- ems_primary_combined %>%
  mutate(across(.cols = all_of(c(enforce_list,
                                 "situation_provider_primary_impression_code_and_description",
                                 "situation_chief_complaint_anatomic_location")),
                .fns = ~str_replace_all(.x, r"(^\||\|$)", "")))


# remove complaint_duration info when they disagree, as well as the remaining couple duplicates, such as pulse ox mismatches,
# slight differences in cardiac arrest datetime, etc.

duplicates <- ems_pipes_trimmed %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date,
           response_ems_unit_call_sign) %>%
  filter(n() > 1)

deduped_duplicates <- duplicates %>%
  mutate(across(c(situation_complaint_duration,
                  situation_complaint_duration_time_units),
                ~ifelse(n_distinct(situation_complaint_duration) > 1 | n_distinct(situation_complaint_duration) > 1 ,
                        NA,
                        .x))) %>%
  mutate(across(everything(),
                ~ifelse(n_distinct(.x) > 1,
                        NA,
                        .x))) %>% # get rid of the last couple random differences that I can't explain
  ungroup() %>%
  distinct()


ems_fully_deduped <- ems_pipes_trimmed %>%
  group_by(response_incident_number,
           outcome_external_report_number,
           scene_incident_street_address,
           patient_age,
           patient_gender,
           patient_race_list,
           disposition_destination_name_delivered_transferred_to,
           incident_date,
           response_ems_unit_call_sign) %>%
  filter(!(n() > 1)) %>%
  bind_rows(deduped_duplicates) %>%
  ungroup() %>%
  distinct()


# clean up column types

enforce_numeric <- c("scene_gps_latitude",
                     "scene_gps_longitude",
                     "patient_initial_blood_glucose_level",
                     "patient_initial_carbon_dioxide_co2_level",
                     "situation_complaint_duration",
                     "patient_initial_pulse_oximetry",
                     "patient_last_pulse_oximetry",
                     "patient_last_blood_glucose_level",
                     "patient_initial_pain_scale_score",
                     "patient_last_pain_scale_score",
                     "patient_initial_body_temperature_in_fahrenheit",
                     "patient_last_body_temperature_in_fahrenheit")

enforce_date_time <- c("cardiac_arrest_date_time",
                       "incident_psap_call_date_time")


# function to turn all complaint durations into minutes
standardize_time <- function(time, time_units) {
  time_table <- tibble(number_of_minutes = c(1/60, 1, 60, 60 * 24, 60 * 24 * 7, 60 * 24 * 30, 60 * 24 * 365),
                       time_units = c("seconds", "minutes", "hours", "days", "weeks", "months", "years"))

  joined_times <- tibble(time_units = time_units) %>%
    left_join(time_table, by = "time_units")

  time * joined_times$number_of_minutes
}

# the final cleaned data set
ems_clean_data <- ems_fully_deduped %>%
  mutate(across(all_of(enforce_numeric), as.numeric),
         across(all_of(enforce_date_time), as_datetime),
         incident_date = as_date(incident_date),
         situation_complaint_duration = standardize_time(situation_complaint_duration, situation_complaint_duration_time_units),
         situation_complaint_duration_time_units = ifelse(is.na(situation_complaint_duration_time_units), NA, "minutes"))

readr::write_csv(ems_clean_data, here::here("data", "working", "ems_clean_data_unit_observation.csv"))
