library(dplyr)
library(here)
library(purrr)
library(stringr)
library(lubridate)

## Columns mutually selected to be removed based on irrelevance or data quality issues
drop_cols <- c("cad_crew_member_full_name_and_level_list",
               "cardiac_arrest_etiology_with_code",
               "cardiac_arrest_initial_cpr_date_time",
               "cardiac_arrest_indications_resuscitation_attempted_by_ems_with_code_list",
               "cardiac_arrest_rosc_date_time",
               "cardiac_arrest_who_initiated_cpr_with_code",
               "cardiac_arrest_witnessed_by_list",
               "destination_cardiac_arrest_team_activation_date_time",
               "incident_type",
               "injury_cause_of_injury",
               "injury_cause_of_injury_description_list",
               "injury_mechanism_of_injury_list",
               "medication_given_description_and_rxcui_code",
               "patient_advance_directives_list",
               "patient_cincinnati_stroke_scale_used",
               "patient_environmental_food_allergies_list",
               "patient_head_assessment_exam_details",
               "patient_initial_stroke_scale_type",
               "patient_last_oral_intake_date_time",
               "patient_medical_history_obtained_from_list",
               "patient_medication_allergies_and_type_list",
               "patient_medication_given_descriptions_list",
               "patient_mental_status_assessment_exam_details",
               "patient_neurological_assessment_exam_details",
               "patient_respiratory_effort_list",
               "patient_skin_assessment_exam_details",
               "patient_weight_actual_or_estimate_pounds",
               "vitals_cardiac_rhythm_ecg_findings_list",
               "vitals_level_of_responsiveness_avpu")

albemarle <- readxl::read_xlsx(here("data","original","Data4UVA.xlsx"), 1, col_types = c(rep("text", 4), "date", rep("text", 78))) %>%
  rename_with(~tolower(gsub(r"( +\(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(r"( )", "_", .x)) %>%  # change periods to underscores
  select(-all_of(drop_cols))

  ## Add this to scrub leading zeros from outcome numbers so they will match with city data - make sure this isn't messing up when numbers have dashes, etc.
  # mutate(outcome_external_report_number = str_replace(outcome_external_report_number, "(?<![0-9])0+", ""))

charlottesville <- readxl::read_xlsx(here("data","original","CFD_CARS_EMS_DATA_121616TO60920.xlsx"), 1, col_types = c(rep("text", 4), "date", rep("text", 78))) %>%
  rename_with(~tolower(gsub(r"( +\(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(" ", "_", .x)) %>% # change spaces to underscores
  select(-all_of(drop_cols))

# these variables probably mean the same thing, so I'll just rename the albemarle one
albemarle <- albemarle %>%
  rename(total_unit_response_time = incident_unit_notified_by_dispatch_to_unit_arrived_on_scene_in_minutes)

## Combine data
ems_full <- bind_rows(mutate(albemarle, source = "albemarle"),
                      mutate(charlottesville, source = "charlottesville")) # tag on source variable

## Read in new data


# set column types
col_spec <- rep("c", 84)
col_spec <- paste0(col_spec, collapse = "")

# change variables that changed names
drop_cols[which(drop_cols == "cad_crew_member_full_name_and_level_list")] <- "incident_crew_member_full_name_and_level_list"
drop_cols <- drop_cols[which(!(drop_cols %in% c("vitals_cardiac_rhythm_ecg_findings_list", "medication_given_description_and_rxcui_code")))]



new_ems_data <- vroom::vroom(here("data", "original", "2020 UVA Project County Bulk Export_Export.csv"),
                        col_types = col_spec) %>%
  rename_with(~tolower(gsub(r"( +\(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(r"( )", "_", .x)) %>%  # change periods to underscores
  select(-all_of(drop_cols))  %>%
  mutate(incident_date = as_date(mdy_hms(incident_date)),
         incident_psap_call_date_time = mdy_hms(incident_psap_call_date_time))



