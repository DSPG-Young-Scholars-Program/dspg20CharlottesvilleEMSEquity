# validating data
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(leaflet)

# run renaming_columns.R first
ems <- charlottesville_renamed

basicInfo <- function(var) {
  # class
  print(class(var))
  # na count
  print(sum(is.na(var)))
  # num levels
  print(nlevels(as.factor(var)))
}

plotFactor <- function(var, n) {
  table(var) %>%
    data.frame(.) %>%
    arrange(desc(Freq)) %>%
    mutate(var = factor(var, unique(var))) %>%
    top_n(n = n, Freq) %>%
    ggplot(., aes(x = var, y = Freq)) +
    geom_bar(stat = 'identity') +
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))
}

ij_sorted <- function(var, i, j) {
  table(var) %>%
    data.frame(.) %>%
    arrange(desc(Freq)) %>%
    mutate(var = factor(var, unique(var))) %>%
    .[i:j,]
}

# getwd() # make sure that working directory = /sfs/qumulo/qhome/scd3dz/git/dspg20CharlottesvilleEMSEquity/
# setwd("/sfs/qumulo/qhome/scd3dz/git/dspg20CharlottesvilleEMSEquity")
# ems <- as.data.table(read_excel("./data/original/CFD_CARS_EMS_DATA_121616TO60920.xlsx", 1))
head(ems)
colnames(ems)

# incident_complaint_reported_by_dispatch
var = ems$`incident_complaint_reported_by_dispatch`
class(var) # character
sum(is.na(var)) # 202
head(var) # seems like I could convert to factor
levels(as.factor(var))
table(var) %>%
  data.frame(.) %>%
  arrange(desc(Freq)) %>%
  mutate(var = factor(var, unique(var))) %>%
  top_n(n=10, Freq) %>% # can get lowest values by supplying negative number
  ggplot(., aes(x = var, y = Freq)) +
  geom_bar(stat = 'identity')
ij_sorted(var, 5, 10)
# could also be compared with one patient primary complaint and other
# columns

# Response Incident Number (eResponse.03)
var = ems$`response_incident_number`
class(var) # character
sum(is.na(var)) # 27
head(var) # seems like I could convert to factor
levels(as.factor(var))
table(var) %>%
  data.frame(.) %>%
  arrange(desc(Freq)) %>%
  mutate(var = factor(var, unique(var))) %>%
  top_n(n=5, Freq) %>%
  ggplot(., aes(x = var, y = Freq)) +
  geom_bar(stat = 'identity')
# weird that there are duplicates for response_incident_number, not sure what that
# would mean or if they are duplicates

# Response EMS Unit Call Sign (eResponse.14)
var = ems$`response_ems_unit_call_sign`
class(var) # character
sum(is.na(var)) # 5
head(var) # seems like I could convert to factor
levels(as.factor(var))
table(var) %>%
  data.frame(.) %>%
  arrange(desc(Freq)) %>%
  mutate(var = factor(var, unique(var))) %>%
  top_n(n = 5, Freq) %>%
  ggplot(., aes(x = var, y = Freq)) +
  geom_bar(stat = 'identity')

# Incident Type
var = ems$`incident_type`
class(var) # character
sum(is.na(var)) # 0
head(var) # seems like I could convert to factor
levels(as.factor(var)) # all EMS

# Incident Date
var = ems$`incident_date`
class(var) # "POSIXct" "POSIXt"
sum(is.na(var)) # 0
head(var)
hist(var, breaks=20)
# generally, dates span from 2016 - 2020, nothing looks wrong here
# could come back later and look at

# Scene GPS Latitude (eScene.11) and Scene GPS Longitude (eScene.11)
# seem to mostly make sense
markers <- ems %>% select(`scene_gps_latitude`, `scene_gps_longitude`)
leaflet(ems) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~`scene_gps_longitude`,
    lat = ~`scene_gps_latitude`,
    radius = 5,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.1
  )

# Disposition Number Of Patients Transported In EMS Unit (eDisposition.11)
var = ems$`disposition_number_of_patients_transported_in_ems_unit`
class(var) # numeric
sum(is.na(var)) # 19286
head(var)
hist(var, breaks = 10)
summary(var)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#  1.000   1.000   1.000   1.014   1.000   7.000   19286

# Agency Name (dAgency.03)
var = ems$`agency_name`
class(var) # character
sum(is.na(var)) # 0
head(var) # seems like I could convert to factor
levels(as.factor(var))

ggplot(data = ems, aes(x = `agency_name`)) +
  geom_bar()
# Cville-Alb Rescue Squad has about double the cases of Cville Fire Dept.

# Situation Provider Primary Impression Code And Description (eSituation.11)
var = ems$`situation_provider_primary_impression_code_and_description`
class(var) # character
sum(is.na(var)) # 12445
head(var) # could maybe be a factor
levels(as.factor(var))
plotFactor(var, 3) # number 1 reason is abuse of alcohol

# Situation Provider Secondary Impression Description And Code (eSituation.12)
var = ems$`situation_provider_secondary_impression_description_and_code`
class(var) # character
sum(is.na(var)) # 28864, why are so many more NA? is there a reason a provider
# wouldn't perform a secondary impression?
head(var)
levels(as.factor(var)) # 179 levels
tbl <- table(var)
plotFactor(var, 5)

# Patient Race List (ePatient.14)
var = ems$`patient_race_list`
class(var) # character
sum(is.na(var)) # 11323
head(var)
tmp <- levels(as.factor(var)) # 36
class(tmp)
nrow(levels(as.factor(var)))
plotFactor(var, 5) # mostly White patients
plotFactor(var, -5)
levels(as.factor(var))


# Patient Gender (ePatient.13)
var <- ems$`patient_gender`
basicInfo(var)
# character with 10323 NAs and 5 levels
plotFactor(var, 5) # how is gender determined? is it given by the patient or
# guessed by the provider?

# patient_age_range_in_years
var <- ems$`patient_age_range_in_years`
basicInfo(var) # character with 11,162 NAs and 12 levels
plotFactor(var, 12)
# hm, what are 44123 and 43839? definitely not age ranges?

# patient_age
var <- ems$`patient_age`
basicInfo(var) # numeric with 11,169 NAs and 112 levels
hist(var, breaks=10)
summary(var)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   1.00   34.00   56.00   53.85   72.00  120.00   11169
# everything looks normaly except for the max of 120, are babies' ages rounded
# up to 1?

ems %>% filter(is.na(patient_age) & !is.na(patient_age_range_in_years)) %>% count()

# patient_suspected_influenza_type_illness
var <- ems$`patient_suspected_influenza_type_illness`
basicInfo(var) # logical with 49909 NAs and 0 levels
# hm not sure why all are NA, maybe a parsing error?

# disposition_destination_name_delivered_transferred_to
var <- ems$`disposition_destination_name_delivered_transferred_to`
basicInfo(var) # character with 16,439 NAs and 10 levels
plotFactor(var, 5)
# vast majority go to UVA Health System then Martha Jefferson
plotFactor(var, -5)

# incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes
var <- ems$incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes
basicInfo(var) # numeric with 2824 NAs and 2167 levels
tmp <- ems %>%
  filter(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes < 60)
hist(tmp$incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes, breaks = 20)
summary(var)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.00    6.38    8.13   10.02   10.43  1981.95    2824

# cad_crew_member_full_name_and_level_list
var <- ems$cad_crew_member_full_name_and_level_list
basicInfo(var) # logical with 49909 NAs and 0 levels

# patient_initial_blood_glucose_level
var <- ems$patient_initial_blood_glucose_level
basicInfo(var) # numeric with 33,735 NA and 525 levels
hist(var, breaks = 20)
summary(var)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# 0.0    94.0   116.0   138.3   151.0  1718.0   33735
# extremely high values for blood glucose? are all of these accurate?

# patient_initial_carbon_dioxide_co2_level
var <- ems$patient_initial_carbon_dioxide_co2_level
basicInfo(var) # numeric with 46,586 NAs and 81 levels
hist(var, breaks = 20)

# patient_cincinnati_stroke_scale_used
var <- ems$patient_cincinnati_stroke_scale_used
basicInfo(var) # logical with 11,741 NAs and 2 levels
plotFactor(var, 2)

# patient_initial_stroke_scale_type
var <- ems$patient_initial_stroke_scale_type
basicInfo(var) # char with 42435 NAs and 4 levels
plotFactor(var, 4)

ems %>% filter(patient_initial_stroke_scale_type == "Cincinnati") %>% count() # 7091
ems %>% filter(patient_cincinnati_stroke_scale_used == TRUE) %>% count() # 7102
# these counts should match up, but they don't

# patient_initial_stroke_scale_score
var <- ems$patient_initial_stroke_scale_score
basicInfo(var) # char with 41813 NAs and 3 levels
plotFactor(var, 3)

# scene_incident_location_type
var <- ems$scene_incident_location_type
basicInfo(var) # char with 2045 NAs and 55 levels
plotFactor(var, 5)

# scene_incident_street_address
var <- ems$scene_incident_street_address
basicInfo(var) # character with 14 NAs and 7964 levels
ij_sorted(var, 1, 5)
# the street address associated with the most observations is a nursing home (or
# similar facility)

ems %>% filter(is.na(scene_gps_latitude) | is.na(scene_gps_longitude)) %>% count() # 543
ems %>% filter((is.na(scene_gps_latitude) | is.na(scene_gps_longitude)) & !is.na(scene_incident_street_address)) %>% count() # 532
tmp <- ems %>%
  filter((is.na(scene_gps_latitude) | is.na(scene_gps_longitude)) & !is.na(scene_incident_street_address))


# outcome_external_report_type
var <- ems$outcome_external_report_type
basicInfo(var) # logical with 49909 NAs and 0 levels

# outcome_external_report_number
var <- ems$outcome_external_report_number
basicInfo(var) # logical with 31123 NAs and 2 levels
plotFactor(var, 2)

# patient_barriers_to_patient_care_list
var <- ems$patient_barriers_to_patient_care_list
basicInfo(var) # char with 132 NAs and 127 levels
plotFactor(var, 5)
levels(as.factor(var)) # lists of values, probably have to unravel

# patient_medical_surgical_history_list
var <- ems$patient_medical_surgical_history_list
basicInfo(var) # character with 25884 NAs and 5424 levels
head(var) # lists of values, probably have to unravel

# patient_medication_allergies_and_type_list
var <- ems$patient_medication_allergies_and_type_list
basicInfo(var) # character with 40,580 NAs and 1544 levels
head(var)

# patient_environmental_food_allergies_list
var <- ems$patient_environmental_food_allergies_list
basicInfo(var) # character with 47742 NA values and 110 levels
plotFactor(var, 5)

# patient_medical_history_obtained_from_list
var <- ems$patient_medical_history_obtained_from_list
basicInfo(var)

# patient_last_oral_intake_date_time
# not sure what this variable represents, perhaps the last time the person
# ate / drank?
var <- ems$patient_last_oral_intake_date_time
basicInfo(var) # POSIXt with 45520 NAs and 3001 levels
hist(var, breaks = 20)
summary(var) # values in early 2000s? those are probably mistakes?
# do the days/months make sense? compare to incident date

# patient_indication_of_pregnancy_with_code
var <- ems$patient_indication_of_pregnancy_with_code
basicInfo(var) # character with 41728 NAs and 6 levels
plotFactor(var, 6)

ems %>%
  filter(patient_indication_of_pregnancy_with_code %in% c("Possible, Unconfirmed (3118003)",
                                                          "Yes, Confirmed < 12 Weeks (3118009)",
                                                          "Yes, Confirmed > 20 Weeks (3118007)",
                                                          "Yes, Confirmed 12-20 Weeks (3118005)",
                                                          "Yes, Weeks Unknown (3118011)") && patient_gender == "Female") %>% count() # 0
# patient_advance_directives_list
var <- ems$ patient_advance_directives_list
basicInfo(var) # character with 42906 NAs and 20 levels
plotFactor(var, 10)
# so many levels of missing: None, Not Recorded, Not Applicable, and Not Reporting

# cardiac_arrest_date_time
var <-ems$cardiac_arrest_date_time
basicInfo(var) # POSIXt with 48618 NAs and 315 levels
hist(var, breaks = 20)
# interesting how there are peaks at the end of every year

# cardiac_arrest_etiology_with_code
var <- ems$cardiac_arrest_etiology_with_code
basicInfo(var) # char with 48118 NAs and 9 levels
plotFactor(var, 5)

# cardiac_arrest_initial_cpr_date_time
var <- ems$cardiac_arrest_initial_cpr_date_time
basicInfo(var) # all NA

# cardiac_arrest_indications_resuscitation_attempted_by_ems_with_code_list
var <- ems$cardiac_arrest_indications_resuscitation_attempted_by_ems_with_code_list
basicInfo(var) # char with 48102 NAs and 23 levels
plotFactor(var, 5)
table(var)

# cardiac_arrest_rosc_date_time
var <- ems$cardiac_arrest_rosc_date_time
basicInfo(var) # all NA

# cardiac_arrest_who_initiated_cpr_with_code
var <- ems$cardiac_arrest_who_initiated_cpr_with_code
basicInfo(var) # all NA

# cardiac_arrest_witnessed_by_list
var <- ems$cardiac_arrest_witnessed_by_list
basicInfo(var) # char with 48092 NA and 13 levels
plotFactor(var, -5)
table(var) # some levels have "Witnessed by Healthcare Provider" and "Not Witnessed"
# which doesn't make sense

# destination_cardiac_arrest_team_activation_date_time
var <- ems$destination_cardiac_arrest_team_activation_date_time
basicInfo(var) # all NA

# injury_cause_of_injury_description_and_code_list
var <- ems$injury_cause_of_injury_description_and_code_list
basicInfo(var) # char with 42269 NAs and 203 levels

# situation_primary_complaint_statement_list
var <- ems$situation_primary_complaint_statement_list
basicInfo(var) # character with 13423 NAs and 8650 levels
plotFactor(var, 5)
ij_sorted(var, 1, 5)

var_l <- var %>%
  as.array(.) %>%
  apply(., 1, tolower) %>%
  data.frame(.)

ij_sorted(var_l, 1, 5) # when converted to all lowercase, we can see that "chest pain"
# is the number one primary complaint

# look at the numer of flu symptoms cases
# doesn't seem to be working atm
ems %>%
  filter(situation_primary_complaint_statement_list == "\"flu symptoms\"") %>% count()

# situation_secondary_complaint_statement_list
var <- ems$situation_secondary_complaint_statement_list
basicInfo(var) # character with 49552 NAs and 136 levels
plotFactor(var, 5)
ij_sorted(var, 1, 5)

var_l <- var %>%
  as.array(.) %>%
  apply(., 1, tolower) %>%
  data.frame(.)

ij_sorted(var_l, 1, 5)

# situation_complaint_duration
var <- ems$situation_complaint_duration
basicInfo(var) # numeric with 17622 NAs and 61 levels
hist(var, breaks = 20)
summary(var)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   1.00    2.00    5.00   10.85   15.00  365.00   17622

# situation_complaint_duration_time_units
var <- ems$situation_complaint_duration_time_units
basicInfo(var) # character with 16968 NAs and 9 levels
plotFactor(var, 9)

# situation_chief_complaint_anatomic_location
var <- ems$situation_chief_complaint_anatomic_location
basicInfo(var) # character with 11730 NAs and 11 levels
plotFactor(var, 5)

# should try validating situation_primary_complaint with anatomic location?

# patient_alcohol_drug_use_indicators_list
var <- ems$patient_alcohol_drug_use_indicators_list
basicInfo(var) # character with 42292 NAs and 96 levels
ij_sorted(var, 10, 20)

# patient_weight_actual_or_estimate_pounds
var <- ems$patient_weight_actual_or_estimate_pounds
basicInfo(var) # all NA

# injury_cause_of_injury
var <- ems$injury_cause_of_injury
basicInfo(var) # character with 41830 NAs and 132 levels
ij_sorted(var, 1, 5)
# just without the codes?

# injury_cause_of_injury_description_list
var <- ems$injury_cause_of_injury_description_list
basicInfo(var) # character with 41831 NAs and 205 levels
ij_sorted(var, 1, 10)
# just without the codes?

# injury_mechanism_of_injury_list
var <- ems$injury_mechanism_of_injury_list
basicInfo(var) # all NA

# incident_protocols_used_list
var <- ems$incident_protocols_used_list
basicInfo(var) # char with 47645 NAs and 155 levels
ij_sorted(var, 1, 10)
# COPD in this column
# could be compared with patient primary complaint

# patient_mental_status_assessment_exam_details
var <- ems$patient_mental_status_assessment_exam_details
basicInfo(var) # all NA

# patient_mental_status_assessment_findings_list
var <- ems$patient_mental_status_assessment_findings_list
basicInfo(var) # character with 35578 NAs and 535 levels
ij_sorted(var, 1, 5)

# patient_neurological_assessment_exam_details
var <- ems$patient_neurological_assessment_exam_details
basicInfo(var) # all NA

# patient_neurological_assessment_findings_list
var <- ems$patient_neurological_assessment_findings_list
basicInfo(var) # character with 40076 NAs and 510 levels
ij_sorted(var, 50, 60)

# patient_skin_assessment_exam_details
var <- ems$patient_skin_assessment_exam_details
basicInfo(var) # all NA

# patient_skin_assessment_findings_list
var <- ems$patient_skin_assessment_findings_list
basicInfo(var) # character with 37265 NAs and 312 levels
ij_sorted(var, 20, 30)

# patient_head_assessment_exam_details
var <- ems$patient_head_assessment_exam_details
basicInfo(var) # all NA

# patient_head_assessment_findings_list
var <- ems$patient_head_assessment_findings_list
basicInfo(var) # char with 42542 NAs and 176 levels
ij_sorted(var, 10, 20)

# patient_eye_assessment_findings_list
# vitals_level_of_responsiveness_avpu
var <- ems$vitals_level_of_responsiveness_avpu
basicInfo(var) # character with 12660 NAs and 6 levels
ij_sorted(var, 1, 6)

# patient_respiratory_effort_list_
var <- ems$patient_respiratory_effort_list_
basicInfo(var) # all NA

# patient_initial_pulse_oximetry
var <- ems$patient_initial_pulse_oximetry
basicInfo(var) # numeric with 15372 NAs and 69 levels
hist(var, breaks = 20)

# patient_last_pulse_oximetry
var <- ems$patient_last_pulse_oximetry
basicInfo(var) # numeric with 15372 NAs and 68 levels
hist(var, breaks = 20)

# patient_last_blood_glucose_level
var <- ems$patient_last_blood_glucose_level
basicInfo(var) # numeric with 33735 NAs and 516 levels

# vitals_cardiac_rhythm_ecg_findings_list
var <- ems$vitals_cardiac_rhythm_ecg_findings_list
basicInfo(var) # character with 41913 NAs and 131 levels
ij_sorted(var, 1, 10)

# patient_initial_pain_scale_score
var <- ems$patient_initial_pain_scale_score
basicInfo(var) # numeric with 40192 NAs and 11 levels
hist(var, breaks = 11)

# patient_last_pain_scale_score
var <- ems$patient_last_pain_scale_score
basicInfo(var) # numeric with 40192 NAs and 11 levels
hist(var, breaks = 11)

ems %>% filter(!is.na(patient_initial_pain_scale_score)) %>% count() # 9717
ems %>% filter(!is.na(patient_initial_pain_scale_score) & !is.na(patient_last_pain_scale_score)) %>% count() # 9717
# every patient that has an initial pain scale score also has a last pain scale score

# patient_initial_body_temperature_in_fahrenheit
var <- ems$patient_initial_body_temperature_in_fahrenheit
basicInfo(var) # numeric with 44624 NAs and 120 levels
hist(var, breaks = 20)
summary(var)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#  36.39   97.89   98.29   98.48   98.90  110.30   44624
# hm, assuming the min value is messed up
# look at the count of values below 95
ems %>% filter(patient_initial_body_temperature_in_fahrenheit < 95) %>% count # 46
ems %>% filter(patient_initial_body_temperature_in_fahrenheit < 90) %>% count # 11
ems %>% filter(patient_initial_body_temperature_in_fahrenheit < 80) %>% count # 7
below80 <- ems %>% filter(patient_initial_body_temperature_in_fahrenheit < 80)
below80$injury_cause_of_injury
# injuries don't really explain why their temps would be that low

# patient_last_body_temperature_in_fahrenheit
var <- ems$patient_last_body_temperature_in_fahrenheit
basicInfo(var) # numeric with 44624 NAs and 120 levels
hist(var, breaks = 20)
summary(var)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#  36.39   97.89   98.29   98.46   98.90  110.30   44624

# medication_given_description_and_rxcui_code
var <- ems$medication_given_description_and_rxcui_code
basicInfo(var) # character with 38735 NAs and 40 levels
ij_sorted(var, 1, 5)

# patient_medication_given_descriptions_list
var <- ems$patient_medication_given_descriptions_list
basicInfo(var) # character with 38701 NAs and 584 levels
ij_sorted(var, 1, 5)

# patient_medication_given_description_and_rxcui_codes_list
var <- ems$patient_medication_given_description_and_rxcui_codes_list
basicInfo(var) # char with 38701 NAs and 584 levels
ij_sorted(var, 1, 5)

# incident_psap_call_date_time
var <- ems$incident_psap_call_date_time
basicInfo(var) # POSIXt with 26 NAs and 29571 levels
hist(var, breaks = 100)

# incident_crew_member_full_name_list
var <- ems$incident_crew_member_full_name_list
basicInfo(var) # character with 473 NAs and 6692 levels
ij_sorted(var, 1, 10)
head(var)

# incident_crew_member_level_list
var <- ems$incident_crew_member_level_list
basicInfo(var) # character with 475 NAs and 286 levels
ij_sorted(var, 1, 5)

# total_unit_response_time
var <- ems$total_unit_response_time
basicInfo(var) # numeric with 2355 NAs and 1425 levels
hist(var, breaks = 20)
summary(var)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's
# 0.000    4.420    5.870    6.672    7.700 1445.280     2355

# patient_attempted_procedure_descriptions_and_codes_list
var <- ems$patient_attempted_procedure_descriptions_and_codes_list
basicInfo(var) # character with 33639 NAs and 1048 levels
ij_sorted(var, 1, 5)
