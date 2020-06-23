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

top_n_sorted <- function(var, i, j) {
  table(var) %>%
    data.frame(.) %>%
    arrange(desc(Freq)) %>%
    mutate(var = factor(var, unique(var))) %>%
    .[i:j,]
}


# situation_primary_complaint_statement_list
var <- ems$situation_primary_complaint_statement_list
basicInfo(var) # character with 13423 NAs and 8650 levels
plotFactor(var, 5)
top_n_sorted(var, 1, 5)

var_l <- var %>%
  as.array(.) %>%
  apply(., 1, tolower) %>%
  data.frame(.)

top_n_sorted(var_l, 1, 5) # when converted to all lowercase, we can see that "chest pain"
# is the number one primary complaint

# look at the numer of flu symptoms cases
# doesn't seem to be working atm
ems %>%
  filter(situation_primary_complaint_statement_list == "\"flu symptoms\"") %>% count()

# situation_secondary_complaint_statement_list
var <- ems$situation_secondary_complaint_statement_list
basicInfo(var) # character with 49552 NAs and 136 levels
plotFactor(var, 5)
top_n_sorted(var, 1, 5)

var_l <- var %>%
  as.array(.) %>%
  apply(., 1, tolower) %>%
  data.frame(.)

top_n_sorted(var_l, 1, 5)

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
