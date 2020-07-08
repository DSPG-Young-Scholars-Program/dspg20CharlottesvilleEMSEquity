
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)

## Read in data
source(here::here("src", "Profiling", "joining_albemarle_charlottesville.R"))

## Fix outcome report numbers with leading zeros in county data - move this to joining_albemarle_charlottesville.R?
# ems_full <- ems_full %>% 
#   mutate(outcome_external_report_number = str_replace(outcome_external_report_number, "(?<![0-9])0+", ""))

#
#
# Full data ----------------------------------------------------------------------------------------------------------------
#
#

## Subset to duplicate incident numbers
ems_duplicates <- ems_full %>% 
  filter(!is.na(response_incident_number)) %>% ## This can potentially be changed once parsing issues are figured out
  distinct() %>%
  group_by(response_incident_number) %>%
  mutate(N = n()) %>%
  filter(N > 1)

## Group by incident, unit, and demographics to summarize duplicates
report_num_summary <- ems_duplicates %>%
  group_by(response_incident_number, response_ems_unit_call_sign, patient_race_list, patient_age, patient_gender) %>%
  summarize(count = n(), 
            non_na_outcome_nums = n_distinct(outcome_external_report_number, na.rm = TRUE), 
            non_na_report_types = n_distinct(outcome_external_report_type, na.rm = TRUE),
            na_outcome_nums = sum(is.na(outcome_external_report_number)),
            n_sources = n_distinct(source)) 

# ---- One person, multiple report numbers ---- #

## These are cases where we have one incident number, call sign, report type, and demo but still manage to have multiple non-NA outcome numbers

## Will have to decide whether we think these are cases where two people happened to have the same demographics, or if they are errors
## Might be worth just doing this manually - a glance suggests some seem to be the same person, others are more ambiguous
report_num_summary %>%
  filter(count > 1, non_na_outcome_nums > 1, non_na_report_types != non_na_outcome_nums)

## Multiple report types (may be necessary to overwrite the second report type with the MRN to make it easier to ID individual patients)
report_num_summary %>% 
  filter(non_na_report_types > 1)

# ---- One person, report number + NA report numbers ---- #

## Incidents where potentially same person has both an outcome num and NA outcome nums
## Most of these probably represent the same person and can fill in the NA number?
report_num_summary %>% 
  filter(count > 1, na_outcome_nums > 0, count != na_outcome_nums)

# ---- One report number for multiple demographic sets ---- #

## As a logical check, we also consider the reverse direction:
## If we group by incident, unit, and report type, how many times do we have distinct set of demographic characteristics?
multi_demo_cases <- ems_duplicates %>% 
  group_by(response_incident_number, response_ems_unit_call_sign, outcome_external_report_type, outcome_external_report_number) %>%
  mutate(demo_comb = paste(paste(patient_race_list, patient_age, sep = "|"), patient_gender, sep = "|"), n_demo = n_distinct(demo_comb)) %>%
  select(response_incident_number, response_ems_unit_call_sign, outcome_external_report_type, outcome_external_report_number, demo_comb, n_demo) %>%
  filter(n_demo > 1)

## Cases where single report number associated with different demographic characteristics
## This seems to suggest that **where there is a report number**, that number does a pretty good job of IDing a unique individual
## Question remains whether we want to assume that this pattern holds for people with NA report number
multi_demo_cases %>% 
  filter(!is.na(outcome_external_report_number))

## ----

## Summary of outcome numbers associated with both county and city
ems_full %>% 
  group_by(response_incident_number, outcome_external_report_number) %>% 
  summarize(n = n_distinct(source)) %>% 
  filter(n > 1, !is.na(outcome_external_report_number))

## Visual inspection 
# View(ems_full %>% filter(response_incident_number == "2019-00010068"))

#
#
# Same procedures on separate data sources, in case this is helpful for consistency checks ----------------
#
#

#
#
# County Data ---------------------------------------------------------------------------------------------
#
#

## ID duplicate incident numbers
dups_county <- albemarle %>%
  filter(!is.na(response_incident_number)) %>% ## This can potentially be changed once parsing issues are figured out
  distinct() %>%
  group_by(response_incident_number) %>%
  mutate(N = n()) %>%
  filter(N > 1)

## Grouping by incident number, call sign, report type, and demographics
outcome_report_summary_1 <- dups_county %>%
  group_by(response_incident_number, response_ems_unit_call_sign, patient_race_list, patient_age, patient_gender) %>%
  summarize(count = n(), 
            non_na_outcome_nums = n_distinct(outcome_external_report_number, na.rm = TRUE), 
            non_na_report_types = n_distinct(outcome_external_report_type, na.rm = TRUE),
            na_outcome_nums = sum(is.na(outcome_external_report_number))) 

# ---- One person, multiple report numbers ---- #

outcome_report_summary_1 %>%
  filter(count > 1, non_na_outcome_nums > 1, non_na_report_types != non_na_outcome_nums)

## Multiple report types
outcome_report_summary_1 %>%
  filter(non_na_report_types > 1)

# ---- One person, report number + NA report numbers ---- #

outcome_report_summary_1 %>% 
  filter(count > 1, na_outcome_nums > 0, count != na_outcome_nums)

# ---- One report number for multiple demographic sets ---- #

multi_demo_cases_county <- dups_county %>% 
  group_by(response_incident_number, response_ems_unit_call_sign, outcome_external_report_type, outcome_external_report_number) %>%
  mutate(demo_comb = paste(paste(patient_race_list, patient_age, sep = "|"), patient_gender, sep = "|")) %>%
  mutate(n_demo = n_distinct(demo_comb)) %>%
  select(response_incident_number, response_ems_unit_call_sign, outcome_external_report_type, outcome_external_report_number, demo_comb, n_demo) %>%
  filter(n_demo > 1)

## Cases where single report number associated with different demographic characteristics
multi_demo_cases_county %>% 
  filter(!is.na(outcome_external_report_number))

# -----

## For manual inspection of cases
# View(albemarle %>% filter(response_incident_number == "2017-00001979"))

#
#
# City Data ---------------------------------------------------------------------------------------------
#
#

## ID duplicate incidents
dups_cville <- charlottesville %>% 
  filter(!is.na(response_incident_number)) %>% ## This can potentially be changed once parsing issues are figured out
  distinct() %>%
  group_by(response_incident_number) %>%
  mutate(N = n()) %>%
  filter(N > 1)

## Grouping by incident number, call sign, report type, and demographics
outcome_report_summary_2 <- dups_cville %>%
  group_by(response_incident_number, response_ems_unit_call_sign, patient_race_list, patient_age, patient_gender) %>%
  summarize(count = n(), 
            non_na_outcome_nums = n_distinct(outcome_external_report_number, na.rm = TRUE), 
            non_na_report_types = n_distinct(outcome_external_report_type, na.rm = TRUE),
            na_outcome_nums = sum(is.na(outcome_external_report_number))) 

# ---- One person, multiple report numbers ---- #

## Multiple numbers for one demographic set (likely single individual)
outcome_report_summary_2 %>%
  filter(count > 1, non_na_outcome_nums > 1, non_na_report_types != non_na_outcome_nums)

## Cases where multiple numbers is explained by multiple report types 
outcome_report_summary_2 %>% 
  filter(non_na_report_types > 1)

# ---- One person, report number + NA report numbers ---- #

outcome_report_summary_2 %>% 
  filter(count > 1, na_outcome_nums > 0, count != na_outcome_nums)

# ---- One report number for multiple demographic sets ---- #

multi_demo_cases_cville <- dups_cville %>% 
  group_by(response_incident_number, response_ems_unit_call_sign, outcome_external_report_type, outcome_external_report_number) %>%
  mutate(demo_comb = paste(paste(patient_race_list, patient_age, sep = "|"), patient_gender, sep = "|")) %>%
  mutate(n_demo = n_distinct(demo_comb)) %>%
  select(response_incident_number, response_ems_unit_call_sign, outcome_external_report_type, outcome_external_report_number, demo_comb, n_demo) %>%
  filter(n_demo > 1)

multi_demo_cases_cville %>% 
  filter(!is.na(outcome_external_report_number))

# ----

## For manual inspection
# View(charlottesville %>% filter(response_incident_number == "2019-00006316"))

