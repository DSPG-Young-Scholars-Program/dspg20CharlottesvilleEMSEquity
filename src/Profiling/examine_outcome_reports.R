
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)

## Read in data
source(here::here("src","Profiling","joining_albemarle_charlottesville.R"))

## ID duplicate incident numbers
duplicates <- albemarle %>%
  filter(!is.na(response_incident_number), str_detect(response_incident_number, "[0-9]{4}-[0-9]{8}")) %>% ## This can potentially be changed once parsing issues are figured out
  distinct() %>%
  group_by(response_incident_number) %>%
  mutate(N = n()) %>%
  filter(N > 1)

## Grouping by incident number, call sign, report type, and demographics
outcome_report_summary_1 <- duplicates %>%
  group_by(response_incident_number, response_ems_unit_call_sign, patient_race_list, patient_age, patient_gender) %>%
  summarize(count = n(), 
            non_na_outcome_nums = n_distinct(outcome_external_report_number, na.rm = TRUE), 
            non_na_report_types = n_distinct(outcome_external_report_type, na.rm = TRUE),
            na_outcome_nums = sum(is.na(outcome_external_report_number))) 

# ---- One person, multiple report numbers ---- #

## 20 questionable incidents
## These are cases where we have one incident number, call sign, report type, and demo but still manage to have multiple non-NA outcome numbers
## Will have to decide whether we think these are cases where two people happened to have the same demographics, or if they are errors
## Might be worth just doing this manually - a glance suggests some seem to be the same person, others are more ambiguous
outcome_report_summary_1 %>%
  filter(count > 1, non_na_outcome_nums > 1, non_na_report_types != non_na_outcome_nums)

# ---- One person, report number + NA report numbers ---- #

## 10 incidents where potentially same person has both an outcome num and NA outcome nums
## Most of these probably represent the same person and can fill in the NA number?
outcome_report_summary_1 %>% 
  filter(count > 1, na_outcome_nums > 0, count != na_outcome_nums)

# ---- One set of demographics, multiple report numbers ---- #

## As a logical check, we also consider the reverse direction:
## If we group by incident, unit, and report type, how many times do we have distinct set of demographic characteristics?
multi_demo_cases <- duplicates %>% 
  group_by(response_incident_number, response_ems_unit_call_sign, outcome_external_report_type, outcome_external_report_number) %>%
  mutate(demo_comb = paste(paste(patient_race_list, patient_age, sep = "|"), patient_gender, sep = "|")) %>%
  mutate(n_demo = n_distinct(demo_comb)) %>%
  select(response_incident_number, response_ems_unit_call_sign, outcome_external_report_type, outcome_external_report_number, demo_comb, n_demo) %>%
  filter(n_demo > 1)

## Cases where single report number associated with different demographic characteristics
## This seems to suggest that **where there is a report number**, that number does a pretty good job of IDing a unique individual
## Question remains whether we want to assume that this pattern holds for people with NA report number
multi_demo_cases %>% filter(!is.na(outcome_external_report_number))

# -----

## For manual inspection of cases
View(albemarle %>% filter(response_incident_number == "2017-00007706"))

# -----

## Note:
## These summaries don't really deal with the fact that a single person can have multiple report types, and therefore multiple numbers.
## Will have to include this in any approach to resolve duplicates.
