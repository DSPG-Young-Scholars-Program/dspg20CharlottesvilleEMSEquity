
library(dplyr)
library(lubridate)

cur_dedup <- readr::read_csv(here::here("data", "working", "current_deduplicated_data.csv"))

# source(here::here("src", "Profiling", "joining_albemarle_charlottesville.R"))

## Subset to duplicate incident numbers
ems_duplicates <- cur_dedup %>% 
  distinct() %>%
  group_by(response_incident_number, incident_date) %>%
  mutate(N = n()) %>%
  filter(N > 1)

report_num_summary <- ems_duplicates %>%
  group_by(response_incident_number, incident_date, patient_race_list, patient_gender, patient_age) %>%
  summarize(n_rep_nums = n_distinct(outcome_external_report_number, na.rm = TRUE),
            n_rep_types = n_distinct(outcome_external_report_type, na.rm = TRUE),
            n_units = n_distinct(response_ems_unit_call_sign, na.rm = TRUE)) 

# ---- Multiple numbers, multiple report types ---- #

## May be individual people who are being ID'ed as multiple people because of two report types
## Should clean - maybe remove all PCR numbers when someone has an MRN before doing the roll up?
report_num_summary %>% filter(n_rep_nums > 1, n_rep_types > 1)

# ---- Multiple numbers, single report type ---- #

## Others are cases where we have one MRN and two numbers - should just call two people I think.
report_num_summary %>% filter(n_rep_nums > 1, n_rep_types == 1)

# ---- Multiple units, single report number, but not rolled up? ---- #

## These are not necessarily errors but I'm a little confused about why they haven't been rolled up based on Ellen's script

## Many of these cases seem to be where multiple units record same number but one unit does not record a report type. 
## Report type should be rolled up - it looks like most of these are single people who are not being ID'ed as such.
report_num_summary %>% filter(n_rep_nums == 1, n_units > 1)
