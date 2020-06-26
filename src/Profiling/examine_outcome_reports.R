
library(data.table)
library(stringr)
library(dplyr)
library(here)
library(ggplot2)
library(tidyr)

## Read in data
source(here::here("src","Profiling","joining_albemarle_charlottesville.R"))

## ID duplicate incident numbers
duplicates <- albemarle %>%
  filter(!is.na(response_incident_number), str_detect(response_incident_number, "[0-9]{4}-[0-9]{8}")) %>%
  group_by(response_incident_number) %>%
  mutate(N = n()) %>%
  filter(N > 1)

## 16 incidents that seem to be messed up
## These are cases where we have one incident number, call sign, report type, and demo but still manage to have multiple outcome numbers
duplicates %>%
  group_by(response_incident_number, response_ems_unit_call_sign, outcome_external_report_type, patient_race_list, patient_age, patient_gender) %>%
  summarize(count = n(), 
            non_na_outcome_nums = n_distinct(outcome_external_report_number, na.rm = TRUE), 
            non_na_report_types = n_distinct(outcome_external_report_type, na.rm = TRUE)) %>%
  filter(count > 1, non_na_outcome_nums > 1)

## This is not every incident that needs to be dealt with, but probably the main ones that are intractable.
## Though even some of these seem reasonable as multiple people (e.g. 2019-00013313) - I guess this is where we just happen to have
## two people with same demographics on the same call...?

## Other things that need to be addressed include:
##        dealing with cases where one person has multiple report types (and therefore different numbers)
##        dealing with cases where one incident has both outcome numbers and outcome NAs

## Investigate cases
View(albemarle %>% filter(response_incident_number == "2019-00013422"))

## We can also try grouping just by demographics and incidents:
outcome_report_summary <- duplicates %>%
  group_by(response_incident_number, patient_race_list, patient_age, patient_gender) %>%
  summarize(count = n(), 
            non_na_outcome_nums = n_distinct(outcome_external_report_number, na.rm = TRUE), 
            non_na_report_types = n_distinct(outcome_external_report_type, na.rm = TRUE))

## This doesn't group by unit call sign or report type, so multiple numbers may be related to those factors.
## Nevertheless, we still need to deal with selecting one of those numbers
outcome_report_summary %>% 
  filter(count > 1, 
         non_na_outcome_nums > 0, 
         non_na_report_types > 0, 
         non_na_outcome_nums != non_na_report_types) ## okay if there are multiple numbers if they are associated with different report types

## Duplicates with all NA for outcome numbers - how to deal with these cases? Do we want to assume that these are the same people?
outcome_report_summary %>%
  filter(count > 1, non_na_outcome_nums == 0)

## Duplicates with same number of unique numbers as there are entries
outcome_report_summary %>%
  filter(count > 1, count == non_na_outcome_nums)


