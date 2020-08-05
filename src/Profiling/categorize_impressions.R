
library(here)
library(dplyr)
library(stringr)
library(tidyr)

ems_data <- vroom::vroom(here("data", "working", "first_unit_at_scene_data.csv"))
#ems_data <- vroom::vroom(here("data", "working", "ems_clean_data_unit_observation.csv"))
ems_data <- vroom::vroom(here("data", "final", "ems_clean_data.csv"))

## Mappings for provider impressions to impression categories
categorize_impressions <- function(x) {
  trimws(case_when(str_detect(x, "alcohol|abuse of") ~ "abuse of substance",
                   str_detect(x, "ob -") ~ "ob",
                   str_detect(x, "endocrine") ~ "endocrine",
                   str_detect(x, "gi/gu|gi bleed") ~ "gi/gu",
                   str_detect(x, "neuro -") ~ "neuro",
                   str_detect(x, "cv -") ~ "cv",
                   str_detect(x, "respir") ~ "respiratory",
                   str_detect(x, "infectio|weakness|malaise|fever") ~ "infectious",
                   str_detect(x, "injury|burn") ~ "injury",
                   str_detect(x, "behav") ~ "behavioral",
                   str_detect(x, "enviro|dehydration") ~ "environment",
                   str_detect(x, "pain") ~ "pain",
                   !is.na(x) ~ "other",
                   TRUE ~ "missing"))
}

## This function uses the values recorded in the complaint reported by dispatch to impute possible values for the primary impression where it is missing
## Note that the function is still rough - it may fail for data with low counts, or it may also fail if some complaint strings overlap in terms of the rows they identify. Could be refined
impute_impressions <- function(full_data, ## data to impute
                               freq_data, ## count frequencies for each impression/complaint combination. 3 columns: possible_impression_category, incident_complaint_reported_by_dispatch, n
                               complaint_strings ## a vector of strings that will be matched on the incident_complaint_reported_by_dispatch column for imputation
) {
  
  ## Function that rounds values while preserving total sum
  smart_round <- function(x) {
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y
  }
  
  ## Iterate through complaint strings to use as matches
  for (complaint_string in complaint_strings) {
    
    ## Count the number of times this complaint occurs paired with a missing impression
    complaint_N <- nrow(full_data %>% filter(str_detect(incident_complaint_reported_by_dispatch, complaint_string), possible_impression_category == "missing"))
    
    ## Determine frequency of non-missing primary impressions associated with the current complaint string
    complaint_impression_freq <- freq_data %>% 
      filter(str_detect(incident_complaint_reported_by_dispatch, complaint_string), possible_impression_category != "missing") %>%
      group_by(possible_impression_category) %>%
      count(wt = n) %>%
      ungroup() %>%
      mutate(freq = n / sum(n), 
             allocated = smart_round(complaint_N * freq))
    
    ## Create vector of possible primary impression values in random order. Each impression is present at the frequency it is present in the non-missing impression data for this complaint
    possible_categories <- sample(rep(complaint_impression_freq$possible_impression_category, complaint_impression_freq$allocated))
    
    ## Where we still have not updated the possible impression, add the possible impression vector
    possible_impressions_categorized <- full_data %>%
      filter(str_detect(incident_complaint_reported_by_dispatch, complaint_string) & possible_impression_category == "missing") %>%
      mutate(possible_impression_category = possible_categories)
    
    ## Get the remaining data unaffected by the possible impression update and bind back to the now-updated possible impressions data
    remaining_data <- full_data %>%
      filter(!(str_detect(incident_complaint_reported_by_dispatch, complaint_string)) | possible_impression_category != "missing" | is.na(incident_complaint_reported_by_dispatch))
    
    full_data <- bind_rows(possible_impressions_categorized, remaining_data) ## Update full data in case multiple complaint strings were passed
    
  }
  
  return(full_data)
  
}

#
#
# Categorize Impressions ----------------------------------------------------------------------------------------------------------------------------------
#
#

## ---- Reduce impressions that are present to fewer categories based on direct mappings above ---- ##

ems_impressions_categorized <- ems_data %>% 
  mutate(primary_impression_list = str_split(situation_provider_primary_impression_code_and_description, "\\|"), ## split multiple impression cases
         primary_impression_category_list = purrr::map(primary_impression_list, categorize_impressions), ## categorize all impressions
         primary_impression_category_collapsed = purrr::map(primary_impression_category_list, unique), ## check if multiple impression cases have the same category
         patient_race_trimmed = str_extract(patient_race_list, "[^|]*")) %>% ## pull out first value of race list where there are multiples
  filter(lengths(primary_impression_category_collapsed) == 1) %>% ## remove remaining cases (~100) where impressions gave conflicting categories
  mutate(primary_impression_category = as.character(primary_impression_category_collapsed)) %>%
  select(-c(primary_impression_list, primary_impression_category_list, primary_impression_category_collapsed))

## ---- Where there is no primary impression, use same categories on secondary impression ---- ##

secondary_impressions_categorized <- ems_impressions_categorized %>% 
  filter(primary_impression_category == "missing", !is.na(situation_provider_secondary_impression_description_and_code_list)) %>%
  mutate(secondary_impression_list = str_split(situation_provider_secondary_impression_description_and_code_list, "\\|"), ## split multiple impression cases
         secondary_impression_category_list = purrr::map(secondary_impression_list, categorize_impressions), ## categorize all impressions
         secondary_impression_category_collapsed = purrr::map(secondary_impression_category_list, unique)) %>% ## check if multiple impressions have the same category
  filter(lengths(secondary_impression_category_collapsed) == 1) %>% ## remove remaining cases (~40) where impressions gave conflicting categories
  mutate(secondary_impression_category = as.character(secondary_impression_category_collapsed)) %>%
  select(-c(secondary_impression_list, secondary_impression_category_list, secondary_impression_category_collapsed))

## Create single column incorporating both primary and secondary categories
impressions_categorized_full <- left_join(ems_impressions_categorized, secondary_impressions_categorized) %>%
  mutate(impression_category = ifelse(is.na(secondary_impression_category), primary_impression_category, secondary_impression_category)) %>%
  select(-c(primary_impression_category, secondary_impression_category))

## ---- Where impressions are still missing, impute possible values using the complaint reported by dispatch ---- ##

## Add new column to populate with possible impression categories
impressions_categorized_full <- impressions_categorized_full %>%
  mutate(possible_impression_category = impression_category)

## Generate frequency table of impression/complaint pairs
impression_complaint_freqs <- impressions_categorized_full %>%
  group_by(possible_impression_category, incident_complaint_reported_by_dispatch) %>%
  count()

## Note that order matters because the impute_impressions function is greedy
complaints_to_classify <- c("traffic|transp", "fall", "breath", "chest", "abdom", "sick person", 
                            "cardiac", "stroke", "public", "medical alarm", "seiz", "injur", 
                            "faint", "burn|fire", "overdose", "psych", "diabet")

## Impute possible impressions
impressions_categorized_imputed <- impressions_categorized_full %>% 
  impute_impressions(freq_data = impression_complaint_freqs, complaint_strings = complaints_to_classify)

# readr::write_csv(impressions_categorized_full, here::here("data", "final", "ems_clean_data_impressions_categorized.csv"))









## -----------------------------------------------------------------------------------------------------------------------------------------------------

## Can be used to confirm impression imputation is working sensibly:

## Remaining complaints
# nrow(impressions_categorized_imputed %>% filter(possible_impression_category == "missing"))
# 
# View(impressions_categorized_imputed %>%
#        filter(possible_impression_category == "missing") %>%
#        group_by(incident_complaint_reported_by_dispatch) %>%
#        count())

## Sanity check to ensure results seem sensible
## The two output tables below should have very similar (within rounding) frequencies for each possible_impression_category
# check_results <- impressions_categorized_imputed %>%
#   filter(str_detect(incident_complaint_reported_by_dispatch, "falls")) %>%
#   select(possible_impression_category, impression_category, incident_complaint_reported_by_dispatch)
# 
# ## Compare frequencies of complaint dispatches associated with actual impressions:
# impression_complaint_freqs %>%
#   filter(str_detect(incident_complaint_reported_by_dispatch, "falls"), possible_impression_category != "missing") %>%
#   group_by(possible_impression_category) %>%
#   count(wt = n) %>%
#   ungroup() %>%
#   mutate(freq = n / sum(n)) 
# 
# ## to complaint frequencies of complaint dispatches associated with missing impressions:
# check_results %>% filter(impression_category == "missing") %>%
#   group_by(possible_impression_category) %>%
#   count() %>%
#   ungroup() %>%
#   mutate(freq = n / sum(n))

