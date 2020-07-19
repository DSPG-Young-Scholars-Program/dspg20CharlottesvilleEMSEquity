
library(here)
library(dplyr)
library(stringr)
library(tidyr)

ems_data <- readr::read_csv(here("data", "working", "ems_clean_data.csv"))

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
                   str_detect(x, "enviro|deydration") ~ "environment",
                   str_detect(x, "pain") ~ "pain",
                   !is.na(x) ~ "other",
                   TRUE ~ "missing"))
}

## Categorize impressions based on above mappings
ems_impressions_categorized <- ems_data %>% 
  mutate(impression_list = str_split(situation_provider_primary_impression_code_and_description, "\\|"), ## split multiple impression cases
         impression_category_list = purrr::map(impression_list, categorize_impressions), ## categorize all impressions
         impression_category_collapsed = purrr::map(impression_category_list, unique), ## check if multiple impression cases have the same category
         patient_race_trimmed = str_extract(patient_race_list, "[^|]*")) %>% ## pull out first value of race list where there are multiples
  filter(lengths(impression_category_collapsed) == 1) %>% ## remove remaining cases (~100) where impressions gave conflicting categories
  mutate(impression_category = as.character(impression_category_collapsed))
