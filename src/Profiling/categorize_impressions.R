
library(here)
library(dplyr)
library(stringr)
library(tidyr)

ems_data <- vroom::vroom(here("data", "working", "first_unit_at_scene_data.csv"))

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

## Function to categorize some NA impressions using associated values from the dispatch complaint.
## Not comprehensive at all - just trying to capture the low-hanging fruit given the timeframe we're working with right now.
# categorize_missing_impressions <- function(x) {
#   trimws(case_when(str_detect(x, "traffic|transport|injury|falls" ~ "injury"),
#                    str_detect(x, "breath" ~ "respiratory")))
#                    
#                    
#                    
#                    
#                    
# }

## Categorize impressions based on above mappings
ems_impressions_categorized <- ems_data %>% 
  mutate(primary_impression_list = str_split(situation_provider_primary_impression_code_and_description, "\\|"), ## split multiple impression cases
         primary_impression_category_list = purrr::map(primary_impression_list, categorize_impressions), ## categorize all impressions
         primary_impression_category_collapsed = purrr::map(primary_impression_category_list, unique), ## check if multiple impression cases have the same category
         patient_race_trimmed = str_extract(patient_race_list, "[^|]*")) %>% ## pull out first value of race list where there are multiples
  filter(lengths(primary_impression_category_collapsed) == 1) %>% ## remove remaining cases (~100) where impressions gave conflicting categories
  mutate(primary_impression_category = as.character(primary_impression_category_collapsed))


## Should do the same for secondary impressions? There are some cases with missing primary and filled secondary:
secondary_impressions_categorized <- ems_impressions_categorized %>% 
  filter(primary_impression_category == "missing", !is.na(situation_provider_secondary_impression_description_and_code_list)) %>%
  mutate(secondary_impression_list = str_split(situation_provider_secondary_impression_description_and_code_list, "\\|"), ## split multiple impression cases
         secondary_impression_category_list = purrr::map(secondary_impression_list, categorize_impressions), ## categorize all impressions
         secondary_impression_category_collapsed = purrr::map(secondary_impression_category_list, unique)) %>% ## check if multiple impressions have the same category
  filter(lengths(secondary_impression_category_collapsed) == 1) %>% ## remove remaining cases (~40) where impressions gave conflicting categories
  mutate(secondary_impression_category = as.character(secondary_impression_category_collapsed))

## create single column incorporating both primary and secondary categories (when applicable)
impressions_categorized_full <- left_join(ems_impressions_categorized, secondary_impressions_categorized) %>%
  mutate(impression_category = ifelse(is.na(secondary_impression_category), primary_impression_category, secondary_impression_category)) 



readr::write_csv(impressions_categorized_full, here::here("data", "working", "first_unit_at_scene_impressions_categorized.csv"))

## Add new column to populate with possible impression categories
impressions_categorized_full <- impressions_categorized_full %>%
  mutate(possible_impression_category = impression_category)

## Take a look to see how complaints and impressions compare:
tmp <- impressions_categorized_full %>%
  select(incident_complaint_reported_by_dispatch, possible_impression_category, situation_provider_primary_impression_code_and_description) %>%
  group_by(possible_impression_category, incident_complaint_reported_by_dispatch) %>%
  count()

## Total amount of "falls" complaints associated with missing impressions
allocate_complaints <- function(full_data, grouped_data, complaint_string) {
  
  smart_round <- function(x) {
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y
  }
  
  complaint_N <- grouped_data %>% 
    filter(str_detect(incident_complaint_reported_by_dispatch, complaint_string), possible_impression_category == "missing") %>% 
    ungroup() %>% 
    summarize(N = sum(n))
  
  complaint_N <- complaint_N$N
  
  complaint_impression_freq <- grouped_data %>% 
    filter(str_detect(incident_complaint_reported_by_dispatch, complaint_string), possible_impression_category != "missing") %>%
    group_by(possible_impression_category) %>%
    count(wt = n) %>%
    ungroup() %>%
    mutate(freq = n / sum(n), 
           allocated = smart_round(complaint_N * freq))
  
  possible_categories <- rep(complaint_impression_freq$possible_impression_category, complaint_impression_freq$allocated)
  
  ## Need to add case where if the impression_category is not missing, fill possible impressionc ategory with that value
  possible_impressions_categorized <- full_data %>%
    mutate(possible_impression_category = ifelse( (str_detect(incident_complaint_reported_by_dispatch, complaint_string) & possible_impression_category == "missing"), possible_categories, NA))
  
  return(possible_impressions_categorized)
  
}

## Need to join without filling missing values back in from possible impression
test <- allocate_complaints(full_data = impressions_categorized_full, grouped_data = tmp, complaint_string = "falls")

test %>% select(impression_category, possible_impression_category) %>% filter(possible_impression_category != "missing")

## How to then do for another string?

