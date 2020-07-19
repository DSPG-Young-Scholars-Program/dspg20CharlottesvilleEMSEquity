library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(vroom)
library(stringr)
library(naniar)

# load data
ems <- read.csv('./data/working/ems_clean_data.csv');

ems <- ems %>% mutate(incident_date = as_date(incident_date));

# create a variable for suspected covid cases according to provider primary or secondary impression
primary_predicate <- ems$situation_provider_primary_impression_code_and_description == "infectious - coronavirus (b97.21)";
sum(primary_predicate, na.rm = TRUE) # 19

secondary_predicate <- str_detect(ems$situation_provider_secondary_impression_description_and_code_list, "b97.21");
sum(secondary_predicate, na.rm = TRUE) # 19

ems$provider_suspected_covid <- primary_predicate | secondary_predicate;
sum(ems$provider_suspected_covid, na.rm = TRUE) # 38

# change from TRUE/FALSE to yes/no and convert to factor
ems <- ems %>% mutate(provider_suspected_covid = ifelse(provider_suspected_covid, "yes", "no"))
ems$provider_suspected_covid <- as.factor(ems$provider_suspected_covid)
# replace NA with no
ems$provider_suspected_covid[is.na(ems$provider_suspected_covid)] <- "no"

# checks
# ems %>% filter(provider_suspected_covid == "no") %>% count()
# sum(is.na(ems$provider_suspected_covid))

# symptom probabilities in symptomatic patients
# cough 0.84
# fever 0.80
# myalgia 0.63
# chills 0.63
# fatigue 0.62
# headache 0.59
# shortness of breath 0.57
# diarrhea 0.38
# vomiting 0.13

# create covid indicator
## create cough variable
has_cough <- str_detect(ems$situation_primary_complaint_statement_list, "cough")
has_cough[is.na(has_cough)] <- FALSE # remove NAs
ems$cough <- ifelse(has_cough, 1, 0)
sum(ems$cough) # 293

## create fever variable
has_fever <- str_detect(ems$situation_primary_complaint_statement_list, "fever") |
  ems$patient_initial_body_temperature_in_fahrenheit >= 100.4 |
  ems$patient_last_body_temperature_in_fahrenheit >= 100.4
has_fever[is.na(has_fever)] <- FALSE
ems$fever <- ifelse(has_fever, 1, 0)
sum(ems$fever) # 759

## create myalgia variable
has_myalgia <- str_detect(ems$situation_primary_complaint_statement_list, "myalgia|muscle pain|muscle ache")
has_myalgia[is.na(has_myalgia)] <- FALSE
ems$myalgia <- ifelse(has_myalgia, 1, 0)
sum(ems$myalgia) # 7

## create chills variable
has_chills <- str_detect(ems$situation_primary_complaint_statement_list, "chills")
has_chills[is.na(has_chills)] <- FALSE
ems$chills <- ifelse(has_chills, 1, 0)
sum(ems$chills) # 86

## create fatigue variable
has_fatigue <- str_detect(ems$situation_primary_complaint_statement_list, "fatigue")
has_fatigue[is.na(has_fatigue)] <- FALSE
ems$fatigue <- ifelse(has_fatigue, 1, 0)
sum(ems$fatigue) # 89

## create headache variable
has_headache <- str_detect(ems$situation_primary_complaint_statement_list, "headache")
has_headache[is.na(has_headache)] <- FALSE
ems$headache <- ifelse(has_headache, 1, 0)
sum(ems$headache) # 1049

## shortness of breath
has_shortness_of_breath <- str_detect(ems$situation_primary_complaint_statement_list, "shortness of breath|sob|short of breath|trouble breathing|difficulty breathing")
has_shortness_of_breath[is.na(has_shortness_of_breath)] <- FALSE
ems$shortness_of_breath <- ifelse(has_shortness_of_breath, 1, 0)
sum(ems$shortness_of_breath) # 1225

## diarrhea
has_diarrhea <- str_detect(ems$situation_primary_complaint_statement_list, "diarrhea")
has_diarrhea[is.na(has_diarrhea)] <- FALSE
ems$diarrhea <- ifelse(has_diarrhea, 1, 0)
sum(ems$diarrhea) # 268

## vomit
has_vomit <- str_detect(ems$situation_primary_complaint_statement_list, "vomit")
has_vomit[is.na(has_vomit)] <- FALSE
ems$vomit <- ifelse(has_vomit, 1, 0)
sum(ems$vomit) # 1132

## covid indicator
# symptom probability, taken from https://www.cdc.gov/mmwr/volumes/69/wr/pdfs/mm6928a2-H.pdf
# cough 0.84
# fever 0.80
# myalgia 0.63
# chills 0.63
# fatigue 0.62
# headache 0.59
# shortness of breath 0.57
# diarrhea 0.38
# vomiting 0.13
max_score <- 0.84 + 0.80 + 0.63 + 0.63 + 0.62 + 0.59 + 0.57 + 0.38 + 0.13
ems <- ems %>% mutate(
  prob_covid_indicator = (0.84 * cough + 0.80 * fever + 0.63 * myalgia + 0.63 * chills + 0.62 * fatigue +
    0.59 * headache + 0.57 * shortness_of_breath + 0.38 * diarrhea + 0.13 * vomit) / max_score,
);
summary(ems$prob_covid_indicator)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00000 0.00000 0.00000 0.03506 0.00000 2.27000
sum(ems$prob_covid_indicator > 0) # 4678 patients whose covid indicator is greater than 0
pos_score <- ems %>% filter(prob_covid_indicator > 0)
hist(pos_score$prob_covid_indicator, breaks = 20)

# more questions
# does this indicator align with provider impressions?
ems %>%
  filter(provider_suspected_covid == "yes" & prob_covid_indicator > 0) %>%
  count() # only 24 of 38

# look at patients / their symptoms who providers thought might have covid
# covid_impression <- ems %>%
#   filter(provider_suspected_covid == "yes") %>%
#   select(prob_covid_indicator,
#          situation_provider_primary_impression_code_and_description,
#          situation_provider_secondary_impression_description_and_code_list,
#          situation_primary_complaint_statement_list,
#          situation_secondary_complaint_statement_list)

# what is the average score for patients who the provider thought had covid?

# select first race listed
ems$patient_first_race_listed <- gsub("^(.*?)\\|.*", "\\1", ems$patient_race_list);
ems <- ems %>% replace_with_na(replace = list(patient_first_race_listed = c("Not Recorded", "Not Applicable")));

# compute average prob_covid_indicator by race
cutoff <- ymd("2020-02-01")

race_counts <- ems %>%
  filter(incident_date >= cutoff) %>%
  group_by(patient_first_race_listed) %>%
  count() %>%
  rename(total = n)

ems %>%
  filter(incident_date >= cutoff) %>%
  filter(prob_covid_indicator > 0) %>%
  group_by(patient_first_race_listed) %>%
  summarise(mean = mean(prob_covid_indicator), n = n()) %>%
  inner_join(race_counts, by = "patient_first_race_listed") %>%
  mutate(rate_per_100 = (n / total) * 100)


# counts suspected covid by race
ems %>%
  filter(incident_date >= cutoff) %>%
  filter(provider_suspected_covid == "yes") %>%
  group_by(patient_first_race_listed, provider_suspected_covid) %>%
  summarise(n = n()) %>%
  inner_join(race_counts, by = "patient_first_race_listed") %>%
  mutate(rate_per_100 = (n / total) * 100)

# OLD CODE, DELETE
# run joining_albermarle_charlottesville.R to get ems data frame
# head(new_ems_data)
# ems <- new_ems_data
# # look into potential covid
# ems %>%
#   filter(situation_provider_primary_impression_code_and_description == "Infectious - Coronavirus (B97.21)") %>%
#   count()
#
# # look at incidents that have "potential covid" cases
# covid_symptoms <- c("altered mental status", "chills", "shaking", "cough", "fever", "headache",
#                     "nausea", "shortness of breath", "throat pain", "vomiting", "difficulty breathing")
# pattern <- c("altered mental status|chills|shaking|cough|fever|headache|nausea|shortness of breath|throat pain|vomiting|difficulty breathing")
#
# potential_covid <- ems[str_detect(ems$situation_primary_complaint_statement_list, pattern), ]
# nrow(potential_covid) # 24924
#
#
# ems$covid_like_symptoms <- (str_detect(ems$situation_primary_complaint_statement_list, pattern))
#
# sum(is.na(ems$incident_psap_call_date_time))
# # create month year variable
# ems <- ems %>%
#   mutate(incident_date = ymd(incident_date),
#          yr = as.factor(year(incident_date)),
#          mo = as.factor(month(incident_date)),
#          dy = as.character(day(incident_date)),
#          mo_yr = dmy(paste0("01-", mo, "-", yr)))
#
# plot_data <- ems %>%
#   group_by(mo_yr, potential_covid) %>%
#   count()
#
# plot_data %>%
#   filter(potential_covid) %>%
#   ggplot(., aes(x = mo_yr, y = n)) +
#   geom_line(color = "red") +
#   geom_point(stat = "identity")
#
#
#
#
# covid <- str_detect(ems$situation_provider_secondary_impression_description_and_code_list, "Infectious - Coronavirus (B97.21)")
# sum(is.na(covid)) # 55161
# sum(covid, na.rm = TRUE) # 1715
# infections <- ems %>%
#   filter(covid)
#
# levels(as.factor(infections$situation_provider_secondary_impression_description_and_code_list))
#
# length(covid)
# length(covid[covid == FALSE])
# levels(as.factor(ems$situation_provider_secondary_impression_description_and_code_list))
