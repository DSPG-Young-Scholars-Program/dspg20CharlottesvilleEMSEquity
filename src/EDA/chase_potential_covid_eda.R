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

# symptom probabilities in symptomatic patients, could be used in weighting scheme
# cough 0.84
# fever 0.80
# myalgia 0.63
# chills 0.63
# fatigue 0.62
# headache 0.59
# shortness of breath 0.57
# diarrhea 0.38
# vomiting 0.13

# function to check for symptoms in primary complaint
create_symptom <- function(pattern, var) {
  has_symptom <- str_detect(var, pattern);
  has_symptom[is.na(has_symptom)] <- FALSE;
  symptom <- ifelse(has_symptom, 1, 0);
  return(symptom);
}

## create variable for covid impressions
ems$covid_in_primary_impression <- create_symptom(coll("infectious - coronavirus (b97.21)"), ems$situation_provider_primary_impression_code_and_description);
# sum(ems$covid_in_primary_impression) # 19

ems$covid_in_secondary_impression <- create_symptom(coll("infectious - coronavirus (b97.21)"), ems$situation_provider_secondary_impression_description_and_code_list);
# sum(ems$covid_in_secondary_impression) # 19

ems$covid_in_impressions <- ifelse(ems$covid_in_primary_impression |
                                     ems$covid_in_secondary_impression, 1, 0);
sum(ems$covid_in_impressions) # 38
## end covid in impressions


## create cough variable
# from primary
ems$cough_in_primary_complaint <- create_symptom("cough", ems$situation_primary_complaint_statement_list);
# sum(ems$cough_in_primary_complaint) # 293

ems$cough_in_secondary_complaint <- create_symptom("cough", ems$situation_secondary_complaint_statement_list);
# sum(ems$cough_in_secondary_complaint) # 2

# combine cough vars
ems$cough <- ifelse(ems$cough_in_primary_complaint | ems$cough_in_secondary_complaint, 1, 0);
sum(ems$cough) # 295
## end cough

## create fever variable
# for fever in temp
has_fever_in_temp <- ems$patient_initial_body_temperature_in_fahrenheit >= 100.4 |
  ems$patient_last_body_temperature_in_fahrenheit >= 100.4
has_fever_in_temp[is.na(has_fever_in_temp)] <- FALSE
ems$fever_in_temp <- ifelse(has_fever_in_temp, 1, 0)

# for fever in complaints
# in primary complaint
ems$fever_in_primary_complaint <- create_symptom("fever", ems$situation_primary_complaint_statement_list);
# sum(ems$fever_in_primary_complaint) # 382

ems$fever_in_secondary_complaint <- create_symptom("fever", ems$situation_secondary_complaint_statement_list);
# sum(ems$fever_in_secondary_complaint) # 4

ems$fever_in_primary_impression <- create_symptom("fever", ems$situation_provider_primary_impression_code_and_description);
# sum(ems$fever_in_primary_impression) # 307

ems$fever_in_secondary_impression <- create_symptom("fever", ems$situation_provider_secondary_impression_description_and_code_list);
# sum(ems$fever_in_secondary_impression) # 168

# combine fever vars
ems$fever <- ifelse(ems$fever_in_temp |
                      ems$fever_in_primary_complaint |
                      ems$fever_in_secondary_complaint |
                      ems$fever_in_primary_impression |
                      ems$fever_in_secondary_impression, 1, 0);
sum(ems$fever) # 974
## end fever

## create myalgia variable
# in primary complaint
ems$myalgia_in_primary_complaint <- create_symptom("myalgia|muscle pain|muscle ache", ems$situation_primary_complaint_statement_list);
# sum(ems$myalgia_in_primary_complaint) # 7

# in secondary complaint
ems$myalgia_in_secondary_complaint <- create_symptom("myalgia|muscle pain|muscle ache", ems$situation_secondary_complaint_statement_list);
# sum(ems$myalgia_in_secondary_complaint) # 1

# combine myalgia vars
ems$myalgia <- ifelse(ems$myalgia_in_primary_complaint | ems$myalgia_in_secondary_complaint, 1, 0);
sum(ems$myalgia) # 8
## end myalgia


## create chills variable
# in primary complaint
ems$chills_in_primary_complaint <- create_symptom("chills", ems$situation_primary_complaint_statement_list);
# sum(ems$chills_in_primary_complaint) # 86

# in secondary complaint
ems$chills_in_secondary_complaint <- create_symptom("chills", ems$situation_secondary_complaint_statement_list);
# sum(ems$chills_in_secondary_complaint) # 1

# combine chills vars
ems$chills <- ifelse(ems$chills_in_primary_complaint | ems$chills_in_secondary_complaint, 1, 0);
sum(ems$chills) # 87
## end chills


## create fatigue variable
ems$fatigue_in_primary_complaint <- create_symptom("fatigue", ems$situation_primary_complaint_statement_list);
# sum(ems$fatigue_in_primary_complaint) # 89

ems$fatigue_in_secondary_complaint <- create_symptom("fatigue", ems$situation_secondary_complaint_statement_list);
# sum(ems$fatigue_in_secondary_complaint) # 1

# combine fatigue vars
ems$fatigue <- ifelse(ems$fatigue_in_primary_complaint | ems$fatigue_in_secondary_complaint, 1, 0)
sum(ems$fatigue) # 90
## end fatigue


## create headache variable
ems$headache_in_primary_complaint <- create_symptom("headache", ems$situation_primary_complaint_statement_list);
# sum(ems$headache_in_primary_complaint) # 1049

ems$headache_in_secondary_complaint <- create_symptom("headache", ems$situation_secondary_complaint_statement_list);
# sum(ems$headache_in_secondary_complaint) # 15

# headache can show up as "neuro - headache (r51)" or "neuro - headache - migraine (g43.9)" so just look for "headache"
ems$headache_in_primary_impression <- create_symptom("headache", ems$situation_provider_primary_impression_code_and_description);
# sum(ems$headache_in_primary_impression) # 653

ems$headache_in_secondary_impression <- create_symptom("headache", ems$situation_provider_secondary_impression_description_and_code_list);
# sum(ems$headache_in_secondary_impression) # 433

# combine headache variables
ems$headache <- ifelse(ems$headache_in_primary_complaint |
                         ems$headache_in_secondary_complaint |
                         ems$headache_in_primary_impression |
                         ems$headache_in_secondary_impression, 1, 0);
sum(ems$headache) # 1549
## end headache


## shortness of breath
ems$sob_in_primary_complaint <- create_symptom("breath|sob", ems$situation_primary_complaint_statement_list);
# sum(ems$sob_in_primary_complaint) # 4106

ems$sob_in_secondary_complaint <- create_symptom("breath|sob", ems$situation_secondary_complaint_statement_list);
# sum(ems$sob_in_secondary_complaint) # 20

# combine sob vars
ems$sob <- ifelse(ems$sob_in_primary_complaint | ems$sob_in_secondary_complaint, 1, 0);
sum(ems$sob) # 4125
## end sob


## diarrhea
ems$diarrhea_in_primary_complaint <- create_symptom("diarrhea", ems$situation_primary_complaint_statement_list);
# sum(ems$diarrhea_in_primary_complaint) # 268

ems$diarrhea_in_secondary_complaint <- create_symptom("diarrhea", ems$situation_secondary_complaint_statement_list);
# sum(ems$diarrhea_in_secondary_complaint) # 3

ems$diarrhea_in_primary_impression <- create_symptom(coll("gi/gu - diarrhea (k59.1)"), ems$situation_provider_primary_impression_code_and_description);
# sum(ems$diarrhea_in_primary_impression) # 236

ems$diarrhea_in_secondary_impression <- create_symptom(coll("gi/gu - diarrhea (k59.1)"), ems$situation_provider_secondary_impression_description_and_code_list);
# sum(ems$diarrhea_in_secondary_impression) # 152


# combine diarrhea vars
ems$diarrhea <- ifelse(ems$diarrhea_in_primary_complaint |
                         ems$diarrhea_in_secondary_complaint |
                         ems$diarrhea_in_primary_impression |
                         ems$diarrhea_in_secondary_impression, 1, 0);
sum(ems$diarrhea) # 493
## end diarrhea


## nausea or vomit
ems$nausea_or_vomit_in_primary_complaint <- create_symptom("nausea|vomit", ems$situation_primary_complaint_statement_list);
# sum(ems$nausea_or_vomit_in_primary_complaint) # 1855

ems$nausea_or_vomit_in_secondary_complaint <- create_symptom("nausea|vomit", ems$situation_secondary_complaint_statement_list);
# sum(ems$nausea_or_vomit_in_secondary_complaint) # 34

ems$nausea_or_vomit_in_medicine <- create_symptom("zofran", ems$patient_medication_given_description_and_rxcui_codes_list);
# sum(ems$nausea_or_vomit_in_medicine) # 2532

ems$nausea_or_vomit_in_primary_impression <- create_symptom("nausea", ems$situation_provider_primary_impression_code_and_description);
# sum(ems$nausea_or_vomit_in_primary_impression) # 1133

ems$nausea_or_vomit_in_secondary_impression <- create_symptom("nausea", ems$situation_provider_secondary_impression_description_and_code_list);
# sum(ems$nausea_or_vomit_in_secondary_impression) # 891

# combine vomit variables
ems$nausea_or_vomit <- ifelse(ems$nausea_or_vomit_in_primary_complaint |
                                ems$nausea_or_vomit_in_secondary_complaint |
                                ems$nausea_or_vomit_in_medicine |
                                ems$nausea_or_vomit_in_primary_impression |
                                ems$nausea_or_vomit_in_secondary_impression, 1, 0);
# sum(ems$nausea_or_vomit) # 44660
## end nausea or vomit


## hypoxemia
# create var for when pulse oximetry is less than 94
has_hypoxemia <- ems$patient_initial_pulse_oximetry <= 94 | ems$patient_last_pulse_oximetry <= 94;
has_hypoxemia[is.na(has_hypoxemia)] <- FALSE;
ems$hypoxemia <- ifelse(has_hypoxemia, 1, 0);
# sum(ems$hypoxemia) # 12332

# create variable for when given oxygen
ems$given_oxygen <- create_symptom("oxygen", ems$patient_medication_given_description_and_rxcui_codes_list);
# sum(ems$given_oxygen) # 6374

# create hypoxemia var for when there's no improvement after oxygen
has_hypoxemia_no_improvement <- ems$given_oxygen == 1 &
  (ems$patient_last_pulse_oximetry <= ems$patient_initial_pulse_oximetry)
has_hypoxemia_no_improvement[is.na(has_hypoxemia_no_improvement)] <- FALSE;
ems$hypoxemia_no_improvement <- ifelse(has_hypoxemia_no_improvement, 1, 0);
# sum(ems$hypoxemia_no_improvement) # 2573
## end hypoxemia


## cyanosis
# get levels of skin assessment findings
# unique(unlist(str_split(ems$patient_medication_given_description_and_rxcui_codes_list, "\\|"))) %>% sort()# look at possible values in skin assessment
ems$cyanosis <- create_symptom("cyanotic", ems$patient_skin_assessment_findings_list);
# sum(ems$cyanosis) # 78
## end cyanosis


## COMBINE VARIABLES TO CREATE COVID INDICATOR
weights <- rep(1, 13) # could replace later on with customized weights
ems <- ems %>% mutate(
  covid_indicator = weights[1] * cough +
                    weights[2] * fever +
                    weights[3] * sob +
                    weights[4] * myalgia +
                    weights[5] * chills +
                    weights[6] * fatigue +
                    weights[7] * headache +
                    weights[8] * diarrhea +
                    weights[9] * nausea_or_vomit +
                    weights[10] * hypoxemia +
                    weights[11] * hypoxemia_no_improvement +
                    weights[12] * cyanosis +
                    weights[13] * covid_in_impressions
)

# look at number of cases at each level
sum(ems$covid_indicator > 0) # 20731
sum(ems$covid_indicator > 1) # 5247
sum(ems$covid_indicator > 2) # 706
sum(ems$covid_indicator > 3) # 52

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
# max_score <- 0.84 + 0.80 + 0.63 + 0.63 + 0.62 + 0.59 + 0.57 + 0.38 + 0.13
# ems <- ems %>% mutate(
#   prob_covid_indicator = (0.84 * cough + 0.80 * fever + 0.63 * myalgia + 0.63 * chills + 0.62 * fatigue +
#     0.59 * headache + 0.57 * shortness_of_breath + 0.38 * diarrhea + 0.13 * vomit) / max_score,
# );
# summary(ems$prob_covid_indicator)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# # 0.00000 0.00000 0.00000 0.03506 0.00000 2.27000
# sum(ems$prob_covid_indicator > 0) # 4678 patients whose covid indicator is greater than 0
# pos_score <- ems %>% filter(prob_covid_indicator > 0)
# hist(pos_score$prob_covid_indicator, breaks = 20)
#
# # more questions
# # does this indicator align with provider impressions?
# ems %>%
#   filter(provider_suspected_covid == "yes" & prob_covid_indicator > 0) %>%
#   count() # only 24 of 38
#
# # look at patients / their symptoms who providers thought might have covid
# # covid_impression <- ems %>%
# #   filter(provider_suspected_covid == "yes") %>%
# #   select(prob_covid_indicator,
# #          situation_provider_primary_impression_code_and_description,
# #          situation_provider_secondary_impression_description_and_code_list,
# #          situation_primary_complaint_statement_list,
# #          situation_secondary_complaint_statement_list)
#
# # what is the average score for patients who the provider thought had covid?
#
# # select first race listed
# ems$patient_first_race_listed <- gsub("^(.*?)\\|.*", "\\1", ems$patient_race_list);
# ems <- ems %>% replace_with_na(replace = list(patient_first_race_listed = c("Not Recorded", "Not Applicable")));
#
# # compute average prob_covid_indicator by race
# cutoff <- ymd("2020-02-01")
#
# race_counts <- ems %>%
#   filter(incident_date >= cutoff) %>%
#   group_by(patient_first_race_listed) %>%
#   count() %>%
#   rename(total = n)
#
# ems %>%
#   filter(incident_date >= cutoff) %>%
#   filter(prob_covid_indicator > 0) %>%
#   group_by(patient_first_race_listed) %>%
#   summarise(mean = mean(prob_covid_indicator), n = n()) %>%
#   inner_join(race_counts, by = "patient_first_race_listed") %>%
#   mutate(rate_per_100 = (n / total) * 100)
#
#
# # counts suspected covid by race
# ems %>%
#   filter(incident_date >= cutoff) %>%
#   filter(provider_suspected_covid == "yes") %>%
#   group_by(patient_first_race_listed, provider_suspected_covid) %>%
#   summarise(n = n()) %>%
#   inner_join(race_counts, by = "patient_first_race_listed") %>%
#   mutate(rate_per_100 = (n / total) * 100)
#
# # provider primary impression counts
# pri_imps <- unique(unlist(str_split(ems$situation_provider_primary_impression_code_and_description, "\\|"))) %>% sort()
# sec_imps <- unique(unlist(str_split(ems$situation_provider_secondary_impression_description_and_code_list, "\\|"))) %>% sort()
#
# impressions <- unique(c(pri_imps, sec_imps))
#
# primary_counts <- sapply(impressions, function(impression) {
#   count <- sum(str_detect(ems$situation_provider_primary_impression_code_and_description, coll(impression)), na.rm = TRUE);
#   return(count);
# });
#
# secondary_counts <- sapply(impressions, function(impression) {
#   count <- sum(str_detect(ems$situation_provider_secondary_impression_description_and_code_list, coll(impression)), na.rm = TRUE);
#   return(count);
# });
#
# impression_landscape <- tibble(
#   impression = impressions,
#   primary_count = primary_counts,
#   secondary_count = secondary_counts,
# );
#
# View(impression_landscape)
# write.csv("impression_landscape.csv")




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
