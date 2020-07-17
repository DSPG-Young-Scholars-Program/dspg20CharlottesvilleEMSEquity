library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(vroom)

# run joining_albermarle_charlottesville.R to get ems data frame
head(new_ems_data)
ems <- new_ems_data

# temp variable
ems$patient_initial_body_temperature_in_fahrenheit
sum(is.na(ems$patient_initial_body_temperature_in_fahrenheit)) # 44624
sum(is.na(ems$patient_last_body_temperature_in_fahrenheit)) # 44624
ems %>%
  filter(is.na(patient_initial_body_temperature_in_fahrenheit) & is.na(patient_last_body_temperature_in_fahrenheit)) %>%
  count() # 44624
# if a patient has an initial temp, they also have a last temp, and vice versa
# filter to just observations where the patient has a temp
has_temp <- ems %>% filter(!is.na(patient_initial_body_temperature_in_fahrenheit))
# convert character to numeric
has_temp$patient_initial_body_temperature_in_fahrenheit <- as.numeric(has_temp$patient_initial_body_temperature_in_fahrenheit)
t0 <- has_temp$patient_initial_body_temperature_in_fahrenheit

# some really low temps
summary(t0)

# a bunch of 36.39s, which could be converted to F most likely
# a 50 which would equal 122 degree F? not sure which is more likely
sort(t0, decreasing = FALSE)

# drop temps less than 80 for now
has_temp <- has_temp %>% filter(patient_initial_body_temperature_in_fahrenheit > 80)
nrow(has_temp) # 5278
t0 <- has_temp$patient_initial_body_temperature_in_fahrenheit
hist(t0, breaks = 20)

head(has_temp$incident_complaint_reported_by_dispatch)
levels(as.factor(has_temp$incident_complaint_reported_by_dispatch))
# incident complaint reported by dispatch values that could be related to covid

dispatch_complaint_potential_covid_list <- c("Breathing Problem", "Headache", "Sick Person")
# filter has temp so that only those with these dispatch complaints present
plot_data <- has_temp %>%
  filter(incident_complaint_reported_by_dispatch %in% dispatch_complaint_potential_covid_list) %>%
  filter(ymd(incident_date) > "2020-01-01")

# plot_data <- ems %>%
#   filter(!is.na(patient_change_pulse_oximetry) & !is.na(incident_date)) %>%
#   filter(ymd(incident_date) > "2020-01-01") %>%
#   filter(patient_change_pulse_oximetry < 15 & patient_change_pulse_oximetry > -15)
ggplot(plot_data, aes(patient_initial_body_temperature_in_fahrenheit)) + geom_histogram()


ggplot(data = plot_data, mapping = aes(x = incident_date, y = patient_initial_body_temperature_in_fahrenheit)) +
  geom_point()

ems$dispatch_complaint_potenial_covid <- ems$incident_complaint_reported_by_dispatch %in% dispatch_complaint_potential_covid_list

# PLOT: temp colored by dispatch complaint type
plot_data <- has_temp %>%
  filter(ymd(incident_date) > "2020-01-01") %>%
  select(incident_date, patient_initial_body_temperature_in_fahrenheit, dispatch_complaint_potenial_covid)

ggplot(data = plot_data,
       mapping = aes(x = incident_date,
                     y = patient_initial_body_temperature_in_fahrenheit,
                     color = dispatch_complaint_potenial_covid)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)

# PLOT:
ems %>%
  filter(!is.na(patient_initial_body_temperature_in_fahrenheit)) %>%
  filter(patient_initial_body_temperature_in_fahrenheit > 80) %>%
  filter(ymd(incident_date) > "2020-01-01") %>%
  ggplot(., aes(x = incident_date, y = patient_initial_body_temperature_in_fahrenheit, color = dispatch_complaint_potenial_covid)) +
    geom_point()

# pulse ox variable
ems$patient_initial_pulse_oximetry
head(ems$patient_initial_pulse_oximetry)
sum(is.na(ems$patient_initial_pulse_oximetry))
hist(ems$patient_initial_pulse_oximetry, breaks = 20)


# scatter of incident date and initial pulse oximetry
plot_data <- ems %>%
  filter(!is.na(patient_initial_pulse_oximetry & !is.na(incident_date)))

ggplot(data = plot_data, mapping = aes(x = incident_date, y = patient_initial_pulse_oximetry)) +
  geom_point()

# scatter of incident date and last pulse ox
sum(is.na(ems$patient_last_pulse_oximetry))
plot_data <- ems %>%
  filter(!is.na(patient_last_pulse_oximetry & !is.na(incident_date)))

ggplot(data = plot_data, mapping = aes(x = incident_date, y = patient_last_pulse_oximetry)) +
  geom_point()

# compute difference between initial and last pulse ox
ems$patient_change_pulse_oximetry <- ems$patient_last_pulse_oximetry - ems$patient_initial_pulse_oximetry
sum(is.na(ems$patient_change_pulse_oximetry))
hist(ems$patient_change_pulse_oximetry, breaks = 20)

class(ems$incident_date)
head(ems$incident_date)
ymd(ems$incident_date) > "2020-01-01"
ems$incident_date >= 2020

plot_data <- ems %>%
  filter(!is.na(patient_change_pulse_oximetry) & !is.na(incident_date)) %>%
  filter(ymd(incident_date) > "2020-01-01") %>%
  filter(patient_change_pulse_oximetry < 15 & patient_change_pulse_oximetry > -15)

ggplot(data = plot_data, mapping = aes(x = incident_date, y = patient_change_pulse_oximetry)) +
  geom_point()

# PLOT: pulse ox and incident date by patients who have covid-like dispatch complaints
ems %>%
  filter(!is.na(patient_change_pulse_oximetry)) %>%
  filter(ymd(incident_date) > "2020-01-01") %>%
  filter(dispatch_complaint_potenial_covid) %>%
  ggplot(., aes(x = incident_date, y = patient_change_pulse_oximetry)) +
    geom_point()

# PLOT: pulse ox and incident date by patients who don't have covid-like dispatch complaints
ems %>%
  filter(!is.na(patient_change_pulse_oximetry)) %>%
  filter(ymd(incident_date) > "2020-01-01") %>%
  filter(!dispatch_complaint_potenial_covid) %>%
  ggplot(., aes(x = incident_date, y = patient_change_pulse_oximetry)) +
    geom_point()

# PLOT: pulse ox and incident date colored by dispatch type potential covid
ems %>%
  filter(!is.na(patient_change_pulse_oximetry)) %>%
  filter(ymd(incident_date) > "2020-01-01") %>%
  ggplot(., aes(x = incident_date, y = patient_change_pulse_oximetry, color = dispatch_complaint_potenial_covid)) +
    geom_point()

# grouping by different complaints
# create possible covid indicator--look at complaint, temp, pulse ox

# look at percentage of calls that are "potential covid"
ems %>%
  ggplot(., aes(x = incident_date, y = dispatch_complaint_potenial_covid)) +
  geom_bar(stat = "identity")



ems$incident_month <- month(ems$incident_date)
sum(is.na(ems$incident_complaint_reported_by_dispatch)) # 202

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# 2019
tmp <- ems %>%
  filter(ymd(incident_date) >= "2019-01-01" & ymd(incident_date) < "2020-01-01") %>%
  group_by(incident_month, dispatch_complaint_potenial_covid) %>%
  count()

tmp %>%
  ggplot(., aes(x = incident_month, y = n, fill = dispatch_complaint_potenial_covid)) +
    geom_bar(stat = "identity", position = "fill") +
    labs(title = "City of Charlottesville Incidents in 2019",
         y = "Number of Incidents") +
    theme(axis.title.x = element_blank()) +
    scale_x_continuous(breaks = 1:12, labels = months)


# 2020
tmp <- ems %>%
  filter(ymd(incident_date) >= "2020-01-01") %>%
  group_by(incident_month, dispatch_complaint_potenial_covid) %>%
  count()

tmp %>%
  ggplot(., aes(x = incident_month, y = n, fill = dispatch_complaint_potenial_covid)) +
  geom_bar(stat = "identity", position = "fill") +

  labs(title = "City of Charlottesville Incidents in 2020",
       y = "Number of Incidents",
       x = "Month of Incident") +
  theme(axis.title.x = element_blank()) +
  scale_x_continuous(breaks = 1:6, labels = months[1:6])

# looking at provider primary impression
head(ems$situation_provider_primary_impression_code_and_description)
levels(as.factor(ems$situation_provider_primary_impression_code_and_description))
sum(is.na(ems$situation_provider_primary_impression_code_and_description)) # 12445
related_to_covid <- c("")
sum(is.na(ems$situation_primary_complaint_statement_list))

ems %>%
  filter(situation_provider_primary_impression_code_and_description == "Infectious - Coronavirus (B97.21)") %>%
  count() # 13

ems %>%
  filter(situation_provider_secondary_impression_description_and_code == "Infectious - Coronavirus (B97.21)") %>%
  count() # 14

levels(as.factor(ems$situation_provider_secondary_impression_description_and_code))

# plot covid cases
covid <- ems %>%
  filter(situation_provider_primary_impression_code_and_description == "Infectious - Coronavirus (B97.21)" |
           str_detect(situation_provider_secondary_impression_description_and_code_list, "Infectious - Coronavirus (B97.21)"))
nrow(covid)

# convert to lower case
ems$situation_primary_complaint_statement_list <- tolower(ems$situation_primary_complaint_statement_list)
ems$situation_secondary_complaint_statement_list <- tolower(ems$situation_secondary_complaint_statement_list)

head(covid$situation_primary_complaint_statement_list)
levels(as.factor(covid$situation_primary_complaint_statement_list))
covid_symptoms <- c("altered mental status", "chills", "shaking", "cough", "fever", "headache",
                    "nausea", "shortness of breath", "throat pain", "vomiting", "difficulty breathing")
pattern <- c("altered mental status|chills|shaking|cough|fever|headache|nausea|shortness of breath|throat pain|vomiting|difficulty breathing")

potential_covid <- ems[str_detect(ems$situation_primary_complaint_statement_list, pattern), ]
covid2020 <- potential_covid %>%
  filter(ymd(incident_date) > "2020-01-01")
hist(covid2020$incident_date, breaks = 20)

ems$potential_covid <- (str_detect(ems$situation_primary_complaint_statement_list, pattern))

tmp <- ems %>%
  filter(ymd(incident_date) > "2020-01-01") %>%
  filter(!is.na(potential_covid)) %>%
  group_by(incident_month, potential_covid) %>%
  count()

tmp %>%
  ggplot(., aes(x = incident_month, y = n, fill = potential_covid)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "City of Charlottesville Incidents in 2020",
       y = "Number of Incidents") +
  theme(axis.title.x = element_blank()) +
  scale_x_continuous(breaks = 1:6, labels = months[1:6])

plot_data <- ems %>%
  group_by(mo_yr, potential_covid) %>%
  count()

plot_data %>%
  filter(potential_covid) %>%
  ggplot(., aes(x = mo_yr, y = n)) +
  geom_line(color = "red") +
  geom_point(stat = "identity")

# create month year variable
ems <- ems %>%
  mutate(incident_date = ymd(incident_date),
         yr = as.factor(year(incident_date)),
         mo = as.factor(month(incident_date)),
         dy = as.character(day(incident_date)),
         mo_yr = dmy(paste0("01-", mo, "-", yr)))

# look at covid-like symptoms by race
ems$patient_race_first_listed = gsub("^(.*?),.*", "\\1", ems$patient_race_list)
nrow(ems) # 85446

