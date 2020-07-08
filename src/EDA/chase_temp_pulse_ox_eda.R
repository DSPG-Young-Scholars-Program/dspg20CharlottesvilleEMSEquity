library(lubridate)
library(ggplot2)

ems <- charlottesville_renamed

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
    geom_bar(stat = "identity", position = "identity") +
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
  geom_bar(stat = "identity", position = "stack") +

  labs(title = "City of Charlottesville Incidents in 2020",
       y = "Number of Incidents",
       x = "Month of Incident") +
  theme(axis.title.x = element_blank()) +
  scale_x_continuous(breaks = 1:6, labels = months[1:6])
