library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(vroom)
library(stringr)

# run joining_albermarle_charlottesville.R to get ems data frame
head(new_ems_data)
ems <- new_ems_data
# look into potential covid
ems %>%
  filter(situation_provider_primary_impression_code_and_description == "Infectious - Coronavirus (B97.21)") %>%
  count()

# look at incidents that have "potential covid" cases
covid_symptoms <- c("altered mental status", "chills", "shaking", "cough", "fever", "headache",
                    "nausea", "shortness of breath", "throat pain", "vomiting", "difficulty breathing")
pattern <- c("altered mental status|chills|shaking|cough|fever|headache|nausea|shortness of breath|throat pain|vomiting|difficulty breathing")

potential_covid <- ems[str_detect(ems$situation_primary_complaint_statement_list, pattern), ]
nrow(potential_covid) # 24924


ems$covid_like_symptoms <- (str_detect(ems$situation_primary_complaint_statement_list, pattern))

sum(is.na(ems$incident_psap_call_date_time))
# create month year variable
ems <- ems %>%
  mutate(incident_date = ymd(incident_date),
         yr = as.factor(year(incident_date)),
         mo = as.factor(month(incident_date)),
         dy = as.character(day(incident_date)),
         mo_yr = dmy(paste0("01-", mo, "-", yr)))

plot_data <- ems %>%
  group_by(mo_yr, potential_covid) %>%
  count()

plot_data %>%
  filter(potential_covid) %>%
  ggplot(., aes(x = mo_yr, y = n)) +
  geom_line(color = "red") +
  geom_point(stat = "identity")




covid <- str_detect(ems$situation_provider_secondary_impression_description_and_code_list, "Infectious - Coronavirus (B97.21)")
sum(is.na(covid)) # 55161
sum(covid, na.rm = TRUE) # 1715
infections <- ems %>%
  filter(covid)

levels(as.factor(infections$situation_provider_secondary_impression_description_and_code_list))

length(covid)
length(covid[covid == FALSE])
levels(as.factor(ems$situation_provider_secondary_impression_description_and_code_list))
