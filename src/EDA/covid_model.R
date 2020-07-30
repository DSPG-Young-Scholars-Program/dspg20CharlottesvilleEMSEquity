# load data into environment
source(here::here("src", "Profiling", "create_covid_indicator.R"));

# prepare data for model
# remove cases where all 3 demographic variables (gender, race, and age) are missing
model_data <- ems %>%
  select(c("patient_gender",
           "race",
           "patient_age",
           "patient_age_range_in_years",
           "covid1",
           "covid2",
           "covid3",
           "covid_indicator",
           "incident_date")) %>% # select necessary variables
  filter(!is.na(patient_gender) | !is.na(race) | !is.na(patient_age)) %>% # remove cases where all 3 demographic vars are missing
  mutate(patient_gender = coalesce(patient_gender, "missing"), # encode NAs as "missing"
         race = coalesce(race, "missing"),
         patient_age_missing = ifelse(is.na(patient_age), 1, 0),
         patient_age_range_in_years = coalesce(patient_age_range_in_years, "missing"),
         on_or_after_feb_15 = ifelse(ymd(incident_date) >= "2020-02-15", 1, 0)) # create after date variable, that's 1 if >= feb 15th 2020

# create covid model
model1 <- glm(covid1 ~ (patient_gender +
                race +
                patient_age_range_in_years) *
                on_or_after_feb_15, data = model_data, family = binomial())
summary(model1)

# patient_gendermissing:on_or_after_feb_15 == *

model2 <- glm(covid2 ~ (patient_gender +
                race +
                patient_age_range_in_years) *
                on_or_after_feb_15, data = model_data, family = binomial())
summary(model2)

model3 <- glm(covid3 ~ (patient_gender +
                race +
                patient_age_range_in_years) *
                on_or_after_feb_15, data = model_data, family = binomial())
summary(model3)


