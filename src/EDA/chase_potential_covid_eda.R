library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(vroom)

# run joining_albermarle_charlottesville.R to get ems data frame
head(new_ems_data)
ems <- new_ems_data
# look into potential covid
levels(as.factor(ems$situation_provider_primary_impression_code_and_description))
ems %>%
  filter(situation_provider_primary_impression_code_and_description == "Infectious - Coronavirus (B97.21)") %>%
  count()

covid <- str_detect(ems$situation_provider_secondary_impression_description_and_code_list, "Infectious")
sum(is.na(covid)) # 55161
sum(covid, na.rm = TRUE) # 1715
infections <- ems %>%
  filter(covid)

levels(as.factor(infections$situation_provider_secondary_impression_description_and_code_list))

length(covid)
length(covid[covid == FALSE])
levels(as.factor(ems$situation_provider_secondary_impression_description_and_code_list))
