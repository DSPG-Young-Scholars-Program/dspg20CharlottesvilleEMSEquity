library(tidycensus)
library(tidyverse)
library(viridis)
library(tigris)

# load census api key
census_api_key("bf17bc7966a00f283ce6fcef72c0a37bb1650c26", install = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# census data example
us_county_income <- get_acs(geography = "county", variables = "B19013_001",
                            shift_geo = TRUE, geometry = TRUE)

ggplot(us_county_income) +
  geom_sf(aes(fill = estimate), color = NA) +
  coord_sf(datum = NA) +
  theme_minimal() +
  scale_fill_viridis_c()

# view available variables for the 5-year ACS 2013-2017
v17 <- load_variables(year = 2017, dataset = "acs5", cache = TRUE)
View(v17)

# school districts
va_school_districs <- school_districts(state = "VA", type = "unified", class="sf")
# Charlottesville City Public Schools
cville_sd <- va_school_districs[va_school_districs$NAME == "Charlottesville City Public Schools",]
# Albemarle County Public Schools
alb_sd <- va_school_districs[va_school_districs$NAME == "Albemarle County Public Schools",]

# PLOTTING
# hm not really what I want. is there a way to get the school zones within district?
ggplot(cville_sd) +
  geom_sf()

ggplot(alb_sd) +
  geom_sf()

# POVERTY
# CVILLE FIPS = 504, ALB FIPS = 003
cville_pov <- get_acs(geography = "tract", variables = "B1")
