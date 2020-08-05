
library(sf)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidycensus)

source(here::here("src", "IPF", "04_run_ipf.R"))

## Cville neighborhoods
planning_area <- st_read(here::here("data", "original", "neighborhoods", "planning_area_06_04_2020.shp")) %>% st_transform(crs = 4326)

## Synthetic pop (from 04_run_ipf.R)
synth_pop_sf <- st_as_sf(synth_pop_latlong, coords = c("long", "lat"), crs = 4326)

## Join neighborhoods on synthetic point data. Recode categorical variables
joined <- st_join(planning_area, synth_pop_sf, left = FALSE, join = st_covers) %>%
  mutate(SEX = recode(SEX, "1" = "male", "2" = "female"),
         RACE = recode(RAC1P, "1" = "white", "2" = "black", "3" = "am_ind",
                       "4" = "alaska.native", "5" = "am_ind_alaska_native_tribes_spec", 
                       "6" = "asian", "7" = "hawaiian_pac_isl", "8" = "other", "9" = "multiple"))

## Calculate mean/SE by neighborhood for age
age_by_neighborhood <- joined %>% 
  group_by(NAME) %>% 
  summarize(n = n(), med_age = median(AGEP))
            #mean_age = mean(AGEP), se_age = sd(AGEP) / sqrt(n))

## Calculate gender proportions by neighborhood
sex_by_neighborhood <- joined %>% 
  group_by(NAME, SEX) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(NAME) %>% 
  mutate(prop = n / sum(n)) %>%
  mutate(prop_rev = 1 - prop, se_prop_gender = round(sqrt((prop * prop_rev) / sum(n)), 3)) %>%
  select(-n, -prop_rev) %>%
  as.data.frame() %>%
  pivot_wider(names_from = SEX, values_from = prop)

## Calculate race proportions by neighborhood
race_by_neighborhood <- joined %>%
  group_by(NAME, RACE) %>%
  count() %>%
  ungroup() %>% 
  group_by(NAME) %>% 
  mutate(prop = n / sum(n)) %>%
  mutate(prop_rev = 1 - prop, se_prop = sqrt((prop * prop_rev) / sum(n))) %>%
  select(-n, -prop_rev) %>%
  as.data.frame() %>%
  pivot_wider(names_from = RACE, values_from = c(prop, se_prop))

## Combine into single source
aggregate_data <- full_join(full_join(age_by_neighborhood, sex_by_neighborhood), race_by_neighborhood)

# st_write(aggregate_data, here::here("data", "working", "neighborhood_demographics.geojson"), driver = "GeoJSON")

#
#
# Some basic maps to see how sensical results are ---------------------------------------------------------------------------
#
#

## Actual ACS data for reference
cville_bgs <- block_groups(state = "Virginia", county = c("Charlottesville"), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

acs_data <- get_acs("block group",
                    variables = c(total_pop = "B02001_001", black_alone = "B02001_003", white_alone = "B02001_002"),
                    state = "VA",
                    county = "Charlottesville city",
                    year = 2018)

acs_data_sp <- acs_data %>%
  select(-moe) %>% # only plotting estimates
  pivot_wider(names_from = variable, values_from = estimate) %>%
  left_join(cville_bgs, ., by = "GEOID") %>%
  st_transform(4326) %>%
  mutate(black_percent = black_alone / total_pop)

pal <- colorBin("Reds", domain = c(0,8000))

## Comparing actual and synthetic for total_pop:

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = acs_data_sp,
              fillOpacity = 0.7,
              fillColor = ~pal(total_pop),
              weight = 3,
              color = "gray",
              label = ~total_pop,
              group = "ACS") %>%
  addPolygons(data = aggregate_data,
              fillOpacity = 0.7,
              fillColor = ~pal(n),
              weight = 3,
              color = "gray",
              label = ~n,
              group = "Synthetic") %>%
  addLayersControl(baseGroups = c("ACS", "Synthetic")) %>%
  addLegend("bottomright", pal = pal, values = seq(0,1))

## ----- Synthetic Population Results ----- ## 

## Mapping to check results
pal <- colorBin("Blues", domain = c(0,1))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = aggregate_data,
              fillOpacity = 0.7,
              fillColor = ~pal(prop_white),
              weight = 3,
              color = "gray",
              label = ~prop_white,
              group = "White") %>%
  addPolygons(data = aggregate_data,
              fillOpacity = 0.7,
              fillColor = ~pal(prop_black),
              weight = 3,
              color = "gray",
              label = ~prop_black,
              group = "Black") %>%
  addPolygons(data = aggregate_data,
              fillOpacity = 0.7,
              fillColor = ~pal(prop_asian),
              weight = 3,
              color = "gray",
              label = ~prop_asian,
              group = "Asian") %>%
  addLayersControl(baseGroups = c("Black", "White", "Asian")) %>%
  addLegend("bottomright", pal = pal, values = seq(0,1))

## Synthetic results for age:
pal_age <- colorBin("Greens", domain = range(aggregate_data$med_age))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = aggregate_data,
              fillOpacity = 0.7,
              fillColor = ~pal_age(med_age),
              weight = 3,
              color = "gray",
              label = ~med_age) %>%
  addLegend("bottomright", pal = pal_age, values = seq(0,1))


## Could stand to compare a couple more to the ACS estimates
