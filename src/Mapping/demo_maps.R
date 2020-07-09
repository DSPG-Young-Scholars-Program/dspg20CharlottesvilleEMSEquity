
library(tidycensus)
library(dplyr)
library(leaflet)
library(sf)

## ACS table IDs
tables <- c(
  "B02001", ## Race
  "B17020", ## Poverty in past 12 months
  "B19013" ## Median household income
)

## API Key
census_api_key(Sys.getenv("CENSUS_API_KEY"))

#
#
# Mapping race ------------------------------------------------------------------------------------------------------------
#
#

## Use a simple ACS table to get the border polylines of the city
cville_border <- get_acs(geography = "county", state = 51, county = 540, 
                         table = tables[3], year = 2018, survey = "acs5", cache_table = TRUE, 
                         output = "wide", geometry = TRUE) %>% 
  select(geometry) %>% 
  st_transform(crs = 4326)

## Get race data by block group for Albemarle
alb_race <- get_acs(geography = "block group", state = 51, county = 003, 
                    table = tables[1], year = 2018, survey="acs5", cache_table = TRUE, 
                    output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

## Get race data by block group for Charlottesville
cville_race <- get_acs(geography = "block group", state = 51, county = 540,
                       table = tables[1], year = 2018, survey = "acs5", cache_table = TRUE,
                       output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

## Compute race percentages for Albemarle County
alb_race <- alb_race %>% transmute(
  STATEFP = STATEFP, 
  COUNTYFP = COUNTYFP, 
  AFFGEOID = AFFGEOID, 
  GEOID = GEOID, 
  LSAD = LSAD, 
  NAME.x = NAME.x, 
  NAME.y = NAME.y,
  ALAND = ALAND, 
  AWATER = AWATER, 
  pct_white = B02001_002E / B02001_001E * 100,
  pct_black = B02001_003E / B02001_001E * 100,
  pct_native_am = B02001_004E / B02001_001E * 100,
  pct_asian = B02001_005E / B02001_001E * 100,
  pct_pacific_isl = B02001_006E / B02001_001E * 100,
  pct_other = B02001_007E / B02001_001E * 100,
  pct_multi = B02001_008E / B02001_001E * 100,
  geometry = geometry
) %>% 
  st_transform(crs = 4326)

## Compute race percentages for Charlottesville city
cville_race <- cville_race %>% transmute(
  STATEFP = STATEFP, 
  COUNTYFP = COUNTYFP, 
  AFFGEOID = AFFGEOID, 
  GEOID = GEOID, 
  LSAD = LSAD, 
  NAME.x = NAME.x, 
  NAME.y = NAME.y,
  ALAND = ALAND, 
  AWATER = AWATER, 
  pct_white = B02001_002E / B02001_001E * 100,
  pct_black = B02001_003E / B02001_001E * 100,
  pct_native_am = B02001_004E / B02001_001E * 100,
  pct_asian = B02001_005E / B02001_001E * 100,
  pct_pacific_isl = B02001_006E / B02001_001E * 100,
  pct_other = B02001_007E / B02001_001E * 100,
  pct_multi = B02001_008E / B02001_001E * 100,
  geometry = geometry
) %>% 
  st_transform(crs = 4326)

## Convenience function to map race characteristics
map_race <- function(data, border_data, fill_opacity = 0.6, 
                     border_color = "#4D4D4D", smoothing = 0.5, 
                     fill_pal = colorBin("YlOrRd", c(0, 100))) {
  
  ## Leaflet map of race distribution by block group
  leaflet(data = data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addMapPane("polygons", zIndex = 400) %>%
    addMapPane("borders", zIndex = 410) %>%
    addPolygons(
      color = border_color,
      weight = 1,
      smoothFactor = smoothing,
      fillOpacity = fill_opacity,
      fillColor = ~fill_pal(pct_white),
      group = "White",
      options = pathOptions(pane = "polygons")
    ) %>% 
    addPolygons(
      color = border_color,
      weight = 1,
      smoothFactor = smoothing,
      fillOpacity = fill_opacity,
      fillColor = ~fill_pal(pct_black),
      group = "Black",
      options = pathOptions(pane = "polygons")
    ) %>%
    addPolygons(
      color = border_color,
      weight = 1,
      smoothFactor = smoothing,
      fillOpacity = fill_opacity,
      fillColor = ~fill_pal(pct_asian),
      group = "Asian",
      options = pathOptions(pane = "polygons")
    ) %>%
    addPolygons(
      color = border_color,
      weight = 1,
      smoothFactor = smoothing,
      fillOpacity = fill_opacity,
      fillColor = ~fill_pal(pct_native_am),
      group = "American Indian/Alaskan Native",
      options = pathOptions(pane = "polygons")
    ) %>%
    addPolygons(
      color = border_color,
      weight = 1,
      smoothFactor = smoothing,
      fillOpacity = fill_opacity,
      fillColor = ~fill_pal(pct_pacific_isl),
      group = "Pacific Islander",
      options = pathOptions(pane = "polygons")
    ) %>%
    addPolygons(
      color = border_color,
      weight = 1,
      smoothFactor = smoothing,
      fillOpacity = fill_opacity,
      fillColor = ~fill_pal(pct_other),
      group = "Other (single race)",
      options = pathOptions(pane = "polygons")
    ) %>%
    addPolygons(
      color = border_color,
      weight = 1,
      smoothFactor = smoothing,
      fillOpacity = fill_opacity,
      fillColor = ~fill_pal(pct_multi),
      group = "More than one",
      options = pathOptions(pane = "polygons")
    ) %>%
    addPolylines(
      data = border_data,
      color = "black",
      weight = 2,
      opacity = 1,
      options = pathOptions(pane = "borders")
    ) %>%
    addLegend(pal = fill_pal,
              values = c(0,100),
              position = "bottomright",
              title = "Percentage of Population<br>of Selected Race",
              opacity = 0.7) %>%
    addLayersControl(baseGroups = c("White", "Black", "Asian", "American Indian/Alaskan Native", "Pacific Islander", "Other (single race)", "More than one"),
                     options = layersControlOptions(collapsed = FALSE))
}

## Maps for county and city individually
# cville_race_map <- map_race(cville_race, cville_border) 
# alb_race_map <- map_race(alb_race, cville_border)

## Combined data for county and city
comb_race_data <- rbind(cville_race, alb_race)

## Map of race breakdown
comb_race_map <- map_race(comb_race_data, cville_border)
comb_race_map

#
#
# Mapping poverty ------------------------------------------------------------------------------------------------------------
#
#

## NOTE: Poverty is at census tract level - not block group (was not available at that resolution)

## Poverty rate for Albemarle
alb_pov <- get_acs(geography = "tract", state = 51, county = 003, 
                   table = tables[2], year = 2018, survey="acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

## Compute poverty rate and transform geographic datum
alb_pov <- alb_pov %>% 
  mutate(pov_rate = B17020_002E / B17020_001E * 100) %>% 
  st_transform(crs = 4326)

## Poverty rate for Charlottesville
cville_pov <- get_acs(geography = "tract", state = 51, county = 540, 
                      table = tables[2], year = 2018, survey="acs5", cache_table = TRUE, 
                      output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

## Compute poverty rate and transform geographic datum
cville_pov <- cville_pov %>% 
  mutate(pov_rate = B17020_002E / B17020_001E * 100) %>% 
  st_transform(crs = 4326)

## Data for county and city together
comb_pov_data <- rbind(cville_pov, alb_pov)

## Adjust color palette to avoid outliers distorting the viz
color_domain <- c(0, max(comb_pov_data$pov_rate[comb_pov_data$pov_rate < 60], na.rm=TRUE))
fill_pal <- colorBin("RdPu", color_domain, bins = 5, pretty = TRUE)

comb_pov_map <- leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addPolygons(
    data = comb_pov_data %>% filter(pov_rate < 60), ## Separate polygons with a rate less than 60 to keep the color ramp informative
    color = "#4D4D4D",
    weight = 1,
    smoothFactor = 0.5,
    label = ~paste0("Poverty rate: ", round(pov_rate, 2), "%"),
    fillOpacity = 0.7,
    fillColor = ~fill_pal(pov_rate)
  ) %>% 
  addPolygons(
    data = comb_pov_data %>% filter(pov_rate > 60), ## Include outlier tracts but exclude them from the color ramp
    color = "#4D4D4D",
    weight = 1,
    smoothFactor = 0.5,
    label = ~paste0("Poverty rate: ", round(pov_rate, 2), "%"),
    fillOpacity = 0.7,
    fillColor = "gray"
  ) %>%
  addPolylines(
    data = cville_border,
    color = "black",
    weight = 2,
    opacity = 1
  ) %>%
  addLegend(pal = fill_pal,
            values = c(0,100),
            position = "bottomright",
            title = "Percentage below poverty line",
            opacity = 0.7)

comb_pov_map

#
#
# Mapping median household income ------------------------------------------------------------------------------------------------------------
#
#

## Median household income for Albemarle
alb_income <- get_acs(geography = "block group", state = 51, county = 003, 
                      table = tables[3], year = 2018, survey="acs5", cache_table = TRUE, 
                      output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

## Rename variable and transform geographic datum
alb_income <- alb_income %>% 
  rename(med_income = B19013_001E) %>% 
  st_transform(crs = 4326)

## Median household income for Charlottesville
cville_income <- get_acs(geography = "block group", state = 51, county = 540, 
                         table = tables[3], year = 2018, survey="acs5", cache_table = TRUE, 
                         output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

## Rename variable and transform geographic datum
cville_income <- cville_income %>% 
  rename(med_income = B19013_001E) %>% 
  st_transform(crs = 4326)

## Convenience function for leaflet map
map_income <- function(data, border_data, fill_opacity = 0.7, 
                       border_color = "#4D4D4D", smoothing = 0.5, 
                       fill_pal = colorBin("YlGn", data$med_income, bins=10, pretty=TRUE)) {
  
  leaflet(data = data) %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addPolygons(
      color = border_color,
      weight = 1,
      smoothFactor = smoothing,
      fillOpacity = fill_opacity,
      fillColor = ~fill_pal(med_income),
      label = ~ifelse(!is.na(data$med_income), paste0("$", prettyNum(data$med_income, big.mark = ",")), "Missing")
    ) %>% 
    addPolylines(
      data = border_data,
      color = "#222222",
      weight = 2,
      opacity = 1
    ) %>%
    addLegend(pal = fill_pal,
              values = data$med_income,
              position = "bottomright",
              title = "Median household income",
              na.label = "Missing",
              opacity = 0.7)
}

## Maps of median income
# alb_income_map <- map_income(alb_income, cville_border)
# cville_income_map <- map_income(cville_income, cville_border, fill_pal = colorBin("RdBu", cville_income$med_income, bins=5, pretty=TRUE))

## Map of income for county and city together
comb_income_map <- map_income(rbind(cville_income, alb_income), cville_border, border_color = "#787A7A")
comb_income_map

