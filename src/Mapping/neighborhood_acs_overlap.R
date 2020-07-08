library(sf)
library(here)
library(ggplot2)
library(tigris)
library(leaflet)
library(tidycensus)
library(tidyr)
library(dplyr)
library(RColorBrewer)

# census_api_key(census_key)

# download.file("http://widget.charlottesville.org/gis/zip_download/planning_area.zip", here("planning_area.zip"))
# unzip(here("planning_area.zip"), exdir = here("planning_area"))

planning_area <- st_read(here("downloaded_data/planning_area/planning_area_06_04_2020.shp"))

nrow(planning_area)

# checking overlap with block groups

cville_bgs <- block_groups(state = "Virginia", county = c("Charlottesville"), cb = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)
#
# planning_area %>%
#   ggplot() +
#   geom_sf(fill = NA, color = "red", size = 1) +
#   geom_sf(data = cville_bgs, fill = NA, color = "blue") +
#   theme_minimal()

v18 <- load_variables(2018, "acs5", cache = TRUE)



acs_data <- get_acs("block group",
                    variables = c(total_pop = "B02001_001", black_alone = "B02001_003", white_alone = "B02001_002",
                                  median_hhi = "B19013_001"),
                    state = "VA",
                    county = "Charlottesville city",
                    year = 2018)

acs_data_sp <- acs_data %>%
  select(-moe) %>% # only plotting estimates
  pivot_wider(names_from = variable, values_from = estimate) %>%
  left_join(cville_bgs, ., by = "GEOID") %>%
  st_transform(4326) %>%
  mutate(black_percent = black_alone / total_pop)


pal <- colorBin(brewer.pal(5, "Greens"), acs_data_sp$median_hhi, 5, pretty = FALSE)

planning_area %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(data = acs_data_sp,
              fillColor = ~pal(median_hhi),
              fillOpacity = 0.8,
              color = "blue",
              weight = 1,
              opacity = 1) %>%
  addPolygons(color = "#111",
              opacity = 1,
              weight = 1.2,
              fillOpacity = 0 ,
              label = ~NAME) %>%
  addLegend(data = acs_data_sp,
            pal = pal,
            values = ~median_hhi,
            title = "Median HHI")



