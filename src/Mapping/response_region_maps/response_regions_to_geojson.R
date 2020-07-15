# Save the response areas as a geoJSON

library(geojsonsf)
library(geojsonio)
library(sf)
library(dplyr)

# read in shapefile

response_areas <- st_read("./data/original/FireRescueResponse/FireRescueResponse.shp")
response_areas <- st_transform(response_areas, 4326)

# keep original shapefile but also union the fire station, rescue day, and rescue night regions

fire <- response_areas %>%
  group_by(FIRE_FIRST) %>%
  summarise(geom = st_union(st_make_valid(geometry)))

resD <- response_areas %>%
  group_by(RES1DAY) %>%
  summarise(geom = st_union(st_make_valid(geometry))) %>%
  filter(!is.na(RES1DAY))

resN <- response_areas %>%
  group_by(RES1NITE) %>%
  summarise(geom = st_union(st_make_valid(geometry))) %>%
  filter(!is.na(RES1NITE))

# convert each set of polygons to geojson

response_geoj <- sf_geojson(response_areas)
fire_geoj <- sf_geojson(fire)
resD_geoj <- sf_geojson(resD)
resN_geoj <- sf_geojson(resN)

# write to file

geojson_write(response_geoj, file = "./data/final/response_regions/response_regions.geojson")
geojson_write(fire_geoj, file = "./data/final/response_regions/fire_regions.geojson")
geojson_write(resD_geoj, file = "./data/final/response_regions/rescue_day_regions.geojson")
geojson_write(resN_geoj, file = "./data/final/response_regions/rescue_night_regions.geojson")



