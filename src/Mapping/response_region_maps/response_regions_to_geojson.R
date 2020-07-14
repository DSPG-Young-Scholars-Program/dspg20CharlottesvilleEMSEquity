# Save the response areas shapefile as a geoJSON

library(geojsonsf)
library(geojsonio)
library(sf)

# read in shapefile

response_areas <- st_read("./data/original/FireRescueResponse/FireRescueResponse.shp")
response_areas <- st_transform(response_areas, 4326)

# convert to geojson

response_geoj <- sf_geojson(response_areas)

# write to file

geojson_write(response_geoj, file = "./data/final/response_regions.geojson")


