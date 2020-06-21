# Working with shape file on Charlottesville fire station locations

library(sf)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(tidycensus)

#
# Basic maps from shape files - stations, city border ----------------------
#

stations <- st_read("../../data/original/website_map_data/Charlottesville_stations/Charlottesville_Fire_Department_Station_Locations.shp")
head(stations)

# plots the fire stations, but there is no base map

ggplot(data = stations) +
  geom_sf()

city_outline <- st_read("../../data/original/website_map_data/charlottesville_outline/Charlottesville_Basemap.shp")
ggplot(data = city_outline) +
  geom_sf()


#
# leaflet map -- nicer map with combined data ---------------------------------------------
#

stations <- st_transform(stations, 4326)
city_outline <- st_transform(city_outline, 4326)


# custom legend -----------------------------------
# code adapted from https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends


# set legend features
colors <- c("blue")
labels <- c("Fire Station")
sizes <- c(10)
shapes <- c("circle")
borders <- c("blue")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 1){
  
  mycol <- rgb(0, 0, 255, max = 255, alpha = 51, names = "blue20")  # 51 is 20% of 255 -- changes opacity for fill only
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    paste0(mycol, "; width:", sizes, "px; height:", sizes, "px; border:1px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity))
  
}

# labels  -------------------------------------

area_labels <- lapply(
  paste("Charlottesville, Virginia"
        ),
  htmltools::HTML
)


station_labels <- lapply(
  paste("Station Name: </strong>",
        stations$NAME
  ),
  htmltools::HTML
)


# map --------------------------


leaflet(data = st_zm(city_outline)) %>%
  #setView(zoom = 6, lat = 38.032560, lng = -79.422777) %>%
  addTiles() %>%
  addPolygons(
              fillColor = "white",
              fillOpacity = 0.5,
              stroke = TRUE,
              weight = 2,
              color = "black",
              smoothFactor = 0.7,
              layerId = ~ID,
              label = area_labels,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          ))) %>% 
  addCircleMarkers(data = stations, radius = 5, fillColor = "blue", fillOpacity = 0.2, stroke = TRUE, color = "blue", opacity = 1, weight = 1,  popup= station_labels) %>%    
  addLegendCustom(colors, labels, sizes, shapes, borders)


