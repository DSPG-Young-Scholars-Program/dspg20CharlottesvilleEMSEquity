# Working with shape file on fire station response areas

library(sf)
library(ggplot2)
library(leaflet)
library(RColorBrewer)

#
# Basic maps from shape files - stations, response areas ----------------------
#

stations <- st_read("../../data/original/website_map_data/stations/Fire_Stations.shp")
head(stations)
                    
# plots the fire stations, but there is no base map

ggplot(data = stations) +
  geom_sf()

response_areas <- st_read("../../data/original/website_map_data/station_response_areas/FireResponse.shp")
head(response_areas)

ggplot(data = response_areas) +
  geom_sf()

#
# leaflet map -- nicer map with combined data ---------------------------------------------
#

stations <- st_transform(stations, 4326)
response_areas <- st_transform(response_areas, 4326)


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
  paste("<strong>Station: </strong>",
        response_areas$FIRE_FIRST,
        "<br />",
        "<strong>Square Miles Covered: </strong>",
        round(response_areas$SQMI, 2)),
  htmltools::HTML
)


station_labels <- lapply(
    paste("Station Name: </strong>",
          stations$STATION
    ),
    htmltools::HTML
  )


# map --------------------------

pal <- colorFactor(c("blue", "white"), domain = response_areas$FIRE_1st)

leaflet(data = response_areas) %>%
  #setView(zoom = 6, lat = 38.032560, lng = -79.422777) %>%
  addTiles() %>%
  addPolygons(fillColor = "white", #~pal(FIRE_1st), 
              fillOpacity = 0.5,
              stroke = TRUE,
              weight = 2,
              color = "black",
              smoothFactor = 0.7,
              layerId = ~FIRE_1st,
              label = area_labels,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          ))) %>%
  addCircleMarkers(data = stations, radius = 5, fillColor = "blue", fillOpacity = 0.2, stroke = TRUE, color = "blue", opacity = 1, weight = 1,  popup= station_labels) %>%    
  addLegendCustom(colors, labels, sizes, shapes, borders)
  
   # addLegend(position = "bottomleft",
   #         #values = ~(round(selected_type, 0)),
   #         colors = "blue",
   #         labels = c("Fire Stations"),
   #         #title = "Fire Stations",
   #         opacity = 0.2)


