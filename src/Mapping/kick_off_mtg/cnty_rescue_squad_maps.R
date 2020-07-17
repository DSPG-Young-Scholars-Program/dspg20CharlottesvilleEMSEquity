# Working with shape file on rescue squads

library(sf)
library(ggplot2)
library(leaflet)
library(RColorBrewer)

#
# Basic maps from shape files - rescue squads, response areas ----------------------
#

squads <- st_read("../../data/original/website_map_data/rescue_squads/Rescue_Squads.shp")
head(squads)

# plots the fire stations, but there is no base map

ggplot(data = squads) +
  geom_sf()

response_day <- st_read("../../data/original/website_map_data/rescue_response_day/RescueResponseDay.shp")
head(response_day)

ggplot(data = response_day) +
  geom_sf()

response_night <- st_read("../../data/original/website_map_data/rescue_response_night/RescueResponseNight.shp")
head(response_night)

ggplot(data = response_night) +
  geom_sf()




#
# leaflet map -- nicer map with day combined data ---------------------------------------------
#

squads <- st_transform(squads, 4326)
response_day <- st_transform(response_day, 4326)
response_night <- st_transform(response_night, 4326)

# custom legend -----------------------------------
# code adapted from https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends


# set legend features
colors <- c("green", "yellow", "blue")
labels <- c("Day/Night Rescue Squad", "Day Rescue Squad", "Night Rescue Squad")
sizes <- c(10, 10, 10)
shapes <- c("circle", "circle", "circle")
borders <- c("black", "black", "black")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 1){

  rgb_vals <- col2rgb(c("green", "yellow", "blue"))
  mycols <- rgb(rgb_vals[ ,1], rgb_vals[ ,2], rgb_vals[ ,3], max = 255, alpha = 127.5, 
                names = c("green50", "yellow50", "blue50"))  # changes fill only to 50% transparency
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    paste0(mycols, "; width:", sizes, "px; height:", sizes, "px; border:1px solid ", borders, "; border-radius:", shapes)
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

day_labels <- lapply(
  paste("<strong>Rescue Squad: </strong>",
        response_day$RES1DAY,
        "<br />",
        "<strong>Square Miles Covered: </strong>",
        round(response_day$SQMI, 2)),
  htmltools::HTML
)

night_labels <- lapply(
  paste("<strong>Rescue Squad: </strong>",
        response_night$RES1NITE,
        "<br />",
        "<strong>Square Miles Covered: </strong>",
        round(response_night$SQMI, 2)),
  htmltools::HTML
)

squad_labels <- lapply(
  paste("Rescue Squad Name: </strong>",
        squads$SQUAD,
        "<br />",
        "<strong>Active: </strong>",
        squads$Active
  ),
  htmltools::HTML
)


# map --------------------------

pal <- colorFactor(c("green", "yellow", "blue"), domain = squads$Active) 

leaflet(data = response_day) %>%
  #setView(zoom = 6, lat = 38.032560, lng = -79.422777) %>%
  addTiles() %>%
  addPolygons(fillColor = "white", #~pal(FIRE_1st), 
              fillOpacity = 0.5,
              stroke = TRUE,
              weight = 2,
              color = "black",
              smoothFactor = 0.7,
              layerId = ~Rescue_1st,
              label = day_labels,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          ))) %>%
  addCircleMarkers(data = squads, radius = 5, fillColor = ~pal(Active), fillOpacity = 0.5, stroke = TRUE, color = "black", opacity = 1, weight = 1,  popup= squad_labels) %>%    
  addLegendCustom(colors, labels, sizes, shapes, borders)





#
# leaflet map -- nicer map with night combined data ---------------------------------------------
#

# map --------------------------

leaflet(data = response_night) %>%
  #setView(zoom = 6, lat = 38.032560, lng = -79.422777) %>%
  addTiles() %>%
  addPolygons(fillColor = "white", #~pal(FIRE_1st), 
              fillOpacity = 0.5,
              stroke = TRUE,
              weight = 2,
              color = "black",
              smoothFactor = 0.7,
              layerId = ~Rescue_1st,
              label = night_labels,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          ))) %>%
  addCircleMarkers(data = squads, radius = 5, fillColor = ~pal(Active), fillOpacity = 0.5, stroke = TRUE, color = "black", opacity = 1, weight = 1,  popup= squad_labels) %>%    
  addLegendCustom(colors, labels, sizes, shapes, borders)


