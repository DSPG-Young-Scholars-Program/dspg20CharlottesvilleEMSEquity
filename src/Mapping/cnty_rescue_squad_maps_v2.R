# Working with shape file on Albemarle rescue squads

library(sf)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(tigris)

#
# Basic maps from shape files - rescue squads, response areas ----------------------
#

response_areas <- st_read("./data/original/FireRescueResponse/FireRescueResponse.shp")

pts <- st_read("./data/original/FireRescueStations/FireRescueStations.shp")

# fix a typo
pts[pts$STATION == "WARS", ]$STTYPE <- "Rescue"

# plots the response areas and stations/rescue squads

ggplot(data = response_areas) +
  geom_sf()

ggplot(data = pts) +
  geom_sf()

# get census tracts --------------------------------------

# cnty_tracts <- tracts(state = "Virginia", 
#                       county = c("Albemarle", "Fluvanna", "Buckingham"), cb = TRUE) %>% 
#   st_as_sf() %>%
#   st_transform(crs = 4326)


# get census block groups --------------------------------------

cnty_block_groups <- block_groups(state = "Virginia", 
                                  county = c("Albemarle", "Fluvanna", "Buckingham"),
                                  cb = TRUE) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326)

#
# leaflet map -----------------------------------------------
#

response_areas <- st_transform(response_areas, 4326)
pts <- st_transform(pts, 4326)

# labels  -------------------------------------

response_areas_labels <- lapply(
  paste("<strong>Name: </strong>",
        response_areas$Name,
        "<br />",
        "<strong>Fire 1st: </strong>",
        response_areas$FIRE_FIRST,
        "<br />",
        "<strong>Rescue 1st Day: </strong>",
        response_areas$RES1DAY,
        "<br />",
        "<strong>Rescue 1st Night: </strong>",
        response_areas$RES1NITE,
        "<br />",
        "<strong>Jurisdiction: </strong>",
        response_areas$Jurisdicti),
  htmltools::HTML
)

pts_labels <- lapply(
  paste("Station Name: </strong>",
        pts$STATION,
        "<br />",
        "<strong>Type: </strong>",
        pts$STTYPE,
        "<br />",
        "<strong>ID: </strong>",
        pts$STID
  ),
  htmltools::HTML
)

# map --------------------------


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# wheel <- function(col, radius = 1, ...)
#   pie(rep(1, length(col)), col = col, radius = radius, ...)
# wheel(cbPalette)

pal <- colorFactor(cbPalette[c(5,4,6)], domain = pts$STTYPE) 

pal2 <- colorFactor("Blues", domain = response_areas$FIRE_1st)
pal3 <- colorFactor("Blues", domain = response_areas$Rescue_1st)
pal4 <- colorFactor("Blues", domain = response_areas$Rescue_1_1)

leaflet(data = response_areas) %>%
  #setView(zoom = 6, lat = 38.032560, lng = -79.422777) %>%
  addTiles() %>%
  # addPolygons(data=cnty_tracts,
  #             fillColor = "white",
  #             fillOpacity = 0.5,
  #             stroke = TRUE,
  #             weight = 5,
  #             color = cbPalette[1],
  #             smoothFactor = 0.7
  # ) %>%
  addPolygons(data=cnty_block_groups,
              fillColor = "white",
              fillOpacity = 0.0,
              stroke = TRUE,
              weight = 2,
              color = cbPalette[1],
              smoothFactor = 0.7
  ) %>%
  addPolygons(fillColor = ~pal2(FIRE_1st), 
               fillOpacity = 0.7,
              stroke = TRUE,
              weight = 2,
              color = "black",
              smoothFactor = 0.7,
              group = "Fire First Response",
              layerId = ~Name,
              label = response_areas_labels,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          ))
              ) %>%
  # addPolygons(fillColor = ~pal3(Rescue_1st), 
  #             fillOpacity = 0.7,
  #             stroke = TRUE,
  #             weight = 2,
  #             color = "black",
  #             smoothFactor = 0.7,
  #             group = "Rescue First Response - Day",
  #             layerId = ~Name,
  #             label = response_areas_labels,
  #             labelOptions = labelOptions(direction = "bottom",
  #                                         style = list(
  #                                           "font-size" = "12px",
  #                                           "border-color" = "rgba(0,0,0,0.5)",
  #                                           direction = "auto"
  #                                         ))
  # ) %>%
  # addPolygons(fillColor = ~pal4(Rescue_1_1), 
  #             fillOpacity = 0.7,
  #             stroke = TRUE,
  #             weight = 2,
  #             color = "black",
  #             smoothFactor = 0.7,
  #             group = "Rescue First Response - Night",
  #             layerId = ~Name,
  #             label = response_areas_labels,
  #             labelOptions = labelOptions(direction = "bottom",
  #                                         style = list(
  #                                           "font-size" = "12px",
  #                                           "border-color" = "rgba(0,0,0,0.5)",
  #                                           direction = "auto"
  #                                         ))
  #) %>%
  addCircleMarkers(data = pts, radius = 7, fillColor = ~pal(STTYPE), fillOpacity = 0.7, 
                   stroke = TRUE, color = "black", opacity = 1, weight = 1,  popup = pts_labels) %>%
  addLayersControl(baseGroups = c("Fire First Response", "Rescue First Response - Day", 
                                  "Rescue First Response - Night"),
                   options = layersControlOptions(collapsed = FALSE))
  # #addLegendCustom(colors, labels, sizes, shapes, borders)






# get census blocks for Albemarle County - finer grained than rescue squad map

#cnty_blocks <- blocks(state = "Virginia", county = c("Albemarle")) #, cb = TRUE) #%>%
  #st_as_sf() %>%
  #st_transform(crs = 4326)

# leaflet(cnty_tracts) %>%
#   addTiles() %>%
#   addPolygons()


# Question for Damon: 
# 1) last two digits of area name?  first two are fire station ID 1st reponse
# 2) CARS on station data?
# 3) add c-ville stations: 01, 10, HQ
# 4) find 05X1 on map.


