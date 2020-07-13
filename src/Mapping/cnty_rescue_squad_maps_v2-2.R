# Working with shape file on Albemarle rescue squads

library(sf)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(tigris)
library(dplyr)

#
# Read in shape files - rescue squads, response areas ----------------------
#

response_areas <- st_read("./data/original/FireRescueResponse/FireRescueResponse.shp")

pts <- st_read("./data/original/FireRescueStations/FireRescueStations.shp")

# fix a typo
pts[pts$STATION == "WARS", ]$STTYPE <- "Rescue"

#
# get census tracts --------------------------------------
#

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

# clip to the response area

clip_bgrps <- st_intersection(st_union(st_make_valid(response_areas)), cnty_block_groups)



#
# labels  -------------------------------------
#

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

#
# color palette setup --------------------------
#

cbPal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# this palette is the first 11 colors of "Paired"
cbPal2 <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", 
            "#CAB2D6", "#6A3D9A", "#FFFF99")

cbPal3 <- c("#999999", "#7570B3", "#56B4E9", "#F0E442", "#D55E00", "#CC79A7")


# choose a palette with different colors! discrete for factor variables. 

pal2 <- colorFactor(cbPal2, domain = response_areas$FIRE_FIRST)
pal <- colorFactor(cbPal, domain = response_areas$RES1DAY)
pal3 <- colorFactor(cbPal3, domain = response_areas$RES1NITE)

#
# marker setup ----------------------------------------------
#

# code adapted from https://stackoverflow.com/questions/47064921/leaflet-legend-for-addawesomemarkers-function-with-icons
# legend html generator:
markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 5px; padding-bottom: 5px;'><h5 style='padding-top:0; padding-bottom:5px; margin: 0; color:black; '> Legend </h5>"

  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "glyphicon") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 30px'>",
                          #"<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 0px; margin-top: 11px; color:white; background-color:black 'class= 'glyphicon glyphicon-",Icon[["icon"]]," glyphicon-inverse'></i>",
                          #"</div>",
                          "<p style='position: relative; top: 10px; display: inline-block; color:black; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}

IconSet <- awesomeIconList(
  "Fire"   = makeAwesomeIcon(icon= 'fire', library = "glyphicon", markerColor = 'black', iconColor = "white", squareMarker = TRUE),
  "Rescue" = makeAwesomeIcon(icon= 'plus-sign', library = "glyphicon", markerColor = 'black', iconColor = 'white', squareMarker = TRUE),
  "Fire and Rescue" = makeAwesomeIcon(icon= 'asterisk', library = "glyphicon", markerColor = 'black', iconColor = 'white', squareMarker = TRUE)
)

#
#  polygon unions ----------------------------------------
#

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

#
# map -----------------------------------------
#

leaflet(data = response_areas) %>%
  addTiles() %>%
  addMapPane("polygons", zIndex = 410) %>%
  addMapPane("points", zIndex = 430) %>%
  addMapPane("block_grps", zIndex = 420) %>%
  # addPolygons(data=cnty_tracts,
  #             fillColor = "white",
  #             fillOpacity = 0.5,
  #             stroke = TRUE,
  #             weight = 5,
  #             color = cbPalette[1],
  #             smoothFactor = 0.7
  # ) %>%
  addPolygons(data=clip_bgrps,
              fillColor = "white",
              fillOpacity = 0.0,
              stroke = TRUE,
              weight = 2,
              color = "black",
              smoothFactor = 0.7,
              options = pathOptions(pane = "block_grps")
  ) %>%
  addPolygons(data = fire,
              fillColor = ~pal2(FIRE_FIRST), 
              fillOpacity = 0.7,
              stroke = TRUE,
              weight = 0.5,
              color = "black",
              smoothFactor = 0.7,
              options = pathOptions(pane = "polygons"),
              group = "Fire First Response",
              #layerId = ~Name,
              label = lapply(
                paste("<strong>Fire 1st: </strong>",
                      fire$FIRE_FIRST),
                htmltools::HTML
                ),
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          ))
              ) %>%
  addPolygons(data = resD,
              fillColor = ~pal(RES1DAY),
              fillOpacity = 0.7,
              stroke = TRUE,
              weight = 0.5,
              color = "black",
              smoothFactor = 0.7,
              options = pathOptions(pane = "polygons"),
              group = "Rescue First Response - Day",
              #layerId = ~Name,
              label = lapply(
                paste("<strong>Rescue 1st Day: </strong>",
                      resD$RES1DAY),
                htmltools::HTML
              ),
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          ))
  ) %>%
  addPolygons(data = resN,
              fillColor = ~pal3(RES1NITE),
              fillOpacity = 0.7,
              stroke = TRUE,
              weight = 0.5,
              color = "black",
              smoothFactor = 0.7,
              options = pathOptions(pane = "polygons"),
              group = "Rescue First Response - Night",
              #layerId = ~Name,
              label = lapply(
                paste("<strong>Rescus 1st Night: </strong>",
                      resN$RES1NITE),
                htmltools::HTML
              ),
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          ))
  ) %>%
  addAwesomeMarkers(data=pts, icon = ~IconSet[STTYPE], popup = pts_labels, 
                    options = pathOptions(pane = "points")) %>%
  addLayersControl(baseGroups = c("Fire First Response", "Rescue First Response - Day", 
                                  "Rescue First Response - Night"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")
 
