library(readr)
library(tidyverse)
# SALES
sales <- read_csv("../../Real_Estate__Sales_.csv")
names(sales) # variable names
# get number of NA values in each column
map(sales, ~sum(is.na(.)))
# $RecordID_Int
# [1] 0
#
# $ParcelNumber
# [1] 0
#
# $SaleDate
# [1] 1
#
# $SaleAmount
# [1] 0
#
# $StreetName
# [1] 4
#
# $StreetNumber
# [1] 76
#
# $Unit
# [1] 50953
summary(sales)
par(mar=c(2,2,2,2))
hist(sales$SaleAmount, breaks=10)
sales %>% filter(sales$SaleAmount >= 5000000) %>% count() # 145 sold for >= 5mil
# filter out those values
sales <- sales %>% filter(sales$SaleAmount <= 1000000)
hist(sales$SaleAmount, breaks=10)
adds <- read_csv("../../Real_Estate__Additions_.csv")


# PARCELS
parcels <- read_csv("../../Parcel_Area_Details.csv")

# read in geojson data for Cville Parcels
library(geojsonio)
parcel_geo <- geojson_read("../../Parcel_Boundary_Area.geojson", what = "sp")
leaflet(parcel_geo) %>%
  addTiles() %>%
  addPolygons()
