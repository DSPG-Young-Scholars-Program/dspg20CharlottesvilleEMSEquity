
library(data.table)
library(visdat)
library(naniar)
library(stringr)
library(dplyr)

## Read in data
data <- fread("./data/original/CFD_CARS_EMS_DATA_121616TO51320.csv", 
              na.strings = c(""),
              colClasses = rep("character", 78))

## Peek
# head(data)

## Shortened column names
columnNames <- str_replace_all(sapply(str_split(colnames(data), " "), tail, 1), c("\\(" = "", "\\)" = ""))

## Manual updates for weird cases
columnNames [which(columnNames  == "Minutes")] <- "timeToArrival"
columnNames [which(columnNames  == "List")] <- "eSituation.04.1"
columnNames [which(columnNames  == "Type")] <- "incidentType"
columnNames [which(columnNames  == "eHistory.06.CodeType")] <- "eHistory.06"

## Store mapping dictionary of long to short column names (if useful for data dictionary purposes or something)
colNameMap <- data.frame(full = colnames(data), alias = make.names(columnNames, unique = TRUE))

## Make unique names to resolve duplicate alias names
colnames(data) <- make.names(columnNames , unique = TRUE)

## Convert incident date to POSIXct - can remove time?
data[, Date := as.POSIXct(Date, tz="GMT", format = "%m/%d/%Y %H:%M:%S")]

## Remove rows solely containing missing values
data <- data[rowSums(!is.na(data)) != 0,]

#
#
# Missing Data Evaluation -----------------------------------------------------------------------------------------
#
#

## Overall snapshot
gg_miss_var(data)

## Right now the concern is not the medical stuff - it's reasonable that many of these would be missing if they don't apply
## to a particular call. Missingness amongst "meta" data would be more concerning (e.g. incident id number, etc.)
metaData <- data[,.(eResponse.03, eResponse.14, incidentType, Date, eScene.11, eScene.11.1, eScene.15, dAgency.03)]

## Convenience function to automatically downsample for visualization
my_vis_miss <- function(data, n=1000) {
  if (nrow(data) < n) {
    vis_miss(data)
  } else {
    vis_miss(data[sample(nrow(data), n),])
  }
}

## It seems that for the most part, these meta variables are either missing or present together, which makes sense
## Approximately 1500 rows where only some of these variables are present
my_vis_miss(metaData, 500)

## histogram and frequency table showing distribution of number of missing values for these 8 meta variables
hist(rowSums(!is.na(metaData)))
table(rowSums(!is.na(metaData))) / nrow(metaData) * 100

## Remove cases where all these variables co-occur and re-plot
## It looks like location data is the most likely variable to be missing of these ~1500 cases
metaData$nPresent <- rowSums(!is.na(metaData))
my_vis_miss((metaData[which(metaData$nPresent != 0 & metaData$nPresent != 8),]))

## Without the GPS things are pretty coherent
my_vis_miss(metaData %>% select(-eScene.11, -eScene.11.1))

## All "meta" columns associated with a row that does have an incident number
has_inc_num <- metaData[which(!is.na(metaData$eResponse.03))]
my_vis_miss(has_inc_num)

## Still some data present without an incident number, though - are these unmatched to to other observations?
no_inc_num <- data[which(is.na(data$eResponse.03))]
my_vis_miss(no_inc_num)
