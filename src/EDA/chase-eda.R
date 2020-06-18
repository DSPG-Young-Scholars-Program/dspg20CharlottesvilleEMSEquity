# validating data
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(leaflet)

getwd() # make sure that working directory = /sfs/qumulo/qhome/scd3dz/git/dspg20CharlottesvilleEMSEquity/
setwd("/sfs/qumulo/qhome/scd3dz/git/dspg20CharlottesvilleEMSEquity")
ems <- as.data.table(read_excel("./data/original/CFD_CARS_EMS_DATA_121616TO60920.xlsx", 1))
head(ems)
colnames(ems)

# Incident Complaint Reported By Dispatch
var = ems$`Incident Complaint Reported By Dispatch (eDispatch.01)`
class(var) # character
sum(is.na(var)) # 202
head(var) # seems like I could convert to factor
levels(as.factor(var))
ggplot(data = ems, aes(x = `Incident Complaint Reported By Dispatch (eDispatch.01)`)) +
  geom_bar()

# Response Incident Number (eResponse.03)
var = ems$`Response Incident Number (eResponse.03)`
class(var) # character
sum(is.na(var)) # 27
head(var) # seems like I could convert to factor
levels(as.factor(var))

ggplot(data = tmp, aes(x = `Response Incident Number (eResponse.03)`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# weird that there are some duplicates of response incident number but none that look out of place,
# less than 50 duplicates for each

# Response EMS Unit Call Sign (eResponse.14)
var = ems$`Response EMS Unit Call Sign (eResponse.14)`
class(var) # character
sum(is.na(var)) # 5
head(var) # seems like I could convert to factor
levels(as.factor(var))

# orders ems in descending order, stores in tmp
tmp <- within(ems,
              `Response EMS Unit Call Sign (eResponse.14)` <- factor(`Response EMS Unit Call Sign (eResponse.14)`,
                                                                     levels=names(sort(table(`Response EMS Unit Call Sign (eResponse.14)`),
                                                                                       decreasing=TRUE))))
ggplot(data = tmp, aes(x = `Response EMS Unit Call Sign (eResponse.14)`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Incident Type
var = ems$`Incident Type`
class(var) # character
sum(is.na(var)) # 0
head(var) # seems like I could convert to factor
levels(as.factor(var))

# Incident Date
var = ems$`Incident Date`
class(var) # "POSIXct" "POSIXt"
sum(is.na(var)) # 0
head(var) # seems like I could convert to factor
hist(var, breaks=20)
# generally, dates span from 2016 - 2020, nothing looks wrong here
# could come back later and look at

# Scene GPS Latitude (eScene.11) and Scene GPS Longitude (eScene.11)
# seem to mostly make sense
markers <- ems %>% select(`Scene GPS Latitude (eScene.11)`, `Scene GPS Longitude (eScene.11)`)
leaflet(ems) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~`Scene GPS Longitude (eScene.11)`,
    lat = ~`Scene GPS Latitude (eScene.11)`,
    radius=5,
    color="red",
    stroke=FALSE,
    fillOpacity = 0.1
  )

# Disposition Number Of Patients Transported In EMS Unit (eDisposition.11)
var = ems$`Disposition Number Of Patients Transported In EMS Unit (eDisposition.11)`
class(var) # numeric
sum(is.na(var)) # 19286
head(var)
hist(var, breaks = 10)
summary(var)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#  1.000   1.000   1.000   1.014   1.000   7.000   19286

# Agency Name (dAgency.03)
var = ems$`Agency Name (dAgency.03)`
class(var) # character
sum(is.na(var)) # 0
head(var) # seems like I could convert to factor
levels(as.factor(var))

ggplot(data = ems, aes(x = `Agency Name (dAgency.03)`)) +
  geom_bar()
# Cville-Alb Rescue Squad has about double the cases of Cville Fire Dept.

# Situation Provider Primary Impression Code And Description (eSituation.11)
var = ems$`Situation Provider Primary Impression Code And Description (eSituation.11)`
class(var) # character
sum(is.na(var)) # 12445
head(var) # could maybe be a factor
levels(as.factor(var))

# sort then plot
tmp <- within(ems,
              `Situation Provider Primary Impression Code And Description (eSituation.11)` <- factor(`Situation Provider Primary Impression Code And Description (eSituation.11)`,
                                                                     levels=names(sort(table(`Situation Provider Primary Impression Code And Description (eSituation.11)`),
                                                                                       decreasing=TRUE))))
ggplot(data = tmp, aes(x = `Situation Provider Primary Impression Code And Description (eSituation.11)`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Situation Provider Secondary Impression Description And Code (eSituation.12)
var = ems$`Situation Provider Secondary Impression Description And Code (eSituation.12)`
class(var) # character
sum(is.na(var)) # 28864
head(var)
