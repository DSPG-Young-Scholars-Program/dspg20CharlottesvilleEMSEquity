# reading data
library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)
library(tigris)
library(readxl)


## SETUP
# set working directory
setwd("/home/scd3dz/git/dspg20CharlottesvilleEMSEquity")
# read data
# read in everything as strings
ems <- read_excel("./data/original/CFD_CARS_EMS_DATA_121616TO60920.xlsx", 1) # there's only 1 sheet


## WORK
# list names of all variables
names(ems)
# structure of each variable
date_char <- ems$`Incident Date`
date_char <- date[!is.na(date)] # remove na values from date vector

# `Incident Date` is currently a character. convert to datetime
t <- date[1] # "12/14/2016 12:00:00 AM"
class(t) # t is a character
# convert from am/pm to military time
t
strptime(t, "%I:%M:%S %p")
parse_date_time(t, '%m-%d-%Y %I:%M:%S %p')
# convert entire vector
date <- parse_date_time(date_char, '%m-%d-%Y %I:%M:%S %p')
ems$date <- parse_date_time(ems$`Incident Date`, '%m-%d-%Y %I:%M:%S %p')

# remove observations where date is na
ems1 <- ems %>% filter(!is.na(date))
ems2 <- ems1 %>% filter(year(date) >= 2016)
hist(ems2$date, breaks=10)
# create histogram of dates
hist(date, breaks=10) # let's remove any dates with years < 2016
tmp <- date %>% filter(year(date) >= 2016)


mdy_hms(date[1])
AsDateTime(date[1])
ems$`Incident Date`
# get number of NA in date
sum(is.na(ems$`Incident Date`)) # 224342, hmm all seem to be NA, I might be doing something wrong
class(date)
hist(date)

getwd()
here()
