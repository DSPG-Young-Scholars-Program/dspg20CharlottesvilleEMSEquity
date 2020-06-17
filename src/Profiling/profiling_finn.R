
library(data.table)
library(naniar)
library(visdat)
library(stringr)
library(dplyr)
library(here)
library(lubridate)
library(ggplot2)

## Read in data for county and city
alb_data <- read.csv(here("data", "original", "Data4UVA.csv"), fileEncoding="UTF-16LE", sep = ",", na.strings = "")
cville_data <- readxl::read_xlsx(here("data", "original", "CFD_CARS_EMS_DATA_121616TO60920.xlsx"))

## Standardize column names for each dataset
alb_data <- alb_data %>%
  rename_with(~tolower(gsub(r"(\.\..*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(r"(\.)", "_", .x)) #%>% # change periods to underscores
#as.data.table()

cville_data <- cville_data %>%
  rename_with(~tolower(gsub(r"( \(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(" ", "_", .x)) #%>% # change spaces to underscores
#as.data.table()

#
#
# Longitudinal Consistency ---------------------------------------------------------------------------------------------
#
#

## All datetime columns
cville_datetimes <- cville_data %>% select(response_incident_number,
                                           incident_date, 
                                           incident_psap_call_date_time, 
                                           cardiac_arrest_date_time, 
                                           cardiac_arrest_initial_cpr_date_time, 
                                           cardiac_arrest_rosc_date_time)

## Basic function to ensure dates are the same for all combinations of datetime columns
compare_dates <- function(data, cols, indices = FALSE) {
  
  results <- c() ## Empty vector for storing indices
  combinations <- combn(cols, 2) ## Get all possible combinations of columns to compare
  
  ## Iterate through combinations and identify instances where dates are inconsistent
  for (comb in seq(1:ncol(combinations))) {
    var1 <- combinations[1, comb]
    var2 <- combinations[2, comb]
    
    ref_dates <- as.Date(pull(data, var1))
    comp_dates <- as.Date(pull(data, var2))
    
    errs <- which(ref_dates != comp_dates)
    
    results <- c(results, errs)
  }
  
  ## Indices argument indicates whether to return error locations or values
  if (indices == FALSE) {
    return(data[unique(results),])
  } else {
    return(unique(results))
  }
  
}

## Datetime columns for comparison
## Note: "destination_cardiac_arrest_team_activation_date_time" also a time column but only has missing values
comp_cols <- c("incident_date",
               "incident_psap_call_date_time",
               "cardiac_arrest_date_time",
               "cardiac_arrest_initial_cpr_date_time", ## Empty column
               "cardiac_arrest_rosc_date_time") ## Empty column

## ID inconsistent dates
date_errs <- compare_dates(cville_data, comp_cols)
date_errs

# ------------------------------------------------------------------------------------------------------------------------------

## Checking time logic (as opposed to date logic)
test <- cville_datetimes %>% 
  filter(!is.na(cardiac_arrest_date_time)) %>% 
  select("incident_psap_call_date_time",
         "cardiac_arrest_date_time") %>%
  mutate(time_diff = difftime(cardiac_arrest_date_time, incident_psap_call_date_time))

# 759 cases (58% of non-null cases) have a cardiac arrest before the incident is reported. How is the cardiac arrest time being recorded?
length(which(test$time_diff < 0))

# ------------------------------------------------------------------------------------------------------------------------------

## Time duration variable checks ##

## This suggests there are only 3 cases where total unit response is greater than the more restrictive dispatch_notified variable
## Either I'm misunderstanding the variables or something isn't right in the data dictinoary, because this doesn't make logical sense.
cville_data[which(cville_data$incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes < cville_data$total_unit_response_time),]

#
#
# County data - data recording errors? --------------------------------------------------- 
#
#

## 2029 incident dates are fail to parse - only a few hundred are NA though.
parse_fails <- which(is.na(ymd(as.character(alb_data$incident_date), tz="UTC")))

## They appear to be misrecorded data?
as.character(alb_data$incident_date)[parse_fails]

## The entire rows for these observations are weird. I wonder if it is a data reading issue/something to do with encoding?
## It looks like these rows are just shifted to the right or left. As if there was a missing comma on the end of the line or something.
error_rows <- alb_data[parse_fails,]

## Error rows either completely populated with nonsense or filled with nearly all NAs
# vis_miss(error_rows)

## Quick check to see if these are consecutive entries.
values <- vector()

for (i in seq_along(parse_fails)) {
  val <- parse_fails[i+1] - parse_fails[i]
  values <- append(values, val)
}

values ## Looks like there is a long sequence of errors, but that's not the only thing that's going on.

## Comparing dates - won't wory until data recording errors are fixed
# alb_datetimes <- alb_data %>% select(incident_date, 
#                                      incident_psap_call_date_time, 
#                                      cardiac_arrest_date_time, 
#                                      cardiac_arrest_initial_cpr_date_time, 
#                                      cardiac_arrest_rosc_date_time)

#compare_dates(alb_datetimes, comp_cols)

#
#
# Duplication --------------------------------------------------- 
#
#

## 27 full duplicate rows
length(which(duplicated(cville_data)))

## 20392 duplicated incident numbers
length(which(duplicated(cville_data$response_incident_number)))
#cville_data$response_incident_number[which(duplicated(cville_data$response_incident_number))]

## 29517 unique incident numbers
length(unique(cville_data$response_incident_number))

## Function to tally number of duplicate entries by column
my_dup_fun <- function(x) {
  return(sum(as.numeric(duplicated(x))))
}

## Compute duplicate entries in each column for each incident number
setDT(cville_data)
dups <- cville_data[, lapply(.SD, my_dup_fun), by = response_incident_number]

## Total number of duplicate values for each duplicate incident number
#apply(dups[,-1], 1, sum)


