
library(data.table)
library(naniar)
library(visdat)
#library(stringr)
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

## Columns to check: 
## incident_date, cardiac_arrest_date_time, cardiac_arrest_initial_cpr_date_time, cardiac_arrest_rosc_date_time, incident_psap_call_date_time

## Duration columns
## incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes, incident_unit_notified_by_dispatch_to_unit_arrived_on_scene_in_minutes

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


# ------------------------------------------------------------------------------------------------------------------------------

## All date time columns
cville_datetimes <- cville_data %>% select(incident_date, 
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
comp_cols <- c("incident_date",
               "incident_psap_call_date_time",
               "cardiac_arrest_date_time",
               "cardiac_arrest_initial_cpr_date_time",
               "cardiac_arrest_rosc_date_time")

## ID inconsistent dates
date_errs <- compare_dates(cville_datetimes, comp_cols)
date_errs






## Next confirm that times make sense - not just dates

## Then figure out what the hell is going on with county data


## 21 cases where cardiac arrest happened on different date than the call was made?
cville_datetimes %>% 
  filter(as.Date(incident_date) != as.Date(cardiac_arrest_date_time))



#-----------------

## County data - won't work until the file errors are fixed...
alb_datetimes <- alb_data %>% select(incident_date, 
                                           incident_psap_call_date_time, 
                                           cardiac_arrest_date_time, 
                                           cardiac_arrest_initial_cpr_date_time, 
                                           cardiac_arrest_rosc_date_time)

## Confirm all datetime columns have same intial date as overall incident date
which(as.Date(alb_datetimes$incident_date) != as.Date(alb_datetimes$incident_psap_call_date_time))
which(as.Date(alb_datetimes$incident_date) != as.Date(alb_datetimes$cardiac_arrest_date_time))
which(as.Date(alb_datetimes$incident_date) != as.Date(alb_datetimes$cardiac_arrest_initial_cpr_date_time))
which(as.Date(alb_datetimes$incident_date) != as.Date(alb_datetimes$cardiac_arrest_rosc_date_time))



