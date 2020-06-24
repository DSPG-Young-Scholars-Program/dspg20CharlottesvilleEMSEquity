
library(data.table)
library(stringr)
library(dplyr)
library(here)
library(lubridate)
library(ggplot2)
library(tidyr)

## Read in data for county and city
#alb_data <- read.csv(here("data", "original", "Data4UVA.csv"), fileEncoding="UTF-16LE", sep = ",", na.strings = "")
alb_data <- readxl::read_xlsx(here("data", "original", "Data4UVA.xlsx"))
cville_data <- readxl::read_xlsx(here("data", "original", "CFD_CARS_EMS_DATA_121616TO60920.xlsx"))

## Standardize column names for each dataset
alb_data <- alb_data %>%
  rename_with(~tolower(gsub(r"( \(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(" ", "_", .x)) # change spaces to underscores

cville_data <- cville_data %>%
  rename_with(~tolower(gsub(r"( \(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(" ", "_", .x)) #%>% # change spaces to underscores

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
date_errs_cville <- compare_dates(cville_data, comp_cols)

# ------------------------------------------------------------------------------------------------------------------------------

## Checking time logic (as opposed to date logic)
time_compare <- cville_datetimes %>% 
  filter(!is.na(cardiac_arrest_date_time)) %>% 
  select("incident_psap_call_date_time",
         "cardiac_arrest_date_time") %>%
  mutate(time_diff = difftime(cardiac_arrest_date_time, incident_psap_call_date_time))

# 528 cases have a cardiac arrest before the incident is reported. How is the cardiac arrest time being recorded?
length(which(time_compare$time_diff < 0))

# ------------------------------------------------------------------------------------------------------------------------------

## Time duration variable checks ##

## This suggests there are only 3 cases where total unit response is greater than the more restrictive dispatch_notified variable
## Either I'm misunderstanding the variables or something isn't right in the data dictinoary, because this doesn't make logical sense.
cville_data %>% 
  filter(incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes <= total_unit_response_time) %>%
  select(response_incident_number, incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes, total_unit_response_time)


#
#
# County data - needs to wait until parsing issues fixed? --------------------------------------------------- 
#
#

#
#
# Uniqueness ----------------------------------------------------------------------------------------------------------------
#
#

## Should be re-done after removing duplicates

## Get character columns 
chars <- cville_data %>% 
  select_if(is.character) %>% 
  select(-response_incident_number)

## Summarize unique values in these columns
char_summary <- as.data.frame(apply(chars, 2, function(x) length(unique(x[!is.na(x)]))))
colnames(char_summary) <- "num_unique"

## Bar chart for rough idea of unique values by variable
ggplot(char_summary) + 
  geom_bar(aes(x=row.names(char_summary), y = num_unique), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Get numeric columns
numerics <- cville_data %>% 
  select(which(sapply(.,class)=="numeric"))

num_summary <- as.data.frame(numerics)

## Boxplots for a rough idea of distributions
ggplot(gather(num_summary), aes(value)) + 
  geom_boxplot() + 
  facet_wrap(~key, scales = 'free_x')
