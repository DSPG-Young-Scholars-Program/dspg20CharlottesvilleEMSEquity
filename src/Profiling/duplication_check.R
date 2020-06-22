
library(data.table)
library(naniar)
library(visdat)
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
#as.data.table()

# alb_sub <- alb_data[str_detect(alb_data$response_incident_number, "[:alpha:]") & !is.na(alb_data$response_incident_number),]
# 
# city_complaints <- levels(as.factor(cville_data$incident_complaint_reported_by_dispatch))
# 
# test <- alb_data[!(alb_data$incident_complaint_reported_by_dispatch %in% city_complaints) & !is.na(alb_data$incident_complaint_reported_by_dispatch),]

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
date_errs_cville <- compare_dates(cville_data, comp_cols)

# ------------------------------------------------------------------------------------------------------------------------------

## Checking time logic (as opposed to date logic)
test <- cville_datetimes %>% 
  filter(!is.na(cardiac_arrest_date_time)) %>% 
  select("incident_psap_call_date_time",
         "cardiac_arrest_date_time") %>%
  mutate(time_diff = difftime(cardiac_arrest_date_time, incident_psap_call_date_time))

# 528 cases have a cardiac arrest before the incident is reported. How is the cardiac arrest time being recorded?
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

# ## Incident dates that fail to parse 
# parse_fails <- which(is.na(ymd(as.character(alb_data$incident_date), tz="UTC")))
# 
# ## The entire rows for these observations are weird. I wonder if it is a data reading issue/something to do with encoding?
# ## It looks like these rows are just shifted to the right or left. As if there was a missing comma on the end of the line or something.
# error_rows <- alb_data[parse_fails,]
# 
# ## Error rows either completely populated with nonsense or filled with nearly all NAs
# # vis_miss(error_rows)
# 
# ## Quick check to see if these are consecutive entries.
# values <- vector()
# 
# for (i in seq_along(parse_fails)) {
#   val <- parse_fails[i+1] - parse_fails[i]
#   values <- append(values, val)
# }
# 
# values ## Looks like there is a long sequence of errors, but that's not the only thing that's going on.
# 
# ## Comparing dates - won't wory until data recording errors are fixed
# alb_datetimes <- alb_data %>% select(incident_date,
#                                      incident_psap_call_date_time,
#                                      cardiac_arrest_date_time,
#                                      cardiac_arrest_initial_cpr_date_time,
#                                      cardiac_arrest_rosc_date_time)
# 
# compare_dates(alb_datetimes, comp_cols)

#
#
# Duplication --------------------------------------------------- 
#
#

to_drop <- c("patient_medical_history_obtained_from_list", 
             "patient_advance_directives_list",
             "cardiac_arrest_witnessed_by_list",
             "injury_cause_of_injury",
             "outcome_external_report_type",
             "outcome_external_report_number",
             "cardiac_arrest_initial_cpr_date_time",
             "cardiac_arrest_who_initiated_cpr_with_code",
             "medication_given_description_and_rxcui_code",
             "patient_medication_given_descriptions_list",
             "cardiac_arrest_rosc_date_time",
             "destination_cardiac_arrest_team_activation_date_time",
             "patient_weight_actual_or_estimate_pounds",
             "injury_mechanism_of_injury_list",
             "cad_crew_member_full_name_and_level_list",
             "patient_suspected_influenza_type_illness",
             "patient_mental_status_assessment_exam_details",
             "patient_neurological_assessment_exam_details",
             "patient_skin_assessment_exam_details",
             "patient_head_assessment_exam_details",
             "patient_respiratory_effort_list_")
             
cville_sub <- cville_data %>% select(-to_drop)

setDT(cville_sub)

## Remove 4220 full duplicates
length(which(duplicated(cville_sub)))
cville_sub <- cville_sub[!duplicated(cville_sub),]

## 10819 distinct incident numbers have duplicates
duplicates <- cville_sub[, .N, by = response_incident_number]

## 7919 are doubles, 2900 have more than 2 duplicates
double_entries <- duplicates[N == 2]
multi_entries <- duplicates[N > 2]

cville_doubles <- cville_sub[response_incident_number %in% double_entries$response_incident_number]
cville_multi <- cville_sub[response_incident_number %in% multi_entries$response_incident_number]

## This gives us a record of whether the call signs match for each doubled entry
cville_doubles[, dup_sign := (uniqueN(response_ems_unit_call_sign) == 1), by = response_incident_number]

## 5023 have different call signs - this may account for the duplication here?
length(cville_doubles[dup_sign == FALSE, unique(response_incident_number)])

## That leaves 2896 doubles with call signs not the culprit of duplication and 2900 multi-duplicates
## Peek at some of the remaining doubles
cville_doubles[dup_sign == TRUE][1:2]

## See whether one of the duplicate rows is entirely contained within another
## 0 values indicate we can safely select the one with more data(?)
test1 <- cville_doubles[dup_sign == TRUE][, sum(as.numeric(!(.SD[1,] %in% .SD[2,]))), by = response_incident_number]
test2 <- cville_doubles[dup_sign == TRUE][, sum(as.numeric(!(.SD[2,] %in% .SD[1,]))), by = response_incident_number]

test1[V1==0] ## 1387 likely can be merged safely - just a matter of selecting the row with more info/fewer NA
test2[V1==0] ## 63 can likely be merged safely - just a matter of selecting the row with more info/fewer NA

## 1446 doubles left to deal with
test_nums <- test1[V1==1]$response_incident_number

look <- cville_doubles %>% filter(response_incident_number %in% test_nums)


## Function to help id columns that are often different despite other columns being duplicated
## Not sure if this is really helpful. We still need to make a call about how we're going to deal with near duplicates
id_near_duplicates <- function(test_nums, order = 1) {
  
  ## Initialize loop variables
  storage <- list()
  i <- 1
  
  ## Compare rows within incident numbers
  for (num in test_nums) {
    data_sub <- cville_doubles[response_incident_number == num]
    
    if (order == 1) {
      vec <- data_sub[1,] %in% data_sub[2,]
    } else if (order == 2) {
      vec <- data_sub[2,] %in% data_sub[1,]
    }
    
    storage[[i]] <- as.numeric(vec)
    i <- i + 1
  }
  
  ## Combine into df
  names(storage) <- test_nums
  df <- do.call(rbind, storage)
  colnames(df) <- colnames(cville_doubles)
  
  ## number of times each column is a culprit in the near-duplication
  return(colSums(df == 0))
  
}

nums <- test1[V1==1]$response_incident_number

id_near_duplicates(nums)

cville_doubles[dup_sign == TRUE]

cville_doubles




#
#
# Uniqueness ----------------------------------------------------------------------------------------------------------------
#
#

## Should be re-done after removing duplicates

glimpse(cville_sub)

chars <- cville_sub %>% select_if(is.character) %>% select(-response_incident_number)

char_summary <- as.data.frame(apply(chars, 2, function(x) length(unique(x[!is.na(x)]))))
colnames(char_summary) <- "num_unique"

ggplot(char_summary) + 
  geom_bar(aes(x=row.names(char_summary), y = num_unique), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##

numerics <- cville_sub %>% select(which(sapply(.,class)=="numeric"))
df1 <- as.data.frame(numerics)

## Distributions of numeric variables
ggplot(gather(df1), aes(value)) + 
  geom_boxplot() + 
  facet_wrap(~key, scales = 'free_x')



#######

## Trying to fix parsing issues

test_case_alb <- alb_data[50137,]

ar_text <- readLines(con <- file(here("data","original","Data4UVA.csv"), encoding = "UTF-16LE"))
close(con)

test_case_lines <- ar_text[c(1,50137)]

## This will extract the grouped list and replace "," within that extraction, but not sure how to put back??
str_extract_all(test_case_lines[2], ",\"\"[^,].*?\"\",")

## This may be working! Replaces "," with , but only in the relevant stretches of the string (where items are grouped)
test_case_lines[2] <- gsubfn("(,\"\"[^,].*?\"\",)", function(g1) gsub("\",\"", ",", g1, fixed=TRUE), test_case_lines[2])

## Then replace "" with " within the same substring
test_case_lines[2] <- gsubfn(",\"\"[^,].*?\"\",", function(g1) gsub("\"\"", "\"", g1, fixed=TRUE), test_case_lines[2])

## But it doesn't read back in so it's messing something else up too apparently...?
writeLines(test_case_lines, here("data", "county_data_TEST.csv"))
test <- read.csv(here("data", "county_data_TEST.csv"))


