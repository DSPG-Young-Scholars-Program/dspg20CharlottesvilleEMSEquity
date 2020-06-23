
library(data.table)
library(stringr)
library(dplyr)
library(here)
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
# Duplication --------------------------------------------------- 
#
#

## Drop columns that likely won't be used. Can be updated as we determine which are actually necessary
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

# ---------------

## Remove 4220 full duplicates
length(which(duplicated(cville_sub)))
cville_sub <- cville_sub[!duplicated(cville_sub),]

## There are 10819 distinct incident numbers still have duplicates
## 7919 are doubles, 2900 have more than 2 duplicates
duplicates <- cville_sub[, .N, by = response_incident_number]

double_entries <- duplicates[N == 2]
multi_entries <- duplicates[N > 2]

## Split into doubles and multi-duplicates since these will likely have to be handled differently
cville_doubles <- cville_sub[response_incident_number %in% double_entries$response_incident_number]
cville_multi <- cville_sub[response_incident_number %in% multi_entries$response_incident_number]

# ---------------

## Duplicates may exist when multiple units were sent to scene and each recorded data
## Among doubled incident numbers, check if the call sign is also duplicated:
cville_doubles[, dup_sign := (uniqueN(response_ems_unit_call_sign) == 1), by = response_incident_number]

## 5023 have different call signs - this may account for the duplication in these rows?
## May want to check with Damon/Lucas to make sure this is plausible
length(cville_doubles[dup_sign == FALSE, unique(response_incident_number)])

## That leaves 2896 doubles with call signs not the culprit of duplication and 2900 multi-duplicates
## Peek at some of the remaining doubles
#cville_doubles[dup_sign == TRUE][1:2]

## Check whether one of the rows in duplicate pairs is entirely contained within another
## 0 values should indicate we can safely select the one with more data
compare_rows1 <- cville_doubles[dup_sign == TRUE][, inconsistencies := sum(as.numeric(!(.SD[1,] %in% .SD[2,]))), by = response_incident_number]
compare_rows2 <- cville_doubles[dup_sign == TRUE][, inconsistencies := sum(as.numeric(!(.SD[2,] %in% .SD[1,]))), by = response_incident_number]

compare_rows1[inconsistencies==0] ## 1387 likely can be merged safely - just a matter of selecting the row with more info/fewer NA
compare_rows2[inconsistencies==0] ## 63 can likely be merged safely - just a matter of selecting the row with more info/fewer NA

## If these can be merged, it would leave 1446 doubles left to deal with:
test_nums <- intersect(unique(compare_rows1[inconsistencies > 0]$response_incident_number), 
                       unique(compare_rows2[inconsistencies > 0]$response_incident_number))


# ---------------

## Function to help id columns that are often different despite other columns being duplicated
## Not sure if this is really helpful but leaving it here for now
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

## Get data subset to investigate. Here we look at all cases where there is one inconsistent column
nums <- compare_rows1[inconsistencies==1]$response_incident_number
id_near_duplicates(nums)

