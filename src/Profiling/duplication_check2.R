library(data.table)
library(here)
library(dplyr)
library(ggplot2)

data <- fread(here("data", "working", "remaining_duplicates.csv"))

## Pull out the ages where they are inconsistent
## 296 incidents, 656 rows
temp_age <- data %>% group_by(response_incident_number, response_ems_unit_call_sign) %>% 
  mutate(N = length(unique(patient_age))) %>% 
  filter(N > 1) %>%
  select(patient_age, patient_gender)

## Pull out genders where inconsistent
## 150 incidents, 344 rows
temp_sex <- data %>% group_by(response_incident_number, response_ems_unit_call_sign) %>% 
  mutate(N = length(unique(patient_gender))) %>% 
  filter(N > 1) %>%
  select(patient_age, patient_gender)

## 308 total incidents with errors after accounting for overlap
length(unique(c(temp_age$response_incident_number, temp_sex$response_incident_number)))

age_err <- setdiff(unique(temp_age$response_incident_number), unique(temp_sex$response_incident_number)) ## Incidents where age is inconsistent but gender is fine
gender_err <- setdiff(unique(temp_sex$response_incident_number), unique(temp_age$response_incident_number)) ## Inicdents where gender is inconsistent but age is fine

## Incidents where age is inconsistent but gender is fine
data %>% filter(response_incident_number %in% age_err) %>% 
  select(response_incident_number, patient_age, patient_gender)

## Inicdents where gender is inconsistent but age is fine
data %>% filter(response_incident_number %in% gender_err) %>% 
  select(response_incident_number, patient_age, patient_gender)

#
#
# ----------------------------------------------------------------
#
#

setDT(data)
data[, incons_N := sum(as.numeric(!(.SD[1,] %in% .SD[2,]))), by = .(response_incident_number, response_ems_unit_call_sign)]

data[incons_N == 1]

## Function to help id columns that are often different despite other columns being duplicated
## input_N is the number of columns that have errors (i.e. if you want to subset to all cases where there is only 1 inconsistent column, set input_N = 1)
id_near_duplicates <- function(data, input_N = 1, order = 1) {
  
  test_nums <- data[incons_N == input_N][, response_incident_number]
  
  ## Initialize loop variables
  storage <- list()
  i <- 1
  
  ## Compare rows within incident numbers
  for (num in test_nums) {
    data_sub <- data[response_incident_number == num]
    
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
  colnames(df) <- colnames(data)
  
  ## number of times each column is a culprit in the near-duplication
  temp <- data.frame(num_incidents = colSums(df == 0))
  temp$variable <- row.names(temp)
  temp$num_errors <- input_N
  
  row.names(temp) <- seq(1:nrow(temp))
  temp <- temp %>% select(variable, num_incidents, num_errors)
  
  return(temp)
  
}

## Iterate through different possible numbers of inconsistent columns and 
## use id_near_duplicates function to calculate which columns are the offenders for each
storage <- list()

for (i in seq(1:10)) {
  storage[[i]] <- id_near_duplicates(data, input_N = i, order = 1)
}

test <- do.call(rbind, storage)
test$num_errors = factor(test$num_errors, levels=levels(as.factor(data$incons_N)))

## Plot to help see which columns seem to be erroneous together
ggplot(test %>% filter(num_errors != 0)) +
  geom_bar(aes(x = variable, y = num_incidents), stat = "identity") +
  coord_flip() + 
  facet_grid(~num_errors) +
  labs(title = "Instances in which each variable is inconsistent, grouped by number of inconsistencies for a given incident")



