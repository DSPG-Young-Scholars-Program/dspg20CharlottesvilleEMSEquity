
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

#data[incons_N == 1]

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

for (i in seq(1:9)) {
  storage[[i]] <- id_near_duplicates(data, input_N = i, order = 1)
}

test <- do.call(rbind, storage)
test$num_errors = factor(test$num_errors, levels=levels(as.factor(data$incons_N)))

test <- test %>% 
  group_by(num_errors) %>% 
  mutate(count = sum(num_incidents), prop = as.numeric(num_incidents) / count) 

## Plot to help see which columns seem to be erroneous together
ggplot(test) +
  geom_bar(aes(x = variable, y = prop), stat = "identity") +
  coord_flip() + 
  facet_grid(~num_errors) +
  labs(title = "Instances in which each variable is inconsistent, grouped by number of inconsistencies for a given incident")

#
#
# Heatmaps ---------------------------------------------------------------------------------------------------------
#
#

# data <- fread(here("data", "working", "remaining_duplicates.csv"))
# 
# ## Select only doubles not multiples
# data <- data %>% 
#   group_by(response_incident_number, response_ems_unit_call_sign) %>% 
#   mutate(count = n()) %>% 
#   filter(count == 2) %>% 
#   select(-count) %>%
#   ungroup()
# 
# setDT(data)
# data[, incons_N := sum(as.numeric(!(.SD[1,] %in% .SD[2,])), na.rm=TRUE), by = .(response_incident_number, response_ems_unit_call_sign)]
# 
# ## Convert to binary matrix
# id_near_duplicates2 <- function(data, input_N = 1, order = 1) {
#   
#   test_nums <- data[incons_N == input_N][, response_incident_number]
#   
#   ## Initialize loop variables
#   storage <- list()
#   i <- 1
#   
#   ## Compare rows within incident numbers
#   for (num in test_nums) {
#     data_sub <- data[response_incident_number == num]
#     
#     if (order == 1) {
#       vec <- data_sub[1,] %in% data_sub[2,]
#     } else if (order == 2) {
#       vec <- data_sub[2,] %in% data_sub[1,]
#     }
#     
#     storage[[i]] <- as.numeric(vec)
#     i <- i + 1
#   }
#   
#   ## Combine into df
#   names(storage) <- test_nums
#   df <- do.call(rbind, storage)
#   colnames(df) <- colnames(data)
#   
#   temp <- df
#   
#   # ## number of times each column is a culprit in the near-duplication
#   # temp <- data.frame(num_incidents = colSums(df == 0))
#   # temp$variable <- row.names(temp)
#   # temp$num_errors <- input_N
#   # 
#   # row.names(temp) <- seq(1:nrow(temp))
#   # temp <- temp %>% select(variable, num_incidents, num_errors)
#   
#   return(temp)
#   
# }
# 
# test <- as.matrix(id_near_duplicates2(data, input_N = 4, order = 1))
# 
# ## Just formatting to allow to subset columns with no errors
# rel_cols <- data.frame(any_discrep = colSums(test) != nrow(test))
# rel_cols$var <- row.names(rel_cols)
# row.names(rel_cols) <- NULL
# cols_wanted <- rel_cols %>% filter(any_discrep)
# test <- test[, cols_wanted$var]
# 
# ## Heatmap
# heatmap(test, margin = c(25, 10), scale = "none")

#
#
# Calculate frequency of errors in each variable pair -----------------------------------------------------------------
#
#

# data <- as.data.frame(data)
# 
# all_vars <- colnames(data[2:ncol(data)])
# 
# ## all variable combinations not including incident number
# #var_combs <- t(combn(all_vars, 2))
# var_combs <- permutations(n=59,r=2,v=all_vars,repeats.allowed=F)
# 
# ## storage data frame
# #storage <- data.frame(variable1 = var_combs[,1], variable2 = var_combs[,2], errors = NA)
# storage <- expand.grid(variable1 = all_vars, variable2 = all_vars, errors = NA)
# 
# ## Iterate through all pairs and calculate number of column discrepancies
# for (i in seq(1:nrow(var_combs))) {
# 
#   var1 <- var_combs[i,1]
#   var2 <- var_combs[i,2]
# 
#   ## Extract relevant variable pair
#   temp <- data[, c("response_incident_number", var1, var2)]
# 
#   ## Determine whether there are duplicates in variables
#   setDT(temp)
#   temp[, c("N1", "N2") := .(uniqueN(get(var1)), uniqueN(get(var2))), by = response_incident_number]
# 
#   ## Calculate whether both variables simultaneously had mismatches
#   temp <- temp %>%
#     as.data.frame() %>%
#     group_by(response_incident_number) %>%
#     summarize(err1 = max(N1), err2 = max(N2)) %>%
#     mutate(err_both = (err1 != 1) & (err2 != 1))
# 
#   ## Add up instances of error in both variables and store back in dataframe
#   num_errs <- sum(as.numeric(temp$err_both))
# 
# 
#   storage[which(storage$variable1 == var1 & storage$variable2 == var2),]$errors <- num_errs
# 
# }
#
# readr::write_csv(storage, here("data", "working", "near_duplicate_tracking.csv"))

dup_errors <- readr::read_csv(here("data", "working", "near_duplicate_tracking.csv"))

ggplot(dup_errors) +
  geom_tile(aes(x=variable1, y=variable2, fill = errors)) +
  scale_fill_viridis(na.value = "gray20") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
