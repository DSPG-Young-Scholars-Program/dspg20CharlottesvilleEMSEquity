
#
#
# Synthetic Population Generation ---------------------------------------------------------------------------------------------------
#
#

## This script generates a synthetic population using iterated proportional fitting (IPF). The goal is to generate a synthetic population with individual
## latitude/longitude values that will allow us to estimate the values for ACS variables at geographies other than those provided by the census.

## Path to IPF functions
source(here::here("src", "IPF", "03_ipf_functions.R"))

## ----- Marginal Input Prep ----- ##

## Here we set up the inputs required for the functions in 03_ipf_functions.R

## Each variable needs information about the table it comes from in the ACS, its possible values, the columns in the ACS table where these values are obtained,
## how the variable is coded, etc.

## ------ Sex ------ ##

## Specify inputs for a categorical variable in the ACS (Sex)
input_sex <- list()
input_sex$tablecode <- "B01001" # Sex by Age
input_sex$acs_names <- c("male","female") ## Categories in marginal table - they have already been calcualted manually in 02_construct_marginals.R

## Column numbers in the ACS table
## Use acs.lookup() to see list of column names - can extract indices manually
input_sex$acs_cols <- list()
input_sex$acs_cols[[1]] <- 2 ## male column
input_sex$acs_cols[[2]] <- 26 ## female column

## Variable name and type in microdata
input_sex$micro_name <- "SEX"
input_sex$micro_type <- "categorical"

## How the groups are coded in the microdata
input_sex$micro_ids <- list() 
input_sex$micro_ids[[1]] <- 1 ## male
input_sex$micro_ids[[2]] <- 2 ## female

## ------ Age ------ ##

## Specify inputs for a continuous variable in the ACS (Age)
input_age <- list()
input_age$tablecode <- "B01001" # Sex by Age
input_age$acs_names <- c("age_0_24","age_25_50","age_50_75","age_75_up") ## Categories in the marginal table

## Column numbers in the ACS table
## Use acs.lookup() to see list of column names - can extract indices manually
input_age$acs_cols <- list()
input_age$acs_cols[[1]] <- c(3:10, 27:34)  ## all male/female columns for ages under 25
input_age$acs_cols[[2]] <- c(11:15, 35:39) ## all male/female columns for ages between 25-50
input_age$acs_cols[[3]] <- c(16:22, 40:46) ## all male/female columns for ages between 50-75
input_age$acs_cols[[4]] <- c(23:25, 47:49) ## all male/female columns for ages between 75-100

## Variable name and type in microdata
input_age$micro_name <- "AGEP"
input_age$micro_type <- "continuous"

## Breaks for aggregating continuous microdata variable
input_age$micro_breaks <- c(-Inf, 25, 50, 75, Inf)

## ------ Race ------ ##

## Specify inputs for a categorical variable in the ACS (race)
input_race <- list()
input_race$tablecode <- "B02001" # Basic race table code
input_race$acs_names <- c("white", "black", "asian", "other") ## Columns in the marginal table - too few counts for some categories so had to lump under other

## Column numbers for these groups in the ACS table
## Use acs.lookup() to see list of column names - can extract indices manually
input_race$acs_cols <- list() 
input_race$acs_cols[[1]] <- 2 ## white
input_race$acs_cols[[2]] <- 3 ## black
input_race$acs_cols[[3]] <- 5 ## asian
input_race$acs_cols[[4]] <- c(4,6,7,8) ## other

## Variable name in the microdata
input_race$micro_name <- "RAC1P"

## Type of variable (categorical or continuous)
input_race$micro_type <- "categorical"

## ACS coding scheme for the variables (use PUMS data dictionary for reference)
input_race$micro_ids <- list()
input_race$micro_ids[[1]] <- 1 ## white
input_race$micro_ids[[2]] <- 2 ## black
input_race$micro_ids[[3]] <- 6 ## asian
input_race$micro_ids[[4]] <- c(3,4,5,7,8,9) ## other

## Combine into one input list
inputs <- list(input_sex,input_age,input_race)

## ----- Run IPF with actual ACS estimates ----- ##

## Read in the marginal data (calculated using ACS tables in 02_construct_marginals.R) and microdata (obtained from census in 01_read_microdata.R)
all_marginals <- read.csv(here::here("data", "working", "marginals_for_ipf.csv"))
microdata <- read.csv(here::here("data", "working", "acs_microdata_ipf.csv"))

## Run IPF
ipf_counts <- run_ipf(all_marginals, inputs)

## Categorize each entry in the microdata based on which categories it belongs to for our variables of interest
microdata_category <- create_micro_categories(microdata, inputs=inputs, micro_cols = c("PUMA", "PWGTP"))

## Sample microdata based on the marginal counts for each unique set of variable combinations
synth_pop <- resample_ipf(ipf_counts, inputs, microdata, microdata_category, micro_cols = c("PUMA", "PWGTP"))

## Attach latitude and longitude to the synthetic population
cville_block_groups <- block_groups(state = "VA", county = "Charlottesville city", year = 2018)
synth_pop_latlong <- attach_latlong(synth_pop, method="uniform", geographies = cville_block_groups)


## ----- Run IPF iteratively ----- ##

## This is clunkiest, stupidest function I may have ever written, but for now it works.
## It takes in ACS marginal data, samples based on reported estimates and moes, and outputs a dataframe of sampled values
## Each dataframe in the list has an entry for each block group with estimated values for all variables
sample_marginals <- function(marginal_data) { ## Marginal data should have a GEOID and estimates for total population and moe. Should be ordered such that moe columns immediately follow the estimate column 
  
  ## list to store various values
  vars_split <- list()
  samps <- list()
  results <- list()
  
  ## Convert marginal data to list separating each variable/GEOID combo into its own item with estimate and moe
  ## For each pair of columns (estimate and moe), place into storage list
  rownames(marginal_data) <- marginal_data$GEOID
  i <- 1
  
  for (row in 1:nrow(marginal_data)) {
    for (col in seq(2,ceiling(ncol(marginal_data)), 2)) {
      vars_split[[i]] <- unclass(marginal_data[row, c(col,col+1)])
      i <- i + 1
    }
  }
  
  ## Iterate through list, use moe and estimate to sample new value from normal distribution
  for (i in 1:length(vars_split)) {
    var <- attributes(vars_split[[i]])$names[1]
    geoid <- attributes(vars_split[[i]])$row.names
    
    mean <- vars_split[[i]][[1]]
    se <- vars_split[[i]][[2]] / 1.645
    
    samps[[i]] <- rnorm(1, mean, se)
    attributes(samps[[i]])$row.names <- geoid
    attributes(samps[[i]])$var <- var
  }
  
  ## Convert to list of dataframes
  var_names <- unlist(lapply(samps, function(x) attributes(x)$var))
  geo_names <- unlist(lapply(samps, function(x) attributes(x)$row.names))
  
  dfs <- lapply(samps, function(x) as.data.frame(x))
  
  ## Attach variable names and GEOIDs
  for (i in seq_along(dfs)){
    colnames(dfs[[i]]) <- var_names[i]
    dfs[[i]]$GEOID <- geo_names[i]
  }
  
  ## Combine dataframes into single source
  for (geo_idx in seq(1, length(vars_split), by = 11)) {
    test <- purrr::reduce(dfs[seq(geo_idx, geo_idx+10)], cbind)
    test <- test[,!duplicated(colnames(test))]
    results[[geo_idx]] <- test
  }
  
  sampled_marginals <- bind_rows(results)
  
  ## Convert negative counts to 0
  sampled_marginals <- replace(sampled_marginals, sampled_marginals < 0, 0) %>% select(GEOID, everything())
  
  return(sampled_marginals)
}

all_marginals_moe <- read.csv(here::here("data", "working", "marginals_for_ipf_moe.csv"))

## Categorize each entry in the microdata based on the categories of the variables we are using for IPF
microdata_category <- create_micro_categories(microdata, inputs = inputs, micro_cols = c("PUMA", "PWGTP"))


## Get multiple marginal samples (each will include estimates for all variables for each block group)
set.seed(5256)

n_iter <- 100
sample_results <- list()

for (i in 1:n_iter) {
  print(i)
  sampled_marginals <- sample_marginals(all_marginals_moe)
  sample_results[[i]] <- sampled_marginals
}

## Run IPF on each set of sampled data
ipf_results <- lapply(sample_results, function(x) run_ipf(x, inputs, prob = TRUE))

## Sample microdata based on the counts for each unique set of variable combinations, do this for each set of sampled data
synth_pops <- lapply(ipf_results, function(x) resample_ipf(x, inputs, microdata, microdata_category, micro_cols = c("PUMA", "PWGTP")))

## Attach latitude and longitude to the synthetic population for each set of sampled data
cville_block_groups <- block_groups(state = "VA", county = "Charlottesville city", year = 2018)
synth_pops_latlong <- lapply(synth_pops, function(x) attach_latlong(x, method = "uniform", geographies = cville_block_groups))

# saveRDS(synth_pops_latlong, file = here::here("data", "working", "synthetic_population_samples.rds"))
