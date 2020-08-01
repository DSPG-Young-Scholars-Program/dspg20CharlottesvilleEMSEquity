
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



## ----- Run IPF Once (To confirm things are working) ----- ##

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
synth_pop_latlong <- attach_latlong(synth_pop, method="uniform", state_names = "VA", county_names = c("Charlottesville", "Albemarle"), year = 2018)


## ----- Run IPF iteratively ----- ##

## This will require modifications to the functions in 03_ipf_functions.R

## I made one attempt, but wasn't able to get reasonable results, and given the timeframe for this project just had to rever to the basic
## ACS estimates. A more solid attempt could be made to integrate those functions into a sampling workflow, though.