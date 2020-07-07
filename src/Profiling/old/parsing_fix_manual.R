
library(data.table)
library(stringr)
library(dplyr)
library(here)
library(tidyr)
library(gsubfn)

## Read in data for county and city
#alb_data <- read.csv(here("data", "original", "Data4UVA.csv"), fileEncoding="UTF-16LE", sep = ",", na.strings = "")
alb_data <- readxl::read_xlsx(here("data", "original", "Data4UVA.xlsx"))

## Standardize column names for each dataset
alb_data <- alb_data %>%
  rename_with(~tolower(gsub(r"( \(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(" ", "_", .x)) # change spaces to underscores

#
#
# Attempt to manually fix parsing issues --------------------------------------------------------------------------------------------------
#
#

## Read in data as raw strings
ar_text <- readLines(con <- file(here("data","original","Data4UVA.csv"), encoding = "UTF-16LE"))
close(con)

## Pull out test row from parsed and raw data for comparison
test_case_alb <- alb_data[50137,]
test_case_lines <- ar_text[c(1,50137)]

## This extracts items that should be grouped as a list by identifying ,\"\" and \"\", as starting and end delimiters
## Then it replaces "," with , within these substrings and places back into original position of substring in the data
test_case_lines[2] <- gsubfn("(,\"\"[^,].*?\"\",)", function(g1) gsub("\",\"", ",", g1, fixed=TRUE), test_case_lines[2])

## Then convert double quotes at beginning and end of list to single quotes
test_case_lines[2] <- gsubfn("(,\"\"[^,].*?\"\",)", function(g1) gsub("\"\"", "\"", g1, fixed=TRUE), test_case_lines[2])

## To test, write to csv file and re-read
## It works for the test case here
writeLines(test_case_lines, here("data", "county_data_TEST.csv"))
test <- read.csv(here("data", "county_data_TEST.csv"))

## Try this technique on entire dataset
test_lines <- ar_text

test_lines <- gsubfn("(,\"\"[^,].*?\"\",)", function(g1) gsub("\",\"", ",", g1, fixed=TRUE), test_lines)
test_lines <- gsubfn("(,\"\"[^,].*?\"\",)", function(g1) gsub("\"\"", "\"", g1, fixed=TRUE), test_lines)

## Probably should delete the test csv file when finished!
writeLines(test_lines, here("data", "county_data_TEST.csv"))
test <- read.csv(here("data", "county_data_TEST.csv"), na.strings = "")

## We get other errors when trying on whole dataset. There may be a pattern to these errors that could be corrected for, but I haven't
## had the chance to look into it yet.


