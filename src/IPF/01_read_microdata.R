
library(httr)
library(jsonlite)
library(dplyr)

## Request microdata from census API
## Resulting output can be found in data > working > acs_microdata_ipf.csv

## PUMAs for Charlottesville + Albemarle: 51089, 51090 (Thomas Jefferson Planning Districts North + South)

## API request for microdata for Cville + Albemarle PUMAs
req <- GET("https://api.census.gov/data/2018/acs/acs5/pums?get=PWGTP,SEX,RAC1P,AGEP&ucgid=7950000US5151089,7950000US5151090")

## Convert request to dataframe
req_df <- as.data.frame(fromJSON(content(req, as = "text")))

## Update column names and convert 
colnames(req_df) <- req_df[1,]

## Remove header column from data onvert to correct format
acs_microdata <- req_df[-1,] 

# readr::write_csv(acs_microdata, here::here("data", "working", "acs_microdata_ipf.csv"))
