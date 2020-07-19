
library(httr)
library(jsonlite)
library(dplyr)

## PUMAs for Charlottesville + Albemarle: 51089, 51090 (Thomas Jefferson Planning Districts North + South)

## API request for microdata for Cville + Albemarle PUMAs
req <- GET("https://api.census.gov/data/2018/acs/acs5/pums?get=PWGTP,SEX,RAC1P,AGEP&ucgid=7950000US5151089,7950000US5151090")

## Convert request to dataframe
req_df <- as.data.frame(fromJSON(content(req, as = "text")))

## Update column names and convert 
colnames(req_df) <- req_df[1,]

## Recode columns for clarity and convert to correct format
acs_microdata <- req_df[-1,] %>% 
  mutate(SEX = recode(SEX, "1" = "Male", "2" = "Female"),
         RAC1P = recode(RAC1P, "1" = "White alone", "2" = "Black or African American alone", "3" = "American Indian alone", "4" = "Alaska Native alone", 
                        "5" = "American Indian and Alaskan native tribes specified; or American Indian or Alaskan native, not specified and no other race",
                        "6" = "Asian alone", "7" = "Native Hawaiian and other Pacific Islander alone", "8" = "Some other race alone", "9" = "Two or more races"),
         AGEP = as.numeric(AGEP),
         PWGTP = as.numeric(PWGTP))

# readr::write_csv(acs_microdata, here::here("data", "working", "acs_microdata_ifp.csv"))
