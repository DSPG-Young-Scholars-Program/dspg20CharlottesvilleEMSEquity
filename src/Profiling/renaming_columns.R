library(vroom)
library(here)
library(dplyr)

albemarle <- read.csv2(here("data/original/Data4UVA.csv"), fileEncoding="UTF-16LE", sep = ",", na.strings = "")
charlottesville <- readxl::read_xlsx(here("data/original/CFD_CARS_EMS_DATA_121616TO60920.xlsx"), 1)


albemarle %>%
  rename_with(~tolower(gsub(r"(\.\..*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(r"(\.)", "_", .x)) %>% # change periods to underscores
  names()


charlottesville %>%
  rename_with(~tolower(gsub(r"( \(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(" ", "_", .x)) %>% # change spaces to underscores
  names()
