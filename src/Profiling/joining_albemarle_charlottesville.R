library(dplyr)
library(here)
library(purrr)


albemarle <- readxl::read_xlsx(here("data","original","Data4UVA.xlsx"), 1, col_types = c(rep("text", 4), "date", rep("text", 78))) %>%
  rename_with(~tolower(gsub(r"( +\(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(r"( )", "_", .x))  # change periods to underscores

charlottesville <- readxl::read_xlsx(here("data","original","CFD_CARS_EMS_DATA_121616TO60920.xlsx"), 1) %>%
  rename_with(~tolower(gsub(r"( +\(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(" ", "_", .x)) # change spaces to underscores

# these variables probably mean the same thing, so I'll just rename the albemarle one
albemarle <- albemarle %>%
  rename(total_unit_response_time = incident_unit_notified_by_dispatch_to_unit_arrived_on_scene_in_minutes)


ems_full <- bind_rows(mutate(albemarle, source = "albemarle"),
                      mutate(charlottesville, source = "charlottesville")) # tag on source variable


