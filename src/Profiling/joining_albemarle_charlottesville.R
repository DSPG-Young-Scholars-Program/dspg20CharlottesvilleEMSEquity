library(dplyr)
library(here)
library(purrr)


albemarle <- read.csv(here("data","original","Data4UVA.csv"),
                       fileEncoding="UTF-16LE",
                       sep = ",", na.strings = "",
                      colClasses = "character") %>%
  rename_with(~tolower(gsub(r"(\.\..*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(r"(\.)", "_", .x)) %>%  # change periods to underscores
  mutate(incident_date = lubridate::ymd(incident_date))
charlottesville <- readxl::read_xlsx(here("data","original","CFD_CARS_EMS_DATA_121616TO60920.xlsx"), 1) %>%
  rename_with(~tolower(gsub(r"( +\(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(" ", "_", .x)) %>% # change spaces to underscores
  mutate(across(c(1:4, 6:83), as.character))
# can we just col_bind these dataframes?

# ncol(albemarle)
# ncol(charlottesville)

# there are the same number of rows

# do they match names?
# which(names(albemarle) != names(charlottesville))


# names(albemarle)[82]
# names(charlottesville)[82]

# these variables mean the same thing, so I'll just rename the albemarle one
albemarle <- albemarle %>%
  rename(total_unit_response_time = incident_unit_notified_by_dispatch_to_unit_arrived_on_scene_in_minutes)


# which(names(albemarle) != names(charlottesville))

# looks like we caught all the name mismatches



# sum(map_chr(albemarle, class) != map_chr(charlottesville, ~(class(.x)[1]))) # extract 1st class of dates
# 36 class mismatches is a lot!
# looks like albemarle is all characters, charlottesville tried to guess a lot.
# due to the data in the wrong columns in albemarle, I'm going to set charlottesville to all characters

ems_full <- bind_rows(mutate(albemarle, source = "albemarle"),
                      mutate(charlottesville, source = "charlottesville")) # tag on source variable

# check it worked:
# nrow(ems_full) == nrow(albemarle) + nrow(charlottesville)

