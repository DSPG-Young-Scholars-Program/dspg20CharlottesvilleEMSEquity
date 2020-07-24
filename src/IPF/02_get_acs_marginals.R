
library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)

## Here we calculate marginal values to be used in the IPF procedure for Charlottesville and Albemarle (race, age, and sex).
## Resulting output is located in data > working > marginals_for_ipf.csv

## Variable names for merging on column names in tables
v2018subject <- load_variables(2018, dataset = "acs5/subject", cache = TRUE)
v2018 <- load_variables(2018, dataset = "acs5", cache = TRUE)

## Pull age and sex table at tract level (block groups not working currently - will revisit)
age_by_sex <- get_acs(geography = "block group",
                      year = 2018,
                      table = "B01001",
                      state = "VA",
                      county = c("Charlottesville city")) %>%
  #filter(str_detect(GEOID, "51540|51003")) %>%
  left_join(v2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

## Sum counts within age categories to get age marginals. Sex marginals also included as columns already
age_sex_marginals <- age_by_sex %>%
  select(GEOID, !matches("moe")) %>%
  mutate(total = estimate_total_estimate,
         female = estimate_total_female_estimate,
         male = estimate_total_male_estimate) %>%
  group_by(GEOID, total, female, male) %>%
  rowwise() %>%
  summarize(age_0_24 = sum(across(matches("under_5|5_to_9|10_to_14|15_to_17|18_and_19|20_years|21_years|22_to_24"))),
            age_25_50 = sum(across(matches("25_to_29|30_to_34|35_to_39|40_to_44|45_to_49"))),
            age_50_75 = sum(across(matches("50_to_54|55_to_59|60_and_61|62_to_64|65_and_66|67_to_69|70_to_74"))),
            age_75_up = sum(across(matches("75_to_79|80_to_84|85_years_and_over"))))

## Pull race marginal table
race_data <- get_acs(geography = "block group",
                     year = 2018,
                     table = "B02001",
                     state = "VA",
                     county = "Charlottesville city") %>%
  #filter(str_detect(GEOID, "51540|51003")) %>%
  left_join(v2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

## Select relevant estimate columns
race_marginals <- race_data %>% 
  select(GEOID, !matches("moe|excluding|including"), -NAME, -concept) %>%
  group_by(GEOID, estimate_total_estimate, estimate_total_white_alone_estimate, estimate_total_black_or_african_american_alone_estimate, estimate_total_asian_alone_estimate)  %>%
  rowwise() %>%
  summarize(other = sum(across(matches("american_indian|native_hawaiian|some_other_race|two_or_more_races_estimate"))))

## Make column names more concise
colnames(race_marginals) <- c("GEOID", "total", "white", "black", "asian", "other")

## Now combine into single full marginal column for PUMAs
all_marginals <- full_join(age_sex_marginals, race_marginals) %>% ungroup() %>% select(GEOID, total, male, female, everything())

#readr::write_csv(all_marginals, here::here("data", "working", "marginals_for_ipf.csv"))
