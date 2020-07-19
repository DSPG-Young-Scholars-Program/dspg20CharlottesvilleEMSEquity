
library(tidycensus)
library(dplyr)
library(tidyr)

## Variable names for merging on column names in tables
v2018subject <- load_variables(2018, dataset = "acs5/subject", cache = TRUE)
v2018 <- load_variables(2018, dataset = "acs5", cache = TRUE)

## Pull age and sex table
age_by_sex <- get_acs(geography = "public use microdata area",
                      year = 2018,
                      table = "S0101",
                      state = "VA") %>%
  filter(GEOID %in% c("5151089", "5151090")) %>%
  left_join(v2018subject, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

## Sum counts within age categories to get age marginals. Sex marginals also included as columns already
age_sex_marginals <- age_by_sex %>%
  select(GEOID, (contains("estimate_female_total_population_") | contains("estimate_male_total_population_")) & !matches("selected|raio|summary|moe")) %>%
  mutate(females = estimate_female_total_population_estimate,
         males = estimate_male_total_population_estimate) %>%
  group_by(GEOID, females, males) %>%
  rowwise() %>%
  summarize(age_0_24 = sum(across(matches("under_5|5_to_9|10_to_14|15_to_19|20_to_24"))),
            age_25_50 = sum(across(matches("25_to_29|30_to_34|35_to_39|40_to_44|45_to_49"))),
            age_50_75 = sum(across(matches("50_to_54|55_to_59|60_to_64|65_to_69|70_to_74"))),
            age_75_up = sum(across(matches("75_to_79|80_to_84|85_years_and_over"))))

## Pull race marginal table
race_data <- get_acs(geography = "public use microdata area",
        year = 2018,
        table = "B02001",
        state = "VA") %>%
  filter(GEOID %in% c("5151089", "5151090")) %>%
  left_join(v2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

## Select relevant estimate columns
race_marginals <- race_data %>% 
  select(GEOID, !matches("moe|excluding|including"), -NAME, -concept)

## Make column names more concise
colnames(race_marginals) <- c("GEOID", "total_pop", "white", "black", "am_ind", "asian", "hawaiian_pac_isl", "other", "multiple")

## Now combine into single full marginal column for PUMAs
all_marginals <- full_join(age_sex_marginals, race_marginals) %>% select(GEOID, total_pop, everything())
