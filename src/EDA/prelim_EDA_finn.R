
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

## Read in data
source(here::here("src", "Profiling", "joining_albemarle_charlottesville.R"))

charlottesville <- charlottesville %>% mutate(incident_date = ymd(incident_date))

## EDA function for ease of plotting volume over time grouped by complaint
plot_complaint_volume <- function(data, vars) {
  
  tmp <- data %>%
    filter(incident_complaint_reported_by_dispatch %in% vars) %>%
    group_by(incident_date, incident_complaint_reported_by_dispatch) %>%
    summarize(n = n())
  
  all_dates <- expand.grid(incident_date = seq(min(data$incident_date), max(data$incident_date), by = "days"), 
                           incident_complaint_reported_by_dispatch = vars)
  
  plot_data <- left_join(all_dates, tmp, by = c("incident_date", "incident_complaint_reported_by_dispatch")) %>% 
    mutate(n = case_when(!is.na(n) ~ as.numeric(n),
                         is.na(n) ~ 0))
    
  ggplot(plot_data) + 
    geom_smooth(aes(x = incident_date, y= n, color = incident_complaint_reported_by_dispatch), se = F) +
    geom_line(aes(x = incident_date, y= n, color = incident_complaint_reported_by_dispatch), alpha = 0.3)
  
}

## Complaints to view
complaints <- c("Breathing Problem", "Chest Pain (Non-Traumatic)")

## Plot
plot_complaint_volume(charlottesville, vars = complaints)
