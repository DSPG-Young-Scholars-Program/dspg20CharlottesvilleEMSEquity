library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(vroom)
library(stringr)
library(naniar)

# load data

source(here::here('src', 'EDA', 'chase_potential_covid_eda.R'))

simp = "breat"
# pre covid
pre_breath = ems %>%
  filter(incident_date <= cutoff) %>%
  filter(provider_copd == "no") %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp,situation_primary_complaint_statement_list)) %>%
  count(situation_provider_primary_impression_code_and_description) %>%
  arrange(desc(n)) %>%
  mutate(freq = n / sum(n))
#post covid
post_breath = ems %>%
  filter(incident_date >= cutoff) %>%
  filter(provider_copd == "no") %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp,situation_primary_complaint_statement_list)) %>%
  count(situation_provider_primary_impression_code_and_description) %>%
  arrange(desc(n)) %>%
  mutate(freq = n / sum(n))

breath = pre_breath %>% full_join(post_breath,by = "situation_provider_primary_impression_code_and_description")

breath_freq = reshape2::melt(breath[1:10,], id.vars = "situation_provider_primary_impression_code_and_description", measure.vars = c("freq.x", "freq.y"))


ems %>%
  filter(incident_date <= cutoff) %>%
  filter(provider_copd == "no") %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp ,situation_primary_complaint_statement_list)) %>%
  count(situation_provider_primary_impression_code_and_description) %>%
  arrange(desc(n)) %>%
  mutate(freq = n / sum(n))

ems %>%
  filter(incident_date >= cutoff) %>%
  filter(provider_copd == "no") %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp,situation_primary_complaint_statement_list)) %>%
  count(situation_provider_primary_impression_code_and_description) %>%
  arrange(desc(n)) %>%
  mutate(freq = n / sum(n))

top_complaints <- ems %>%
  filter(provider_copd == "no") %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp ,situation_primary_complaint_statement_list))%>%
  group_by(situation_provider_primary_impression_code_and_description) %>%
  summarize(count = n()) %>%
  top_n(10, count)
top_comp = top_complaints$situation_provider_primary_impression_code_and_description


ems %>%
  filter(grepl(simp ,situation_primary_complaint_statement_list))%>%
  filter(situation_provider_primary_impression_code_and_description %in% top_comp) %>%
  ggplot(aes(situation_primary_complaint_statement_list)) +
  geom_bar()

breath_freq %>%
  ggplot(aes(situation_provider_primary_impression_code_and_description, fill = variable))+
  geom_bar(aes(y=value), stat="identity", position = "dodge")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_discrete(labels=c("Pre-Covid", "Post-Covid"))+
  labs(x= "Provider Primary Impression", y= "Proportion", title = " Frequency of Top 10 Provider Impressions for 'Breat' Incident Complaints")




