library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(vroom)
library(stringr)
library(naniar)
library(astsa, quietly=TRUE, warn.conflicts=FALSE)
library(knitr)
library(printr)
library(gridExtra)
library(reshape2)
library(TTR)


# load data

source(here::here('src', 'EDA', 'chase_potential_covid_eda.R'))

simp = "breat"
# pre covid
pre_breath = ems %>%
  filter(incident_date <= cutoff) %>%
  filter(!grepl("copd",situation_provider_primary_impression_code_and_description)) %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp,situation_primary_complaint_statement_list)) %>%
  count(situation_provider_primary_impression_code_and_description) %>%
  arrange(desc(n)) %>%
  mutate(freq = n / sum(n))
#post covid
post_breath = ems %>%
  filter(incident_date >= cutoff) %>%
  filter(!grepl("copd",situation_provider_primary_impression_code_and_description)) %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp,situation_primary_complaint_statement_list)) %>%
  count(situation_provider_primary_impression_code_and_description) %>%
  arrange(desc(n)) %>%
  mutate(freq = n / sum(n))

breath = pre_breath %>% full_join(post_breath,by = "situation_provider_primary_impression_code_and_description")

breath_freq = reshape2::melt(breath[1:10,], id.vars = "situation_provider_primary_impression_code_and_description", measure.vars = c("freq.x", "freq.y"))


ems %>%
  filter(incident_date <= cutoff) %>%
  filter(!grepl("copd",situation_provider_primary_impression_code_and_description)) %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp ,situation_primary_complaint_statement_list)) %>%
  count(situation_provider_primary_impression_code_and_description) %>%
  arrange(desc(n)) %>%
  mutate(freq = n / sum(n))

ems %>%
  filter(incident_date >= cutoff) %>%
  filter(!grepl("copd",situation_provider_primary_impression_code_and_description)) %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp,situation_primary_complaint_statement_list)) %>%
  count(situation_provider_primary_impression_code_and_description) %>%
  arrange(desc(n)) %>%
  mutate(freq = n / sum(n))

top_complaints <- ems %>%
  filter(!grepl("copd",situation_provider_primary_impression_code_and_description)) %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp ,situation_primary_complaint_statement_list))%>%
  group_by(situation_provider_primary_impression_code_and_description) %>%
  summarize(count = n()) %>%
  top_n(10, count)
top_comp = top_complaints$situation_provider_primary_impression_code_and_description


breath_freq %>%
  ggplot(aes(situation_provider_primary_impression_code_and_description, fill = variable))+
  geom_bar(aes(y=value), stat="identity", position = "dodge")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_discrete(labels=c("Pre-Covid", "Post-Covid"))+
  labs(x= "Provider Primary Impression", y= "Proportion", title = " Frequency of Top 10 Provider Impressions for 'Breat' Incident Complaints")


ems <- ems %>%
  mutate(incident_date = ymd(incident_date),
         yr = as.factor(year(incident_date)),
         mo = as.factor(month(incident_date)),
         dy = as.character(day(incident_date)),
         mo_yr = dmy(paste0("01-", mo, "-", yr)))


plot_data <- ems %>%
  #filter(situation_provider_primary_impression_code_and_description %in% top_impressions) %>%
  #mutate(situation_provider_primary_impression_code_and_description = factor(situation_provider_primary_impression_code_and_description,
  #levels = top_impressions)) %>%
  # filter(incident_date >= cutoff) %>%
  filter(!grepl("copd",situation_provider_primary_impression_code_and_description)) %>%
  filter(!is.na(situation_provider_primary_impression_code_and_description)) %>%
  filter(grepl(simp ,situation_primary_complaint_statement_list)) %>%
  mutate(primary_impression_category = grepl(simp, situation_primary_complaint_statement_list)) %>%
  group_by(mo_yr, primary_impression_category) %>%
  count()



plot_data %>%
ggplot() +
  geom_line(aes(x = mo_yr, y = n, colour=(month(mo_yr) %in% c(10:12,1:2)), group=1 ), alpha = 1, size  = 2) +
  geom_point(aes(x = mo_yr, y = n), alpha = 0.7) +
  geom_vline(xintercept = dmy("01-02-2020"), linetype = "dashed", alpha = 0.8) +
  #scale_color_manual(values = cbPalette) +
  #facet_wrap(~situation_provider_primary_impression_code_and_description, ncol = 1) +
  labs(x = "Date", y = "Monthly Incidents", color = "Flu Season", title = "Call Volume for Complaints about Breath\n") +
  theme_minimal()

plot_data <- ems %>%
  #filter(situation_provider_primary_impression_code_and_description %in% top_impressions) %>%
  #mutate(situation_provider_primary_impression_code_and_description = factor(situation_provider_primary_impression_code_and_description,
  #levels = top_impressions)) %>%
  # filter(incident_date >= cutoff) %>%
  filter(grepl(simp ,situation_primary_complaint_statement_list)) %>%
  filter(grepl('j11', situation_provider_primary_impression_code_and_description)) %>%
  mutate(primary_impression_category = grepl('j11', situation_provider_primary_impression_code_and_description)) %>%
  group_by(mo_yr, primary_impression_category) %>%
  count()
flu_season = data.frame(
  from=as.Date(c('2016-10-01', '2017-10-01', '2018-10-01', '2019-10-01')),
  to=as.Date(c('2017-03-01', '2018-03-01', '2019-03-01', '2020-03-01'))
)


plot_data %>%
  ggplot() +
  geom_line(aes(x = mo_yr, y = n ), alpha = 1) +
  geom_point(aes(x = mo_yr, y = n), alpha = 0.7) +
  geom_vline(xintercept = dmy("01-02-2020"), linetype = "dashed", alpha = 0.8) +
  geom_rect(data = flu_season , aes(xmin = from - 1, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4)+  #scale_color_manual(values = cbPalette) +
  #facet_wrap(~situation_provider_primary_impression_code_and_description, ncol = 1) +
  labs(x = "Date", y = "Monthly Incidents", color = "Flu Season", title = "Call Volume of Primary Impression of Flu Like Symptoms \n with Breathing Complaints\n") +
  theme_minimal()



top_complaints <- ems %>%
  filter(grepl(simp ,situation_primary_complaint_statement_list)) %>%
  filter(situation_provider_primary_impression_code_and_description %in% top_comp) %>%
  group_by(mo_yr, situation_provider_primary_impression_code_and_description) %>%
  summarize(count = n()) %>%
  top_n(10, count)
top_complaints %>%
  ggplot() +
  geom_line(aes(x = mo_yr, y = count), alpha = .7) +
  geom_vline(xintercept = dmy("01-02-2020"), linetype = "dashed", alpha = 0.8) +
  facet_wrap(~situation_provider_primary_impression_code_and_description)+
  geom_rect(data = flu_season , aes(xmin = from - 1, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4)+  #scale_color_manual(values = cbPalette) +
  #scale_color_manual(values = cbPalette) +
  #facet_wrap(~situation_provider_primary_impression_code_and_description, ncol = 1) +
  labs(x = "Date", y = "Monthly Incidents", color = "Flu Season", title = "Call Volume of Primary Impression of Flu Like Symptoms \n with Breathing Complaints\n") +
  theme_minimal()



seasonality <- ems %>%
  filter(grepl(simp ,situation_primary_complaint_statement_list)) %>%
  filter(situation_provider_primary_impression_code_and_description %in% top_comp) %>%
  group_by(mo_yr) %>%
  summarize(count = n())
seasonality = ts(seasonality$count, frequency = 12, start = c(2016,12))
plot.ts(seasonality)

seasonComp = decompose(seasonality)
plot(seasonComp)

SeasonAdj <- seasonality - seasonComp$seasonal
plot.ts(SeasonAdj)
