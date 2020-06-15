library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)

ems <- read_excel("./dspg20CharlottesvilleEMSEquity/data/original/CFD_CARS_EMS_DATA_121616TO60920.xlsx")
str(ems)
ems = as.data.table(ems)

#top n many incidents reported
incidents = ems %>% count(incident_type = `Incident Complaint Reported By Dispatch (eDispatch.01)`, sort =T)
top_incidents = 7
incidents$incident_type = factor(incidents$incident_type)
x = factor(rep(incidents$incident_type[1:top_incidents], times = incidents$n[1:top_incidents]))
others = factor(rep("Other", times =nrow(ems)-length(x)))
f = factor(c(levels(x)[x], levels(others)[others]))
pie(table(f), main = 'Pie Chart of Incidents')

#distribution of ages, primarily around 60 and 20, one person is 120 years old (has to be a misentry)
summary(ems$`Patient Age (ePatient.15)`)
boxplot(ems$`Patient Age (ePatient.15)`, xlab= 'Patient Age', main ='Histogram of Patient Age')
hist(ems$`Patient Age (ePatient.15)`,xlab='Patient Age', main = 'Histogram of Patient Age')

#Rescue squad represents 67% of the entries
agency = ems %>% count(agency = `Agency Name (dAgency.03)`, sort =T)
prop.table(table(ems$`Agency Name (dAgency.03)`))
ggplot(ems, aes(x=`Agency Name (dAgency.03)`))+geom_bar(stat="identity")+ggtitle('Number of calls per agency')

# would be better to have a facet grid or something
ems[`Patient Race List (ePatient.14)`=="Black or African American", .N, by = `Agency Name (dAgency.03)`] %>%
  ggplot(aes(x=`Agency Name (dAgency.03)`,y=N))+
  geom_bar(stat="identity")+
  ggtitle('Number of calls by black patients per agency')

ems[`Patient Race List (ePatient.14)`=="White", .N, by = `Agency Name (dAgency.03)`] %>%
  ggplot(aes(x=`Agency Name (dAgency.03)`,y=N))+
  geom_bar(stat="identity")+
  ggtitle('Number of calls by white patients per agency')

albermarle = ems %>% filter(`Agency Name (dAgency.03)` == "Charlottesville-Albemarle Rescue Squad") %>%
  count(race = `Patient Race List (ePatient.14)`, sort = T)
fire_dept = ems %>% filter(`Agency Name (dAgency.03) `== "Charlottesville Fire Department") %>%
  count(race = `Patient Race List (ePatient.14)`, sort =T)
#ratio of white to black patients by agency
alb_ratio = albermarle$n[1]/albermarle$n[2]
fire_ratio = fire_dept$n[1]/fire_dept$n[2]
cat('The Charlottesville-Albemarle Rescue Squad saw', alb_ratio, 'white people for every black person
    while the Charlottesville Fire Department saw',  fire_ratio, 'white people for every black person')

summary(ems$`Incident Dispatch Notified To Unit Arrived On Scene In Minutes`)

ems%>% filter(`Incident Dispatch Notified To Unit Arrived On Scene In Minutes`<60)%>%
ggplot(aes(x=`Incident Dispatch Notified To Unit Arrived On Scene In Minutes`)) +
  geom_histogram(color="black", fill="white")

# if you compare to the number of trips by agency then about 1/3 of rescue team trips take longer than 10 min, while
# about 1/8 of the fire department trips do
ems %>% filter(`Incident Dispatch Notified To Unit Arrived On Scene In Minutes` >10) %>%
  count(agency = `Agency Name (dAgency.03)`, sort = T)

plot_1 = ggplot(subset(ems, `Patient Race List (ePatient.14)` %in% c("White", "Black or African American") &
                         `Incident Dispatch Notified To Unit Arrived On Scene In Minutes`<60),
                aes(x=`Incident Dispatch Notified To Unit Arrived On Scene In Minutes`)) +
  geom_histogram(color="black", fill="white") +
  facet_grid(. ~ `Patient Race List (ePatient.14)`, scale = "free_y")
plot_1

b = ems %>% filter(`Patient Race List (ePatient.14)`=="Black or African American" &
                     `Incident Dispatch Notified To Unit Arrived On Scene In Minutes`<60)
summary(b$`Incident Dispatch Notified To Unit Arrived On Scene In Minutes`)
w = ems %>% filter(`Patient Race List (ePatient.14)`=="White" &
                     `Incident Dispatch Notified To Unit Arrived On Scene In Minutes`<60)
summary(w$`Incident Dispatch Notified To Unit Arrived On Scene In Minutes`)


ems[, .N, by= `Incident Date`][order(N,decreasing=T)] %>%
  ggplot(aes(`Incident Date`, N))+
  geom_line(lwd= .2)

ems[`Patient Race List (ePatient.14)`=="Black or African American",.N, by= `Incident Date`]%>%
  ggplot(aes(`Incident Date`, N))+
  geom_line(lwd= .2)

ems[`Patient Race List (ePatient.14)`=="White",.N, by= `Incident Date`]%>%
  ggplot(aes(`Incident Date`, N))+
  geom_line(lwd= .2)

sd(ems[`Patient Race List (ePatient.14)`=="White",.N, by= `Incident Date`]$N)
sd(ems[`Patient Race List (ePatient.14)`=="Black or African American",.N, by= `Incident Date`]$N)

ems %>%
  filter(`Patient Race List (ePatient.14)` %chin% c("Black or African American","White","Asian",
                                                    "Hispanic or Latino", "American Indian or Alaska Native"))%>%
  count(date = `Incident Date`, race =`Patient Race List (ePatient.14)`)%>%
  ggplot(aes(date, n,  color = race, group = race))+
  geom_line(lwd= .2)

ems %>%
  filter(`Patient Race List (ePatient.14)` %chin% c("Black or African American","White"))%>%
  count(date = `Incident Date`, race =`Patient Race List (ePatient.14)`)%>%
  ggplot(aes(date, n,  color = race, group = race))+
  geom_line(lwd= .2)
