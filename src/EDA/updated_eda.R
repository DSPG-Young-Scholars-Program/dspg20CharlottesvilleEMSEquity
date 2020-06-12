library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)

ems <- read_excel("./dspg20CharlottesvilleEMSEquity/data/original/CFD_CARS_EMS_DATA_121616TO60920.xlsx")
str(ems)
ems = as.data.table(ems)

incidents = ems %>% count(incident_type = `Incident Complaint Reported By Dispatch (eDispatch.01)`, sort =T)
top_incidents = 7
incidents$incident_type = factor(incidents$incident_type)
x = factor(rep(incidents$incident_type[1:top_incidents], times = incidents$n[1:top_incidents]))
others = factor(rep("Other", times =nrow(ems)-length(x)))
f = factor(c(levels(x)[x], levels(others)[others]))
pie(table(f), main = 'Pie Chart of Incidents')


summary(ems$`Patient Age (ePatient.15)`)
boxplot(ems$`Patient Age (ePatient.15)`, xlab= 'Patient Age', main ='Histogram of Patient Age')
hist(ems$`Patient Age (ePatient.15)`,xlab='Patient Age', main = 'Histogram of Patient Age')

# would be better to have a facet grid or something
agency = ems %>% count(agency = `Agency Name (dAgency.03)`, sort =T)
prop.table(table(ems$`Agency Name (dAgency.03)`))
ggplot(agency, aes(x=agency,y=n))+geom_bar(stat="identity")+ggtitle('Number of calls per agency')

ems[`Patient Race List (ePatient.14)`=="Black or African American", .N, by = `Agency Name (dAgency.03)`] %>%
  ggplot(aes(x=`Agency Name (dAgency.03)`,y=N))+
  geom_bar(stat="identity")+
  ggtitle('Number of calls by black patients per agency')

ems[`Patient Race List (ePatient.14)`=="White", .N, by = `Agency Name (dAgency.03)`] %>%
  ggplot(aes(x=`Agency Name (dAgency.03)`,y=N))+
  geom_bar(stat="identity")+
  ggtitle('Number of calls by white patients per agency')

albermarle = ems %>% filter(`Agency Name (dAgency.03)`=="Charlottesville-Albemarle Rescue Squad") %>%
  count(race = `Patient Race List (ePatient.14)`, sort =T)
fire_dept = ems %>% filter(`Agency Name (dAgency.03)`=="Charlottesville Fire Department") %>%
  count(race = `Patient Race List (ePatient.14)`, sort =T)
#ratio of white to black patients by agency
cat('The albermarle[1]$n/albermarle[2]$n
fire_dept[1]$n/fire_dept[2]$n
