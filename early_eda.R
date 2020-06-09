library(data.table)
library(dplyr)

df =read.csv("./data/original/CFD_CARS_EMS_DATA_121616TO51320.csv", na.strings = " ")
dt = as.data.table(df)
glimpse(dt)
#creates a key by incident number and drops all duplicates
setkeyv(dt, c("Response.Incident.Number..eResponse.03."))
uniqdat <- subset(unique(dt))

#sorts the most common incident types and plots the top_incidents most common
incidents = uniqdat %>% count(incident_type = Incident.Complaint.Reported.By.Dispatch..eDispatch.01., sort =T)
incidents = incidents[-1]
top_incidents = 7
incidents$incident_type = factor(incidents$incident_type)
x = factor(rep(incidents[1:top_incidents]$incident_type, times = incidents[1:top_incidents]$n))
others = factor(rep("Other", times =nrow(uniqdat)-length(x)))
f = factor(c(levels(x)[x], levels(others)[others]))
pie(table(f), main = 'Pie Chart of Incidents')
#histogram/boxplot of patient age distribution, two big modes around 60 and 20, the former being larger
summary(uniqdat$`Patient.Age..ePatient.15.`)
boxplot(uniqdat$`Patient.Age..ePatient.15.`, xlab= 'Patient Age', main ='Histogram of Patient Age')
hist(uniqdat$`Patient.Age..ePatient.15.`,xlab='Patient Age', main = 'Histogram of Patient Age')

(df$Incident.Complaint.Reported.By.Dispatch..eDispatch.01.)
nas_in_column = colSums(is.na(df))
nas_in_column
class(nas_in_column)
nas_in_column[1]
not_all_nas = nas_in_column[nas_in_column < max(nas_in_column)]
not_all_nas
class(df)
df$`Incident Date`
max(df$`Incident Date`)
table(df$`Incident Type`)
min(df$`Patient Last Pain Scale Score (eVitals.27)`)
