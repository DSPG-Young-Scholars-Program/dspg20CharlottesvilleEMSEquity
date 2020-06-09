library(data.table)
library(dplyr)

df =read.csv("./data/original/CFD_CARS_EMS_DATA_121616TO51320.csv", na.strings = c(" ",""))
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

#barplot of department called to site
agency = uniqdat %>% count(agency = Agency.Name..dAgency.03., sort =T)
table(t(agency))
ggplot(agency, aes(x=agency,y=n))+geom_bar(stat="identity")

# looking into race
race = uniqdat %>% count(race = Patient.Race.List..ePatient.14., sort =T) #lots of weird repeats, lots of na's, mostly
                                                                          #white and black ppl
white = uniqdat %>% filter(Patient.Race.List..ePatient.14. == "White") %>%
  count(agency = Agency.Name..dAgency.03., sort =T)
black = uniqdat %>% filter(Patient.Race.List..ePatient.14. == "Black or African American") %>%
  count(agency = Agency.Name..dAgency.03., sort =T)
# White ppl see the rescue squad 3.9365 times more than the fire dept while that stat for black ppl is 2.7 (i dont think
# this intepretation is that useful)
ggplot(white, aes(x=agency,y=n))+geom_bar(stat="identity")
ggplot(black, aes(x=agency,y=n))+geom_bar(stat="identity")

albermarle = uniqdat %>% filter(Agency.Name..dAgency.03.=="Charlottesville-Albemarle Rescue Squad") %>%
  count(race = Patient.Race.List..ePatient.14., sort =T)
fire_dept = uniqdat %>% filter(Agency.Name..dAgency.03.=="Charlottesville Fire Department") %>%
  count(race = Patient.Race.List..ePatient.14., sort =T)
albermarle[1]$n/albermarle[2]$n
fire_dept[1]$n/fire_dept[2]$n
#rescue sees 2.409 times as many white ppl than black ppl while fire department see 1.57 as many white ppl as black ppl

#time to scene
summary(uniqdat$`Incident.Dispatch.Notified.To.Unit.Arrived.On.Scene.In.Minutes`)
#mean =10, median =8, max =1981? should it be 19.81?,then its 1446. then 535, 451, 441
#get rid of times w NA's
tmp = uniqdat %>% filter(!is.na(Incident.Dispatch.Notified.To.Unit.Arrived.On.Scene.In.Minutes))
dim(tmp)

#gets rid of incidents that were longer than 60 minutes, about 145 of them
tmp = tmp %>% filter(Incident.Dispatch.Notified.To.Unit.Arrived.On.Scene.In.Minutes <60)
dim(tmp)
#bring mean time down a little
summary(tmp$`Incident.Dispatch.Notified.To.Unit.Arrived.On.Scene.In.Minutes`)

#heavy right skew
ggplot(tmp, aes(x=Incident.Dispatch.Notified.To.Unit.Arrived.On.Scene.In.Minutes)) +
  geom_histogram(color="black", fill="white")

# if you compare to the number of trips by agency then about 1/3 of rescue team trips take longer than 10 min, while
# about 1/8 of the fire department trips do
uniqdat %>% filter(Incident.Dispatch.Notified.To.Unit.Arrived.On.Scene.In.Minutes >10) %>%
  count(agency = Agency.Name..dAgency.03., sort = T)

