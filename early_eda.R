df =fread("./data/original/CFD_CARS_EMS_DATA_121616TO51320.csv", na.strings = c(""), colClasses = rep("character", 78))
df = read.csv("./data/original/CFD_CARS_EMS_DATA_121616TO51320.csv", na.strings = "")
dt[,.N,by=Incident.Complaint.Reported.By.Dispatch..eDispatch.01.]
dt = as.data.table(df)

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
