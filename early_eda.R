df = CFD_CARS_EMS_DATA_121616TO51320
nas_in_column = colSums(is.na(df))
nas_in_column
class(nas_in_column)
nas_in_column[1]
not_all_nas = nas_in_column[nas_in_column<max(nas_in_column)]
not_all_nas
class(df)
df$`Incident Date`
max(df$`Incident Date`)
table(df$`Incident Type`)
min(df$`Patient Last Pain Scale Score (eVitals.27)`)
