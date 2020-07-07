#todo
#check if cardiac_arrest_date_time is after incident date time and after "cardiac_arrest_initial_cpr_date_time"+
#"cardiac_arrest_rosc_date_time" + destination_cardiac_arrest_team_activation_date_time

library(dplyr)
library(data.table)
library(lubridate)

albemarle <- read.csv2("./data/original/Data4UVA.csv", fileEncoding="UTF-16LE", sep = ",", na.strings = "")
charlottesville <- readxl::read_xlsx("./data/original/CFD_CARS_EMS_DATA_121616TO60920.xlsx", 1)


alb <- albemarle %>%
  rename_with(~tolower(gsub(r"(\.\..*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(r"(\.)", "_", .x)) # change periods to underscores


chr <- charlottesville %>%
  rename_with(~tolower(gsub(r"( \(.*)", "", .x))) %>% # remove code after variable names
  rename_with(~gsub(" ", "_", .x)) %>% # change spaces to underscores
  as.data.frame()

#finds different cols in datasets, patient_respiratory_effort_list_ vs. patient_respiratory_effort_list and
#total_unit_response_time vs. incident_unit_notified_by_dispatch_to_unit_arrived_on_scene_in_minutes
setdiff(union(colnames(chr), colnames(alb)),
        intersect(colnames(chr), colnames(alb)))

#finding the number and indices of entries where age range is na but patient age is not (or vice versa)
find_age_mismatch = function(df){
  df[which(!is.na(df$patient_age_range_in_years)==is.na(df$patient_age)),]
}
alb_age = find_age_mismatch(alb) # 33427  and 49906  seem to have a lot of misentries
View(alb_age)
chr_age = find_age_mismatch(chr) # 10855 1yr old whose been drunk for 3 days? lots of <1 year olds
View(chr_age)


find_gps_mismatch = function(df){
  df[which(!is.na(df$scene_gps_latitude)==is.na(df$scene_gps_longitude)),]
}
#incident number seems to actually be call time, incident date is some non date-numeric (MAYBE TIME TO SCENE)
#lats are things the emts did it seems, longs are NA, most other columns are NAs
alb_gps = find_gps_mismatch(alb)
View(a_inds)
# not the case for chr dataset
chr_gps = find_gps_mismatch(chr)
View(c_inds)

# check if incident time is ever less than time for unit to get to site, results don't fit intuition, maybe total unit
#time is differently calculated than we thought for chr
find_time_mismatch = function(df){
  if ("total_unit_response_time" %chin% colnames(df)){
    df[which(df[,"incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes"]<df[,"total_unit_response_time"]),]
  }
  else
    df[which(df[,"incident_dispatch_notified_to_unit_arrived_on_scene_in_minutes"]<df[,"incident_unit_notified_by_dispatch_to_unit_arrived_on_scene_in_minutes"]),]
}
alb_time = find_time_mismatch(alb)
View(alb_time)
#rows 1210 and 1211 are nearly identical except patient is complaining for 1 day vs 5 days
chr_time = find_time_mismatch(chr)
View(chr_time)

find_date_mismatch = function(df){
  df[which(!as.Date(df$incident_psap_call_date_time, "%Y-%m-%d") == as.Date(df$incident_date)),]
}
#none :)
chr_date = find_date_mismatch(chr)
View(chr_date)
#can't do yet, b/c of weird entries dont think its worth the trouble to check the rows w/o weird entries atm
# alb_date = find_date_mismatch(alb)
# View(alb_date)

# na.exclude(chr$cardiac_arrest_date_time<chr$cardiac_arrest_initial_cpr_date_time)
# chr[which(!is.na(chr$cardiac_arrest_date_time)),][which(!as.Date(chr$incident_psap_call_date_time, "%Y-%m-%d") < as.Date(chr$cardiac_arrest_date_time)),]


find_date_mismatch = function(df){
  df[which(!as.Date(df$incident_psap_call_date_time, "%Y-%m-%d") == as.Date(df$incident_date)),]
}


tmp = chr[which(!format(as.Date(chr$patient_last_oral_intake_date_time),"%m-%d")==format(as.Date(chr$incident_date), "%m-%d")),]
View(tmp[,c("patient_last_oral_intake_date_time","incident_date")])

time.interval = chr$patient_last_oral_intake_date_time %--% chr$incident_date
span_in_seconds = time.interval %>% int_length()
#gives incidents where last intake is after incident date?
late_response = chr[span_in_seconds<= -86400,] %>% filter(!is.na(patient_last_oral_intake_date_time))
View(late_response[,c("patient_last_oral_intake_date_time","incident_date" )])


span_reverse = time.interval %>% int_flip() %>% int_length()

opp = chr[span_reverse<= -86400,] %>% filter(!is.na(patient_last_oral_intake_date_time))
View(opp[,c("patient_last_oral_intake_date_time","incident_date" )])


tmp_psap = chr[which(!format(as.Date(chr$patient_last_oral_intake_date_time),"%m-%d")==format(as.Date(chr$incident_psap_call_date_time), "%m-%d")),]
View(tmp_psap[,c("patient_last_oral_intake_date_time","incident_psap_call_date_time", "incident_date")])

time.interval_psap = chr$patient_last_oral_intake_date_time %--% chr$incident_psap_call_date_time
span_in_seconds_psap = time.interval_psap %>% int_length()
#gives incidents where last intake is after incident date?
late_response_psap = chr[span_in_seconds_psap<= -86400,] %>% filter(!is.na(patient_last_oral_intake_date_time))
View(late_response_psap[,c("patient_last_oral_intake_date_time","incident_psap_call_date_time" )])


span_reverse_psap = time.interval_psap %>% int_flip() %>% int_length()

opp_psap = chr[span_reverse_psap<= -86400,] %>% filter(!is.na(patient_last_oral_intake_date_time))
View(opp_psap[,c("patient_last_oral_intake_date_time","incident_psap_call_date_time" )])


View(chr[which(format(as.Date(chr$patient_last_oral_intake_date_time),"%Y")=="2001"),c("patient_last_oral_intake_date_time","incident_psap_call_date_time", "incident_date")])

year(chr[which(format(as.Date(chr$patient_last_oral_intake_date_time),"%Y")=="2001"),]$patient_last_oral_intake_date_time) = year(chr[which(format(as.Date(chr$patient_last_oral_intake_date_time),"%Y")=="2001"),]$incident_psap_call_date_time)

chr[which(year(chr$patient_last_oral_intake_date_time)==2001),]
