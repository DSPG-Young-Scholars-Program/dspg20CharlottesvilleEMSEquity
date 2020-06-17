#todo
#check date of incident_psap_call_date_time vs. incident date
#check if cardiac_arrest_date_time is after incident date time and after "cardiac_arrest_initial_cpr_date_time"+
#"cardiac_arrest_rosc_date_time" + destination_cardiac_arrest_team_activation_date_time

library(dplyr)

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
cols = !colnames(chr)==colnames(alb)
colnames(chr[,cols])
colnames(alb[,cols])

#finding the number and indices of entries where age range is na but patient age is not (or vice versa)
find_age_mismatch = function(df){
  which(!is.na(df$patient_age_range_in_years)==is.na(df$patient_age))
}
alb_ind = find_age_mismatch(alb) # 33427  and 49906  seem to have a lot of misentries
View(alb[alb_ind,])
chr_ind = find_age_mismatch(chr) # 10855 1yr old whose been drunk for 3 days? lots of <1 year olds
View(chr[chr_ind,])


find_gps_mismatch = function(df){
  which(!is.na(df$scene_gps_latitude)==is.na(df$scene_gps_longitude))
}
#incident number seems to actually be call time, incident date is some non date-numeric (MAYBE TIME TO SCENE)
#lats are things the emts did it seems, longs are NA, most other columns are NAs
a_inds = find_gps_mismatch(alb)
View(alb[a_inds,])
# not the case for chr dataset
c_inds = find_gps_mismatch(chr)
View(chr[c_inds,])




