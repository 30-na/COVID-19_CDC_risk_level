
library(dplyr)
library(usdata)
library(data.table)

# read data from HealthData.org 
Hospital_Capacity = fread("Data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv")


# county population
load("Data/CDC_community_level_county.csv")
county_population = CDC_community_level_county %>%
    select(county_fips,
           population) %>%
    rename(fips_code = county_fips) %>%
    na.omit() %>%
    distinct(fips_code,
             population,
             .keep_all = TRUE)

#clean data
# use "used_beds_covid" for computing bed accupied rate
# use the sum of "previous_day_admission_adult_covid_confirmed_7_day_sum" and 
# "previous_day_admission_pediatric_covid_confirmed_7_day_sum" to compute admission in 100 

bed_accupied = Hospital_Capacity %>%
    select(collection_week,
           state,
           fips_code,
           total_beds_7_day_avg,
           inpatient_beds_used_covid_7_day_avg,
           previous_day_admission_adult_covid_confirmed_7_day_sum,
           previous_day_admission_pediatric_covid_confirmed_7_day_sum)%>%
    
    rename(date = collection_week,
           total_beds = total_beds_7_day_avg,
           used_beds_covid = inpatient_beds_used_covid_7_day_avg,
           adult_hos_7day = previous_day_admission_adult_covid_confirmed_7_day_sum,
           pediatric_hos_7day = previous_day_admission_pediatric_covid_confirmed_7_day_sum)
    
  
#  Suppression is applied to the file for sums and averages less than four (4).
# In these cases, the field will be replaced with “-999,999”.
# replace -999999 with 2
# bed_accupied$total_beds[bed_accupied$total_beds == -999999] = 2
# bed_accupied$used_beds_covid[bed_accupied$used_beds_covid == -999999] = 2
# bed_accupied$adult_hos_7day[bed_accupied$adult_hos_7day == -999999] = 2
# bed_accupied$pediatric_hos_7day[bed_accupied$pediatric_hos_7day == -999999] = 2


set.seed(214654)
bed_accupied$total_beds[bed_accupied$total_beds == -999999] = sample(x = c(1,2,3), size = 1)
set.seed(214654)
bed_accupied$used_beds_covid[bed_accupied$used_beds_covid == -999999] = sample(x = c(1,2,3), size = 1)
set.seed(214654)
bed_accupied$adult_hos_7day[bed_accupied$adult_hos_7day == -999999] = sample(x = c(1,2,3), size = 1)
set.seed(214654)
bed_accupied$pediatric_hos_7day[bed_accupied$pediatric_hos_7day == -999999] = sample(x = c(1,2,3), size = 1)



bed_accupied_rate = bed_accupied %>%
  
  arrange(date, state, fips_code) %>%
  
  group_by(fips_code, date, state) %>%
  
  summarise(total_beds_county = sum(total_beds),
            accupied_bed_county = sum(used_beds_covid),
            adult_hos_7day_county = sum(adult_hos_7day),
            pediatric_hos_7day_county = sum(pediatric_hos_7day)) %>%
  
  mutate(date = as.Date(date, format="%Y/%m/%d"),
         hospital_admissions = adult_hos_7day_county + pediatric_hos_7day_county,
         bed_utilization = round(x=(accupied_bed_county/total_beds_county)*100, digit=2))



bed_accupied_rate$bed_utilization[bed_accupied_rate$bed_utilization > 100] = NA
bed_accupied_rate$bed_utilization[bed_accupied_rate$bed_utilization < 0] = NA

hospital_utilization = bed_accupied_rate

save(hospital_utilization, file="Data/hospital_utilization_county.csv") 



