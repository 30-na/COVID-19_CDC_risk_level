
library(dplyr)
library(usdata)
library(data.table)

# read data from CDC Community Risk level 
hospital_capacity = fread("Data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv")

#clean data
bed_accupied = hospital_capacity %>%
    select(collection_week,
           state,
           fips_code,
           total_beds_7_day_avg,
           inpatient_beds_used_covid_7_day_avg)%>%
    rename(date = collection_week,
           total_beds = total_beds_7_day_avg,
           used_beds_covid = inpatient_beds_used_covid_7_day_avg) %>%
    mutate(date = as.Date(date, format="%Y/%m/%d")) %>%
    arrange(date, state, fips_code) %>%
    filter(date >= "2021/01/01") %>%
    mutate(accupied_rate = round(x=(used_beds_covid/total_beds)*100, digit=2))

bed_accupied$accupied_rate[bed_accupied$accupied_rate < 0] = 0
bed_accupied$accupied_rate[bed_accupied$accupied_rate > 100] = 0

bed_accupied_clean = bed_accupied %>%
    group_by(fips_code, date, state) %>%
    summarise(accupied_rate_county = mean(accupied_rate)) %>%
    arrange(date, state, fips_code)

save(bed_accupied_clean, file="Data/bed_accupied_clean.csv") 



