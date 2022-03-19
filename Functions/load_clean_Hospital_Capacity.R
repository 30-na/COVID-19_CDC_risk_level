
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
    filter(date >= "2021/01/01")


# less than four considerd as -99999
bed_accupied$used_beds_covid[bed_accupied$used_beds_covid < 0] = 2


bed_accupied_rate = bed_accupied %>%
    group_by(fips_code, date, state) %>%
    summarise(accupied_bed_county = sum(used_beds_covid),
              total_beds_county = sum(total_beds)) %>%
    arrange(date, state, fips_code)%>%
    mutate(accupied_rate = round(x=(accupied_bed_county/total_beds_county)*100, digit=2))



bed_accupied_rate$accupied_rate[bed_accupied_rate$accupied_rate > 100] = NA
bed_accupied_rate$accupied_rate[bed_accupied_rate$accupied_rate < 0] = NA

    

save(bed_accupied_rate, file="Data/bed_accupied_clean.csv") 



