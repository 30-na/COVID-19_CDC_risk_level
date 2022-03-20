
library(dplyr)
library(usdata)
library(data.table)

# read data from CDC Community Risk level 
hospital_capacity = fread("Data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv")


# county polulations
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
bed_accupied = hospital_capacity %>%
    select(collection_week,
           state,
           fips_code,
           total_beds_7_day_avg,
           inpatient_beds_used_covid_7_day_avg,
           total_adult_patients_hospitalized_confirmed_covid_7_day_sum,
           total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum)%>%
    
    rename(date = collection_week,
           total_beds = total_beds_7_day_avg,
           used_beds_covid = inpatient_beds_used_covid_7_day_avg,
           adulat_hos_7day = total_adult_patients_hospitalized_confirmed_covid_7_day_sum,
           pediatric_hos_7day = total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum) %>%
    
    mutate(date = as.Date(date, format="%Y/%m/%d"),
           confirm_hospitalized = adulat_hos_7day + pediatric_hos_7day) %>%
    
    arrange(date, state, fips_code) %>%
    
    filter(date >= "2021/01/01")


# less than four considered as -99999
bed_accupied$used_beds_covid[bed_accupied$used_beds_covid < 0] = 2
bed_accupied$confirm_hospitalized[bed_accupied$confirm_hospitalized < 0] = 2

bed_accupied_rate = bed_accupied %>%
    group_by(fips_code, date, state) %>%
    summarise(accupied_bed_county = sum(used_beds_covid),
              total_beds_county = sum(total_beds),
              admission_county = sum(confirm_hospitalized)) %>%
    arrange(date, state, fips_code)%>%
    mutate(accupied_rate = round(x=(accupied_bed_county/total_beds_county)*100, digit=2))



bed_accupied_rate$accupied_rate[bed_accupied_rate$accupied_rate > 100] = NA
bed_accupied_rate$accupied_rate[bed_accupied_rate$accupied_rate < 0] = NA

hospitalization_county = merge(bed_accupied_rate,
                               county_population,
                               by="fips_code")

hospitalization_county = hospitalization_county %>%
    arrange(fips_code,
            date) %>%
    na.omit()

save(hospitalization_county, file="Data/hospitalization_county.csv") 



