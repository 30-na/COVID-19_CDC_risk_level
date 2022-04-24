library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)
library(tidyr)


# load datasets
load("Data/hospital_utilization_county.csv")
load("Data/CDC_community_transmission_county_historical.csv")
load("Data/CDC_community_level_county.csv")

# geting "new case" from 
# "CDC United States COVID-19 County Level of Community Transmission Historical Changes" dataset
new_cases = CDC_community_risk_historical %>%
    select(date,
           fips_code,
           new_case)

# set NA for new case less than zero
new_cases[new_cases$new_case < 0, ]$new_case = NA


# merge "newcase" and "Hospital Utilization" datasets
merged_newcase = merge(hospital_utilization,
                       new_cases,
                       by=c("date", "fips_code"))

# add county population from
# "United States COVID-19 Community Levels by County"
county_pop = CDC_community_level_county %>%
    dplyr::filter(date_updated == "2022-03-24") %>%
    dplyr::select(county_fips, 
                  population) %>%
    rename(fips_code = county_fips)

# add counties population to dataset
merged_data = merge(merged_newcase,
                    county_pop,
                    by="fips_code")

# compute hospital_admission_per100
community_level_county = merged_data %>%
    mutate(hospital_admission_per100 = round((hospital_admissions/population)*100000))

# remove NA value
community_level_county = community_level_county %>%
    drop_na(new_case,
            hospital_admission_per100,
            bed_utilization)



new = 200
hos_min = 10
hos_max = 20
bed_min = 10
bed_max = 15


low_index = 
    (community_level_county$new_case < new & 
         community_level_county$hospital_admission_per100 < hos_min) | 
    (community_level_county$new_case < new &
         community_level_county$bed_utilization < bed_min)

medium_index = 
    (community_level_county$new_case < new &
         (community_level_county$hospital_admission_per100 > hos_min &
              community_level_county$hospital_admission_per100 < hos_max)) |
    (community_level_county$new_case < new &
         (community_level_county$bed_utilization >= bed_min &
              community_level_county$bed_utilization < bed_max)) |
    (community_level_county$new_case >= new &
         community_level_county$hospital_admission_per100 < hos_min) |
    (community_level_county$new_case >= new &
         community_level_county$bed_utilization < bed_min)

high_index = 
    (community_level_county$new_case < new & 
         community_level_county$hospital_admission_per100 >= hos_max) | 
    (community_level_county$new_case < new & 
         community_level_county$bed_utilization >= bed_max) |
    (community_level_county$new_case >= new & 
         community_level_county$hospital_admission_per100 >= hos_min) | 
    (community_level_county$new_case >= new & 
         community_level_county$bed_utilization >= bed_min) 




#######################PART I (HIGH amd MEDIUM MERGED)###################
community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "High"
community_level_county$community_level[high_index] = "High"

#remove NA value
community_level_county_computed = community_level_county %>%
    drop_na(community_level)

#save the file
save(community_level_county_computed, file="Data/CDC_community_level_county_computed_merged_Medium_With_High.csv")

######################PART II (LOW amd MEDIUM MERGED)###################
community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "Low"
community_level_county$community_level[high_index] = "High"



community_level_county_computed = community_level_county %>%
    drop_na(community_level)

save(community_level_county_computed, file="Data/CDC_community_level_county_computed_merged_Medium_With_Low.csv")

#########################LOW MEDIUM HIGH #################################
community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "Medium"
community_level_county$community_level[high_index] = "High"

community_level_county_computed = community_level_county %>%
    drop_na(community_level)

save(community_level_county_computed, file="Data/CDC_community_level_county_computed.csv")


