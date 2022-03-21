library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)


# load datasets
load("Data/hospitalization_county.csv")
load("Data/CDC_community_transmission_county.csv")


new_cases = CDC_community_transmission %>%
    select(date,
           fips_code,
           new_case)


#When the total new case rate metric ("cases_per_100K_7_day_count_change")
#is greater than zero and less than 10, this metric is set to "suppressed"
new_cases$new_case[new_cases$new_case == "suppressed"] = 5
merged_data = merge(hospitalization_county,
                    new_cases,
                    by=c("date", "fips_code"))

community_level_county = merged_data %>%
    mutate(hospital_admission_per100 = round((admission_county/population)*100000))

community_level_county$community_level = NA

low_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 < 10) | 
    (community_level_county$new_case < 200 &
         community_level_county$accupied_rate < 10)

medium_index = 
    (community_level_county$new_case < 200 &
         (community_level_county$hospital_admission_per100 > 10 &
              community_level_county$hospital_admission_per100 < 20)) |
    (community_level_county$new_case < 200 &
            (community_level_county$accupied_rate > 10 &
              community_level_county$accupied_rate < 25)) |
    (community_level_county$new_case > 200 &
         community_level_county$hospital_admission_per100 < 10) |
    (community_level_county$new_case > 200 &
         community_level_county$accupied_rate < 10)

high_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 >= 20) | 
    (community_level_county$new_case < 200 & 
         community_level_county$accupied_rate >= 15) |
    (community_level_county$new_case >= 200 & 
         community_level_county$hospital_admission_per100 >= 10) | 
    (community_level_county$new_case >= 200 & 
         community_level_county$accupied_rate >= 10) 
    

community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "Medium"
community_level_county$community_level[high_index] = "High"
   
community_level_county_computed = community_level_county

save(community_level_county_computed, file="Data/CDC_community_level_county_computed.csv")
