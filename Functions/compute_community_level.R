library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)
library(tidyr)

# load datasets
load("Data/hospital_utilization_county.csv")
load("Data/CDC_community_transmission_county_historical.csv")


new_cases = CDC_community_risk_historical %>%
    select(date,
           fips_code,
           new_case)

new_cases[new_cases$new_case < 0, ]$new_case = NA



#When the total new case rate metric ("cases_per_100K_7_day_count_change")
#is greater than zero and less than 10, this metric is set to "suppressed"

merged_newcase = merge(hospital_utilization,
                    new_cases,
                    by=c("date", "fips_code"))

# add county population from community level files
load("Data/CDC_community_level_county.csv")
county_pop = CDC_community_level_county %>%
    dplyr::filter(date_updated == "2022-03-24") %>%
    dplyr::select(county_fips, 
           population) %>%
    rename(fips_code = county_fips)

merged_data = merge(merged_newcase,
                       county_pop,
                       by="fips_code")

community_level_county = merged_data %>%
    mutate(hospital_admission_per100 = round((hospital_admissions/population)*100000))




# remove NA value
community_level_county = community_level_county %>%
    drop_na(new_case,
            hospital_admission_per100,
            bed_utilization)


community_level_county$community_level = NA


low_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 < 10) | 
    (community_level_county$new_case < 200 &
         community_level_county$bed_utilization < 10)

medium_index = 
    (community_level_county$new_case < 200 &
         (community_level_county$hospital_admission_per100 > 10 &
              community_level_county$hospital_admission_per100 < 20)) |
    (community_level_county$new_case < 200 &
            (community_level_county$bed_utilization > 10 &
              community_level_county$bed_utilization < 25)) |
    (community_level_county$new_case > 200 &
         community_level_county$hospital_admission_per100 < 10) |
    (community_level_county$new_case > 200 &
         community_level_county$bed_utilization < 10)

high_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 >= 20) | 
    (community_level_county$new_case < 200 & 
         community_level_county$bed_utilization >= 15) |
    (community_level_county$new_case >= 200 & 
         community_level_county$hospital_admission_per100 >= 10) | 
    (community_level_county$new_case >= 200 & 
         community_level_county$bed_utilization >= 10) 
    

community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "Medium"
community_level_county$community_level[high_index] = "High"
   


community_level_county_computed = community_level_county %>%
    drop_na(community_level)

save(community_level_county_computed, file="Data/CDC_community_level_county_computed.csv")





# Computed Hospital Admission per 100k in counties with three diferent community level plot
hos_ad = ggplot((community_level_county_computed), aes(x=community_level,
                                            y=hospital_admission_per100,
                                            color=community_level))+
    geom_jitter(position = position_jitter(width = 0.02))+
    geom_boxplot(alpha = 0.7, outlier.shape = NA)+
    coord_flip()+
    geom_rug()+
    labs(title="Computed Hospital Admission per 100k in counties with three diferent community level")

ggsave("Result/c_hos.jpg", hos_ad, height=4,width=8,scale=1.65)

new_case = ggplot((community_level_county_computed), aes(x=community_level,
                                                     y=new_case,
                                                     color=community_level))+
    geom_jitter(position = position_jitter(width = 0.02))+
    geom_boxplot(alpha = 0.7, outlier.shape = NA)+
    coord_flip()+
    geom_rug()+
    labs(title="New cases in counties with three diferent community level")

ggsave("Result/c_newcase.jpg", new_case, height=4,width=8,scale=1.65)


acc_rate = ggplot((community_level_county_computed), aes(x=community_level,
                                                       y=bed_utilization,
                                                       color=community_level))+
    geom_jitter(position = position_jitter(width = 0.02))+
    geom_boxplot(alpha = 0.7, outlier.shape = NA)+
    coord_flip()+
    geom_rug()+
    labs(title="Accupied Rate in counties with three diferent community level")

ggsave("Result/c_accupiedRate.jpg", acc_rate, height=4,width=8,scale=1.65)


