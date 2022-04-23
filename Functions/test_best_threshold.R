#library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(maps)
library(tidycensus)
library(gridExtra)


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


# compute community_level
community_level_county$community_level = NA

new = seq(150, 250, by = 10)
hos_min = seq(5, 25, by = 1)
hos_max = seq(5, 25, by = 1)
bed_min = seq(5, 20, by = 1)
bed_max = seq(5, 20, by = 1)

# new = 200
# hos_min = 10
# hos_max = 20
# bed_min = 10
# bed_max = 15

level_threshold = function(new,
                               hos_min,
                               hos_max,
                               bed_min,
                               bed_max){
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
         (community_level_county$bed_utilization > bed_min &
              community_level_county$bed_utilization < bed_max)) |
    (community_level_county$new_case > new &
         community_level_county$hospital_admission_per100 < hos_min) |
    (community_level_county$new_case > new &
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
return(list(low_index,
            medium_index,
            high_index))
}

result=0
result.names = c("mean", "median", "n", "b1", "b2", "h3", "h4")

for(n in new){
    for(b1 in bed_min){
        for(b2 in bed_max){
            for(h1 in hos_min){
                for(h2 in hos_max){

                    
                    cdc = level_threshold(new = n,
                                          hos_min = h1,
                                          hos_max = h2,
                                          bed_min = b1,
                                          bed_max = b2)
                    
                    low_index = cdc[[1]]
                    medium_index = cdc[[2]]
                    high_index = cdc[[3]]
                    
                    
                    community_level_county$community_level[low_index] = "Low"
                    community_level_county$community_level[medium_index] = "Medium"
                    community_level_county$community_level[high_index] = "High"
                    
                    community_level_county_computed = community_level_county %>%
                        drop_na(community_level)
                    
                    
                    # days list
                    days = unique(community_level_county_computed$date)
                    
                    common_counties_df = community_level_county_computed %>%
                        group_by(state, fips_code)%>%
                        count(fips_code)%>%
                        filter(n == length(days))%>%
                        select(state, fips_code) %>%
                        mutate(state = tolower(abbr2state(state)))
                    
                    
                    
                    # list of counties which are common in all days
                    common_counties = names(table(community_level_county_computed$fips_code)[table(community_level_county_computed$fips_code) == length(days)])
                    
                    
                    #filter common counties data
                    CL_common = community_level_county_computed %>% 
                        dplyr::filter(fips_code %in% common_counties)
                    
                    
                    
                    # four weeks interval 
                    
                    consis = CL_common %>%
                        select(date,
                               fips_code,
                               community_level) %>%
                        group_by(fips_code) %>%
                        arrange(fips_code,
                                date)
                    
                    
                    
                    
                    # 
                    # consis_4weeks = c()
                    # for(i in 4:nrow(consis)){
                    #     consis_4weeks[i] = length(unique(c(consis$community_level[i],
                    #                                        consis$community_level[i-1],
                    #                                        consis$community_level[i-2],
                    #                                        consis$community_level[i-3])))
                    #     
                    # }
                    
                    
                    consis_3weeks = c()
                    for(i in 1:nrow(consis)){
                        consis_3weeks[i] = length(unique(c(consis$community_level[i],
                                                           consis$community_level[i+1],
                                                           consis$community_level[i+2])))
                        
                    }
                    
                    # 
                    # consis_2weeks = c()
                    # for(i in 2:nrow(consis)){
                    #     consis_2weeks[i] = length(unique(c(consis$community_level[i],
                    #                                        consis$community_level[i-1])))
                    #     
                    # }
                    
                    
                    
                    
                    ############# 3-weeeks ####################################################
                    consis$consis_3weeks = consis_3weeks
                    
                    consis_plot_3_3 = consis %>%
                        mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
                        arrange(date) %>%
                        group_by(date) %>%
                        count(consis_3weeks) %>%
                        mutate(total_community_level = sum(n)) %>%
                        mutate(consisRate = n/total_community_level)%>%
                        filter(consis_3weeks == 1)
                    
                    mean = mean(consis_plot_3_3$consisRate)
                    median = median(consis_plot_3_3$consisRate)
                    
                    result = rbind(result, 
                                   c(mean,
                                     median,
                                     n,
                                     b1,
                                     b2,
                                     h1,
                                     h2))
                    
                    
                }
            }
        }
    }
}





