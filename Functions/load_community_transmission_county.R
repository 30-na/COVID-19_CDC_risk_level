
library(dplyr)
library(usdata)
library(data.table)

# read data from CDC Community Risk level 
CDC_risk = fread("Data/United_States_COVID-19_County_Level_of_Community_Transmission_as_Originally_Posted.csv")

#clean data
CDC_community_transmission = CDC_risk %>%
    rename(date = report_date,
           state = state_name,
           county = county_name,
           new_case = cases_per_100K_7_day_count_change,
           positive_test = percent_test_results_reported_positive_last_7_days,
           risk_level = community_transmission_level) %>%
    mutate(date = as.Date(date, format="%Y/%m/%d"),
           state = state2abbr(state),
           risk_level = factor(risk_level,
                               levels=c("low",
                                        "moderate",
                                        "substantial",
                                        "high"))) %>%
    arrange(date, state, fips_code) %>%
    filter(date >= "2021/01/01")

save(CDC_community_transmission, file="Data/CDC_community_transmission_county_original.csv") 
