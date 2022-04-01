
library(dplyr)
library(usdata)
library(data.table)

# read data from CDC Community Risk level 
CDC_risk_new = fread("Data/United_States_COVID-19_Community_Levels_by_County.csv")

#clean data
CDC_community_level_county = CDC_risk_new %>%
    select(c(date_updated,
           state,
           county_fips,
           total_population,
           covid_hospital_admissions_per_100k,
           covid_inpatient_bed_utilization) |
           ends_with("community_level"))%>%
    rename(risk_level = ends_with("community_level"),
           population = total_population,
           bed_utilization = covid_inpatient_bed_utilization,
           hospital_admission = covid_hospital_admissions_per_100k)%>%
    mutate(date_updated = as.Date(date_updated, format="%Y/%m/%d"),
           state = state2abbr(state),
           risk_level = factor(risk_level,
                               levels=c("Low",
                                        "Medium",
                                        "High"))) %>%
    arrange(date_updated, state, county_fips) 


save(CDC_community_level_county, file="Data/CDC_community_level_county.csv") 



