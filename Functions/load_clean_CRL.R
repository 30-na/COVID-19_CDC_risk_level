
library(dplyr)
library(usdata)
library(data.table)

# read data from CDC Community Risk level 
CDC_risk = fread("Data/United_States_COVID-19_County_Level_of_Community_Transmission_as_Originally_Posted.csv")

#clean data
CDC_risk_clean = CDC_risk %>%
    select(report_date,
           state_name,
           county_name,
           community_transmission_level)%>%
    rename(date = report_date,
           state = state_name,
           county = county_name,
           risk_level = community_transmission_level) %>%
    mutate(date = as.Date(date, format="%Y/%m/%d"),
           state = state2abbr(state),
           risk_level = factor(risk_level,
                               levels=c("low",
                                        "moderate",
                                        "substantial",
                                        "high"))) %>%
    arrange(date, state, county) %>%
    filter(date >= "2021/01/01")

save(CDC_risk_clean, file="Data/CDC_risk_level.csv") 



