
# read data from CDC Community Risk level Historical
CDC_risk_his = fread("Data/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes.csv")

# When the total new case rate metric ("cases_per_100K_7_day_count_change") is greater than
# zero and less than 10, this metric is set to "suppressed" rather than including its value
# to protect individual privacy.
# I put 5 for for suppressed value


CDC_community_transmission_his = CDC_risk_his %>%
    rename(state = state_name,
           county = county_name,
           new_case = cases_per_100K_7_day_count_change,
           positive_test = percent_test_results_reported_positive_last_7_days,
           risk_level = community_transmission_level)
CDC_risk_clean = CDC_community_transmission_his %>%
    mutate(date = as.Date(date, format="%m/%d/%Y")) %>%
    mutate(new_case = gsub(",", "", CDC_community_transmission_his$new_case)) %>%
    mutate(new_case = replace(new_case, new_case == "suppressed", 5)) %>%
    mutate(new_case = as.numeric(new_case)) %>%
    mutate(risk_level = factor(risk_level,
                               levels=c("low",
                                        "moderate",
                                        "substantial",
                                        "high"))) %>%
    mutate(positive_test = as.numeric(positive_test)) %>%
    
    arrange(date, state, fips_code)

save(CDC_risk_clean, file="Data/CDC_community_transmission_county_historical.csv") 



