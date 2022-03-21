
library(dplyr)
library(usdata)
library(data.table)

# read data from CDC Community Risk level 
strigency_index = fread("Data/OxCGRT_latest_combined.csv")
strigency_state = strigency_index %>%
    select(CountryCode,
           RegionName,
           Date,
           StringencyIndex) %>%
    rename(state = RegionName) %>%
    filter(CountryCode == "USA") %>%
    mutate(Date = as.Date(as.character(Date),
                          origin = "1964-10-22",
                          format="%Y%m%d"),
           state = state2abbr(state)) %>%
    filter(Date >= "2021-01-01" ) %>%
    na.omit()
   
    
                             
