library(dplyr)
library(tidyr)
library(data.table)
library(maps)
library(usdata)
library(gganimate)
library(gifski)
library(gridExtra)

# load nursing home data for 2020, 2021 and 2022
nurse_file_2020 = fread("Data/faclevel_2020.csv")
nurse_file_2021 = fread("Data/faclevel_2021.csv")
nurse_file_2022 = fread("Data/faclevel_2022.csv")

# merge all data
nurse_file = rbind(nurse_file_2020,
      nurse_file_2021,
      nurse_file_2022)


colnames(nurse_file) = gsub("-", "", names(nurse_file))
colnames(nurse_file) = gsub("[()]", "", names(nurse_file))
colnames(nurse_file) = gsub(" " , "_", names(nurse_file) )
names(nurse_file)

# clean Dataset
nurse_df = nurse_file %>%
    select("Week Ending",
           "Provider State",
           "County",
           "Passed Quality Assurance Check",
           "Weekly Resident Confirmed COVID-19 Cases Per 1,000 Residents",
           "Weekly Resident COVID-19 Deaths Per 1,000 Residents",
           "Number of Residents with a New Positive COVID-19 Test Result with Positive NAAT (PCR) Test Only",
           "Percentage of Current Residents who Received a Completed COVID-19 Vaccination at Any Time") %>%
  rename(date = "Week Ending",
         state = "Provider State",
         county = "County",
         quality_check = "Passed Quality Assurance Check",
         confirmCase_per1000 = "Weekly Resident Confirmed COVID-19 Cases Per 1,000 Residents",
         deaths_per1000 = "Weekly Resident COVID-19 Deaths Per 1,000 Residents",
         positiveTest_NAAT = "Number of Residents with a New Positive COVID-19 Test Result with Positive NAAT (PCR) Test Only",
         vaccineRate = "Percentage of Current Residents who Received a Completed COVID-19 Vaccination at Any Time")

           
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
    filter(Date > "2021-01-01")







