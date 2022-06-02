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
    select(Week_Ending,
           Provider_State,
           County,
           Number_of_Residents_with_a_New_Positive_COVID19_Test_Result_with_Positive_NAAT_PCR_Test_Only,
           ) %>%
  rename("date" = Week_Ending,
         "state" = Provider_State,
         "county" = County,
         "positive_test" = Number_of_Residents_with_a_New_Positive_COVID19_Test_Result_with_Positive_NAAT_PCR_Test_Only)
           
           
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
    filter(Date > "2021-01-01")







