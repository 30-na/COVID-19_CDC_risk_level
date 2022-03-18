library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)

load("Data/CDC_risk_level_new.csv")

fig7 = ggplot(na.omit(CDC_risk_clean_new), aes(x=risk_level,y=bed_utilization, color=risk_level))+
    geom_jitter(position = position_jitter(width = 0.02))+
    geom_boxplot(alpha = 0.7)+
    coord_flip()+
    geom_rug()
    

ggsave("Result/Fig7.jpg",fig7, height=4,width=8,scale=1.65)



first_low = CDC_risk_clean_new %>%
    filter(date_updated == c("2022-03-03") &
           risk_level == "Low") %>%
    select(county_fips)
no_consistent_low = CDC_risk_clean_new %>%
    filter(date_updated == c("2022-03-10") &
               risk_level != "Low" &
               county_fips %in% first_low$county_fips)
consistent_low = CDC_risk_clean_new %>%
    filter(date_updated == c("2022-03-10") &
               risk_level == "Low" &
               county_fips %in% first_low$county_fips)

nrow(no_consistent_low)
nrow(first_low)
nrow(consistent_low)


first_medium = CDC_risk_clean_new %>%
    filter(date_updated == c("2022-03-03") &
               risk_level == "Medium") %>%
    select(county_fips)
no_consistent_medium = CDC_risk_clean_new %>%
    filter(date_updated == c("2022-03-10") &
               risk_level != "Medium" &
               county_fips %in% first_medium$county_fips)
consistent_medium = CDC_risk_clean_new %>%
    filter(date_updated == c("2022-03-10") &
               risk_level == "Medium" &
               county_fips %in% first_medium$county_fips)
nrow(no_consistent_medium)
nrow(consistent_medium)
nrow(first_medium)

first_high = CDC_risk_clean_new %>%
    filter(date_updated == c("2022-03-03") &
               risk_level == "High") %>%
    select(county_fips)
no_consistent_high = CDC_risk_clean_new %>%
    filter(date_updated == c("2022-03-10") &
               risk_level != "High" &
               county_fips %in% first_high$county_fips)
consistent_high = CDC_risk_clean_new %>%
    filter(date_updated == c("2022-03-10") &
               risk_level == "High" &
               county_fips %in% first_high$county_fips)
nrow(consistent_high)
nrow(first_high)
nrow(no_consistent_high)

consistant_table=data.frame("Risk.Level.Counties" = c("Low", "Medium", "High"),
                            
                            "Consistant" = c(nrow(consistent_low),
                                                 nrow(consistent_medium),
                                                 nrow(consistent_high)),
                            
                            "Not.Consistant" = c(nrow(no_consistent_low),
                                                 nrow(no_consistent_medium),
                                                 nrow(no_consistent_high)),
                            "Consistency.Rate" = c(nrow(consistent_low)/nrow(first_low),
                                                   nrow(consistent_medium)/nrow(first_medium),
                                                   nrow(consistent_high)/nrow(first_high)))


fig8 = ggtexttable(consistant_table, rows = NULL, 
                   theme = ttheme("mOrange"))


ggsave("Result/Fig8.jpg",fig8, height=3,width=9,scale=1)
