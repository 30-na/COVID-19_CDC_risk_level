library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)

load("Data/CDC_community_level_county.csv")
load("Data/CDC_community_level_county_computed.csv")

#top 50 county base on the population

top50_county = CDC_community_level_county %>%
    arrange(desc(population)) %>% 
    slice(1:100)



top50_county_list = unique(top50_county$county_fips)

consistent_CRL = CDC_community_level_county %>%
    dplyr::filter(county_fips %in% top50_county_list)


fig6 = ggplot(consistent_CRL, aes(x=date_updated, y=risk_level,
                                  group=1, color=risk_level))+
    geom_point()+
    geom_line()+
    facet_wrap(~county_fips, ncol=5)+
    scale_x_date(date_breaks = "1 week",
                 date_labels = "%b/%d",
                 date_minor_breaks = "1 day",
                 name= "Updated Date")+
    labs(title="Community Risk Level Consistency in 50 most populous counties")

ggsave("Result/Fig6.jpg",fig6, height=8,width=4,scale=1.65)

fig7 = ggplot(na.omit(CDC_risk_clean_new), aes(x=risk_level,y=bed_utilization, color=risk_level))+
    geom_jitter(position = position_jitter(width = 0.02))+
    geom_boxplot(alpha = 0.7)+
    coord_flip()+
    geom_rug()+
    labs(title="Bed Utilizataion rate in counties with three diferent Risk Level")
    

ggsave("Result/Fig7.jpg",fig7, height=4,width=8,scale=1.65)

fig8 = ggplot(na.omit(CDC_risk_clean_new), aes(x=risk_level,y=hospital_admission, color=risk_level))+
    geom_jitter(position = position_jitter(width = 0.02))+
    geom_boxplot(alpha = 0.7)+
    coord_flip()+
    geom_rug()+
    labs(title="Hospital Admission per 100k in counties with three diferent Risk Level")

ggsave("Result/Fig8.jpg",fig8, height=4,width=8,scale=1.65)


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


fig9 = ggtexttable(consistant_table, rows = NULL, 
                   theme = ttheme("mOrange"))


ggsave("Result/Fig9.jpg",fig9, height=3,width=9,scale=1)
