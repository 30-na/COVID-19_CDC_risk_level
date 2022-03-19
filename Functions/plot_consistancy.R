
library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)

load("Data/CDC_community_level_county.csv")


#top 50 county base on the population

top50_county = CDC_risk_clean_new %>%
    arrange(desc(population)) %>% 
    slice(1:100)
    
top50_county_list = unique(top50_county$county_fips)

consistent_CRL = CDC_risk_clean_new %>%
    dplyr::filter(county_fips %in% top50_county_list)

fig6 = ggplot(consistent_CRL, aes(x=date_updated, y=risk_level,
                                  group=1, color=risk_level))+
    geom_point()+
    geom_line()+
    facet_wrap(~county_fips, ncol=5)+
    scale_x_date(date_breaks = "1 week",
                 date_labels = "%b/%d",
                 date_minor_breaks = "1 day",
                 name= "Updated Date")

ggsave("Result/Fig6.jpg",fig6, height=8,width=4,scale=1.65)

