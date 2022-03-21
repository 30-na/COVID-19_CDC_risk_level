
library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)

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


## top50 county in september 2021
top50_county = community_level_county %>%
    filter(date>="2021-09-01" & date<"2021-10-01")%>%
    distinct(fips_code,
             population,
             .keep_all=TRUE)%>%
    arrange(desc(population)) %>%
    slice(1:50)%>%
    select(fips_code)

community_level_county_sep = community_level_county %>%
    filter(date>="2021-09-01" & date<"2021-10-01")
    

# computed community level
consistent_CRL_computed = community_level_county_sep %>%
    dplyr::filter(community_level_county_sep$fips_code %in% top50_county$fips_code)



fig9 = ggplot(consistent_CRL_computed, aes(x=date, y=community_level,
                                  group=1, color=community_level))+
    geom_point()+
    geom_line()+
    facet_wrap(~fips_code, ncol=5)+
    scale_x_date(date_breaks = "1 week",
                 date_labels = "%d",
                 date_minor_breaks = "1 day",
                 name= "Updated Date")+
    labs(title="September 2021 computed Community level in 50 counties")

ggsave("Result/Fig9.jpg",fig9, height=8,width=4,scale=1.65)

