
library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)

load("Data/CDC_community_level_county.csv")
load("Data/CDC_community_level_county_computed.csv")


# days list
days = unique(community_level_county_computed$date)


# list of counties which are common in all days
common_counties = names(table(community_level_county_computed$fips_code)[table(community_level_county_computed$fips_code) == length(days)])


# top 50 population counties list
top50_county = community_level_county_computed %>%
    filter(fips_code %in% common_counties)%>%
    distinct(fips_code,
             population,
             .keep_all=TRUE)%>%
    arrange(desc(population)) %>%
    slice(1:50)%>%
    select(fips_code)


# four weeks interval 

for(i in seq(1, length(days)-1, by=4)){
    fourweek_interval = community_level_county_computed %>%
        filter(date == days[i] |
                   date == days[i+1] | 
                   date == days[i+2] |
                   date == days[i+3]) %>%
        filter(fips_code %in% top50_county$fips_code)
    
    
    g = ggplot(fourweek_interval, aes(x=date, y=community_level,
                                               group=1, color=community_level))+
        geom_point()+
        geom_line()+
        facet_wrap(~fips_code, ncol=5)+
        scale_x_date(date_breaks = "1 week",
                     date_labels = "%Y/%b",
                     date_minor_breaks = "1 day",
                     name= "Updated Date")+
        labs(title="computed Community level in 50 counties")
    ggsave(paste("Result/oneMonth", i, ".jpg", sep=""),g, height=16,width=8,scale=1.65)
    
}


