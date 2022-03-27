
library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)

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
    ggsave(paste("Result/fourWeeks", i, ".jpg", sep=""),g, height=16,width=8,scale=1.65)
    
}

for(i in seq(1, length(days)-1, by=4)){
    community_level_count = community_level_county_computed %>%
        filter(fips_code %in% common_counties) %>%
        filter(date == days[i] |
                   date == days[i+1] | 
                   date == days[i+2] |
                   date == days[i+3]) %>%
       group_by(fips_code,
                community_level) %>%
        summarise(n = n()) %>%
        arrange(fips_code)
    consistent_low = community_level_count %>%
        filter(community_level == "Low" &
                   n == 4)
    
    consistent_medium = community_level_count %>%
        filter(community_level == "Medium" &
                   n == 4)
    
    consistent_high = community_level_count %>%
        filter(community_level == "High" &
                   n == 4)
    no_consistent = community_level_count %>%
        filter(n != 4)
    
    consistant_table = data.frame("Risk.Level.Counties" = c("Low", "Medium", "High", "Total"),
                                  
                                  "Consistant" = c(nrow(consistent_low),
                                                   nrow(consistent_medium),
                                                   nrow(consistent_high),
                                                   nrow(total_consistent)),
                                  
                                  "Not.Consistant" = c(nrow(no_consistent)),
                                  
                                  "Consistency.Rate" = c(nrow(consistent_low)/nrow(community_level_count),
                                                         nrow(consistent_medium)/nrow(community_level_count),
                                                         nrow(consistent_high)/nrow(community_level_count),
                                                         nrow(total_consistent)/nrow(community_level_count)))
    
    
    fig = ggtexttable(consistant_table, rows = NULL, 
                       theme = ttheme("mOrange"))
    ggsave(paste("Result/fourWeeks.t", i, ".jpg", sep=""),fig, height=3,width=9,scale=1)
}





# two weeks interval 

for(i in seq(1, length(days)-1, by=2)){
    fourweek_interval = community_level_county_computed %>%
        filter(date == days[i] |
                   date == days[i+1]) %>%
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
    ggsave(paste("Result/twoWeeks", i, ".jpg", sep=""),g, height=16,width=8,scale=1.65)
    
}

for(i in seq(1, length(days)-1, by=2)){
    community_level_count = community_level_county_computed %>%
        filter(fips_code %in% common_counties) %>%
        filter(date == days[i] |
                   date == days[i+1]) %>%
        group_by(fips_code,
                 community_level) %>%
        summarise(n = n()) %>%
        arrange(fips_code)
    consistent_low = community_level_count %>%
        filter(community_level == "Low" &
                   n == 2)
    
    consistent_medium = community_level_count %>%
        filter(community_level == "Medium" &
                   n == 2)
    
    consistent_high = community_level_count %>%
        filter(community_level == "High" &
                   n == 2)
    
    total_consistent = community_level_count %>%
        filter(n == 2)
    
    no_consistent = community_level_count %>%
        filter(n != 2)
    
    consistant_table = data.frame("Risk.Level.Counties" = c("Low", "Medium", "High", "Total"),
                                  
                                  "Consistant" = c(nrow(consistent_low),
                                                   nrow(consistent_medium),
                                                   nrow(consistent_high),
                                                   nrow(total_consistent)),
                                  
                                  "Not.Consistant" = c(nrow(no_consistent)),
                                  
                                  "Consistency.Rate" = c(nrow(consistent_low)/nrow(community_level_count),
                                                         nrow(consistent_medium)/nrow(community_level_count),
                                                         nrow(consistent_high)/nrow(community_level_count),
                                                         nrow(total_consistent)/nrow(community_level_count)))
    
    
    fig = ggtexttable(consistant_table, rows = NULL, 
                      theme = ttheme("mOrange"))
    ggsave(paste("Result/twoWeeks.t", i, ".jpg", sep=""),fig, height=3,width=9,scale=1)
    
    
}







