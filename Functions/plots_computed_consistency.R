
library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)


load("Data/CDC_community_level_county_computed.csv")





# days list
days = unique(community_level_county_computed$date)


# list of counties which are common in all days
common_counties = names(table(community_level_county_computed$fips_code)[table(community_level_county_computed$fips_code) == length(days)])


#filter common counties data
CL_common = community_level_county_computed %>% 
    dplyr::filter(fips_code %in% common_counties)



# four weeks interval 

consis = CL_common %>%
    select(date,
           fips_code,
           community_level) %>%
    group_by(fips_code) %>%
    arrange(fips_code,
            date)
consis_4weeks = c()
for(i in 4:nrow(consis)){
    consis_4weeks[i] = length(unique(c(consis$community_level[i],
                 consis$community_level[i-1],
                 consis$community_level[i-2],
                 consis$community_level[i-3])))
   
    }
    
consis$consis_4weeks = consis_4weeks

consis = filter(consis, date > "2020-08-14")


consis_plot = consis %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_4weeks) %>%
    mutate(consisRate = n/length(common_counties))%>%
    filter(consis_4weeks == 1)%>%
    arrange(date, community_level)




b3 = ggplot(consis_plot, aes(x=date, y=consisRate,
                                  group=1, color=community_level))+
    
    geom_line()+
    labs(title="Community Risk Level Consistency-4weeks")+
    facet_wrap(~community_level, ncol=3)

ggsave("Result/b3.jpg",b1, height=4,width=8,scale=1.65)




#################################################
consis_2weeks = c()
for(i in 2:nrow(consis)){
    consis_2weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i-1])))
    
}

consis$consis_2weeks = consis_2weeks

consis = filter(consis, date > "2020-08-14")


consis_plot = consis %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_2weeks) %>%
    mutate(consisRate2 = n/length(common_counties))%>%
    filter(consis_2weeks == 1)%>%
    arrange(date, community_level)




b2 = ggplot(consis_plot, aes(x=date, y=consisRate2,
                             group=1, color=community_level))+
    
    geom_line()+
    labs(title="Community Risk Level Consistency-2weeks")+
    facet_wrap(~community_level, ncol=3)

ggsave("Result/b2.jpg",b2, height=4,width=8,scale=1.65)
