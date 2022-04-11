library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(tidyr)
library(usmap)
library(maps)
library(tidycensus)
library(gridExtra)

load("Data/CDC_community_level_county_computed.csv")


# days list
days = unique(community_level_county_computed$date)

common_counties_df = community_level_county_computed %>%
   group_by(state, fips_code)%>%
  count(fips_code)%>%
    filter(n == length(days))%>%
    select(state, fips_code) %>%
    mutate(state = tolower(abbr2state(state)))
    


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


consis_3weeks = c()
for(i in 1:nrow(consis)){
    consis_3weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i+1],
                                       consis$community_level[i+2])))
    
}


consis_2weeks = c()
for(i in 2:nrow(consis)){
    consis_2weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i-1])))
    
}




############# 3-weeeks ####################################################
consis$consis_3weeks = consis_3weeks

consis_plot_3 = consis %>%
    filter(date <= "2022-03-04") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)%>%
    filter(consis_3weeks == 1)

a3week1 = ggplot(consis_plot_3, aes(x=date, y=consisRate,
                                    color=community_level))+
    
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    facet_wrap(~community_level, nrow=3)+
    labs(title="Proportion of county with consistance Comunity level risk in 3weeks (polynomial regression)")

ggsave("Result/aa3week_consis_Rate01.jpg", a3week1, height=4, width=8, scale=1.65)




a3week2 = ggplot(consis_plot_3, aes(x=date, y=consisRate,
                                     color=community_level))+
    
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="Proportion of county with consistance Comunity level risk in 3weeks (polynomial regression)")

ggsave("Result/aa3week_consis_Rate02.jpg", a3week2, height=4, width=8, scale=1.65)





consis_plot_3_2 = consis %>%
    filter(date > "2020-08-07") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)


consis_plot_3_3 = consis %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date) %>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    filter(consis_3weeks == 1)

a3week4 = ggplot(consis_plot_3_3, aes(x=date,
                                      y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21))+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="total proportion of consistant county in 3weeks")

ggsave("Result/aa3week_consis_total.jpg", a3week4, height=4,width=8,scale=1.65)


a3week5 = ggplot(consis_plot_3_3, aes(x=consisRate))+
    geom_histogram(alpha = .3, col="black")+
    geom_density(fill = "steelblue", alpha=.2)+
    geom_rug()+
    theme_bw()+
    labs(title="3week consistancy distribution county")


ggsave("Result/aa3week_consis_total_density.jpg", a3week5, height=4,width=8,scale=1.65)




a3week6 = usmap::plot_usmap(regions = "counties") + 
    labs(title = "US Counties",
         subtitle = "Map of the counties with available data.") + 
    theme(panel.background = element_rect(color = "black", fill = "lightblue"))

ggsave("Result/aa3week_consis_map.jpg", a3week6, height=4,width=8,scale=1.65)


data(fips_codes)



common_fips = fips_codes %>%
    mutate(fips = paste(state_code, county_code, sep = "")) %>%
    mutate(state_name = tolower(state_name)) %>%
    filter(fips %in% common_counties_df$fips_code &
               state_name %in% common_counties_df$state) %>%
    mutate(county = tolower(county)) %>%
    mutate(county = gsub(pattern = " county",
                         replacement = "",
                         county))%>%
    mutate(state_county = paste(state_name, county))


us_county = map_data("county")

us_state = map_data("state")

common_fips_map = us_county %>%
    mutate(state_county_map = paste(region, subregion))%>%
    filter(state_county_map %in% common_fips$state_county)%>%
    select(-state_county_map)

cnames = us_state %>%
    group_by(region) %>%
    mutate(long = mean(range(long)))%>%
    mutate(lat = mean(range(lat))) %>%
    mutate(region = state2abbr(region)) %>%
    select(region, long, lat, group) %>%
    distinct()

a3weeks7 = ggplot(data = us_county,
       mapping = aes(x = long,
                     y = lat, 
                     group = group))+
    
    geom_polygon(color = "#636363",
                 fill = NA,
                 size = 0.05) +
    
    geom_polygon(data = us_state,
                 mapping = aes(long,
                               lat,
                               group = group),
                 fill = NA, 
                 color = "black",
                 size = .3) +
    
    geom_polygon(data = common_fips_map,
                 fill = "#fed98e",
                 alpha=.5)+
    
    geom_text(data=cnames, aes(long, lat, label = region), size=3)+
    
    coord_equal()+
    
    labs(title = "US Counties",
                       subtitle = "Map of the counties with available data.")


ggsave("Result/aa3week_consis_map.jpg", a3weeks7, height=4,width=8,scale=1.65)



    
length(unique(community_level_county_computed$state))
length(unique(community_level_county_computed$fips_code))
length(unique(common_counties_df$state))
length(unique(common_counties_df$fips_code))


a3week8 = ggplot(consis_plot_3_3, aes(y=consis_3weeks, x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    geom_rug()+
    theme_bw()+
    labs(title="Total proportion of consistant county in 3weeks")

ggsave("Result/aa3week_consis_total_box.jpg", a3week8, height=2,width=8,scale=1.65)



a3week9 = community_level_county_computed %>%
    group_by(date) %>%
    count(community_level) %>%
    mutate(total = sum(n)) %>%
    mutate(counties_proportion = round(n/total, 2)) %>%
    
    ggplot(aes(x = date,
               y = counties_proportion,
               color = community_level)) +
    geom_point(alpha=.5)+
    geom_smooth(method = "lm",
                formula = y~ poly(x, 15))+
    facet_wrap(.~community_level, nrow = 3)+
    scale_fill_manual(values=c("#ffeda0", "#E69F00", "#56B4E9"))+
    labs(title="proportion of counties in each risk level per week")

ggsave("Result/aa3week_consis_total_box.jpg", a3week9, height=4,width=8,scale=1.65)


a3week11_count = consis %>%
    dplyr::filter(date <= "2022-03-04") %>%
    group_by(date) %>%
    count(community_level) %>%
    rename("count_community_level" = n)


a3week11_consis = consis %>%
    mutate(consis_3weeks = consis_3weeks) %>%
    dplyr::filter(date <= "2022-03-04") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    group_by(date, community_level) %>% 
    count(consis_3weeks)

  
a3week11 = merge(a3week11_count, a3week11_consis)%>%
    mutate(consisRate = round(n/count_community_level, 2)) %>%
    filter(consis_3weeks == 1)%>%
    
    
    ggplot( aes(x=date,y=consisRate,
                              color=community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    facet_wrap(~community_level, nrow=3)+
    labs(title="consistancy rate in each risk level per week (3 weeks)")

ggsave("Result/aa3week_consis_Rate011.jpg", a3week11, height=4, width=8, scale=1.65)




a3week10 = grid.arrange(a3week9, a3week11, nrow=1)
ggsave("Result/aa3week_mix.jpg", a3week10, height=4,width=8,scale=1.65)

