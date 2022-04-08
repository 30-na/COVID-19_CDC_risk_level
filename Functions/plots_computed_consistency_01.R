
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


consis_3weeks = c()
for(i in 3:nrow(consis)){
    consis_3weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i-1],
                                       consis$community_level[i-2])))
    
}


consis_2weeks = c()
for(i in 2:nrow(consis)){
    consis_2weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i-1])))
    
}



############# 4-weeeks ###########################################

consis$consis_4weeks = consis_4weeks


consis_plot_4 = consis %>%
    filter(date > "2020-08-14") %>%
    mutate(consis_4weeks = replace(consis_4weeks, consis_4weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_4weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)%>%
    filter(consis_4weeks == 1)




a4week1 = ggplot(consis_plot_4, aes(x=date, y=consisRate,
                             group=1, color=community_level))+
    
    geom_line()+
    facet_wrap(~community_level, ncol=1)+
    labs(title="Proportion of county with consistance Comunity level risk in 4weeks")

ggsave("Result/a4week_consis_Rate.jpg", a4week1, height=4,width=8,scale=1.65)

a4week2 = ggplot(consis_plot_4, aes(x=date, y=consisRate,
                               group=1, color=community_level))+
    
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    facet_wrap(~community_level, nrow=1)+
    theme_bw()+
    labs(title="Proportion of county with consistance Comunity level risk in 4weeks (polynomial regression)")

ggsave("Result/a4week_consis_Rate01.jpg", a4week2, height=4, width=8, scale=1.65)





consis_plot_4_2 = consis %>%
    filter(date > "2020-08-14") %>%
    mutate(consis_4weeks = replace(consis_4weeks, consis_4weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_4weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)

a4week3 = ggplot(consis_plot_4_2, aes(x=date,
                               y=n,
                               color = as.factor(consis_4weeks)))+
    
    geom_line()+
    geom_point()+
    facet_wrap(~community_level, nrow=3)+
    labs(title="Number of consistant and unconsistant county in 4weeks")

ggsave("Result/a4week_consis_number.jpg", a4week3, height=4,width=8,scale=1.65)


consis_plot_3 = consis %>%
    mutate(consis_4weeks = replace(consis_4weeks, consis_4weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date) %>%
    count(consis_4weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    filter(consis_4weeks == 1)

a4week4 = ggplot(consis_plot_3, aes(x=date,
                               y=consisRate))+
    
    geom_line()+
    geom_point(alpha = .5)+
    labs(title="total proportion of consistant county in 4weeks")

ggsave("Result/a4week_consis_total.jpg", a4week4, height=4,width=8,scale=1.65)



############# 3-weeeks ####################################################
consis$consis_3weeks = consis_3weeks

consis_plot_3 = consis %>%
    filter(date > "2020-08-07") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)%>%
    filter(consis_3weeks == 1)




a3week1 = ggplot(consis_plot_3, aes(x=date, y=consisRate,
                                 group=1, color=community_level))+
    
    geom_line()+
    facet_wrap(~community_level, ncol=1)+
    labs(title="Proportion of county with consistance Comunity level risk in 3weeks")

ggsave("Result/a3week_consis_Rate.jpg", a3week, height=4,width=8,scale=1.65)

a3week2 = ggplot(consis_plot_3, aes(x=date, y=consisRate,
                                 group=1, color=community_level))+
    
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    facet_wrap(~community_level, nrow=1)+
    theme_bw()+
    labs(title="Proportion of county with consistance Comunity level risk in 3weeks (polynomial regression)")

ggsave("Result/a3week_consis_Rate01.jpg", a3week2, height=4, width=8, scale=1.65)





consis_plot_3_2 = consis %>%
    filter(date > "2020-08-07") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)

a3week3 = ggplot(consis_plot_3_2, aes(x=date,
                                 y=n,
                                 color = as.factor(consis_3weeks)))+
    
    geom_line()+
    geom_point()+
    facet_wrap(~community_level, nrow=3)+
    labs(title="Number of consistant and unconsistant county in 3weeks")

ggsave("Result/a3week_consis_number.jpg", a3week3, height=4,width=8,scale=1.65)


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
    
    geom_line()+
    geom_point(alpha = .5)+
    labs(title="total proportion of consistant county in 3weeks")

ggsave("Result/a3week_consis_total.jpg", a3week4, height=4,width=8,scale=1.65)


############# 2-weeeks ###########################################

consis$consis_2weeks = consis_2weeks


consis_plot_2 = consis %>%
    filter(date > "2020-07-31") %>%
    mutate(consis_2weeks = replace(consis_2weeks, consis_2weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_2weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)%>%
    filter(consis_2weeks == 1)




a2week1 = ggplot(consis_plot_2, aes(x=date, y=consisRate,
                                    group=1, color=community_level))+
    
    geom_line()+
    facet_wrap(~community_level, ncol=1)+
    labs(title="Proportion of county with consistance Comunity level risk in 2weeks")

ggsave("Result/a2week_consis_Rate.jpg", a2week1, height=4,width=8,scale=1.65)

a2week2 = ggplot(consis_plot_2, aes(x=date, y=consisRate,
                                    group=1, color=community_level))+
    
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    facet_wrap(~community_level, nrow=1)+
    theme_bw()+
    labs(title="Proportion of county with consistance Comunity level risk in 2weeks (polynomial regression)")

ggsave("Result/a2week_consis_Rate01.jpg", a2week2, height=4, width=8, scale=1.65)





consis_plot_2_2 = consis %>%
    filter(date > "2020-07-31") %>%
    mutate(consis_2weeks = replace(consis_2weeks, consis_2weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date, community_level) %>%
    count(consis_2weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    arrange(date, community_level)

a2week3 = ggplot(consis_plot_2_2, aes(x=date,
                                      y=n,
                                      color = as.factor(consis_2weeks)))+
    
    geom_line()+
    geom_point()+
    facet_wrap(~community_level, nrow=3)+
    labs(title="Number of consistant and unconsistant county in 2weeks")

ggsave("Result/a2week_consis_number.jpg", a2week3, height=4,width=8,scale=1.65)


consis_plot_2_3 = consis %>%
    mutate(consis_2weeks = replace(consis_2weeks, consis_2weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date) %>%
    count(consis_2weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    filter(consis_2weeks == 1)

a2week4 = ggplot(consis_plot_2_3, aes(x=date,
                                    y=consisRate))+
    
    geom_line()+
    geom_point(alpha = .5)+
    labs(title="total proportion of consistant county in 2weeks")

ggsave("Result/a2week_consis_total.jpg", a2week4, height=4,width=8,scale=1.65)


### 
library("gridExtra")
mix4 = grid.arrange(a2week4, a3week4, a4week4, ncol = 1, nrow = 3)
ggsave("Result/amix4.jpg", mix4, height=8,width=8,scale=1.65)

mix3 = grid.arrange(a2week3, a3week3, a4week3, ncol = 1, nrow = 3)
ggsave("Result/amix3.jpg", mix3, height=8,width=8,scale=1.65)

mix2 = grid.arrange(a2week2, a3week2, a4week2, ncol = 1, nrow = 3)
ggsave("Result/amix2.jpg", mix2, height=8,width=8,scale=1.65)


mix1 = grid.arrange(a2week1, a3week1, a4week1, ncol = 1, nrow = 3)
ggsave("Result/amix1.jpg", mix1, height=8,width=8,scale=1.65)


#########################################################


load("Data/CDC_community_level_county_computed.csv")





# Computed Hospital Admission per 100k in counties with three diferent community level plot
hos_out = boxplot(community_level_county_computed$hospital_admission_per100)$out

a_hos = community_level_county_computed %>%
    mutate(hospital_admission_per100 = replace(hospital_admission_per100,
                                              hospital_admission_per100 %in% hos_out,0)) %>%
    ggplot(aes(hospital_admission_per100, fill=community_level))+
    geom_density(alpha = .2)+
    facet_wrap(~community_level, nrow=3)+
    labs(title="Computed Hospital Admission per 100k in counties")

ggsave("Result/a_hos.jpg", a_hos, height=4,width=6,scale=1.65)

new_case = ggplot((community_level_county_computed), aes(x=community_level,
                                                         y=new_case,
                                                         color=community_level))+
    geom_jitter(position = position_jitter(width = 0.02))+
    geom_boxplot(alpha = 0.7, outlier.shape = NA)+
    coord_flip()+
    geom_rug()+
    labs(title="New cases in counties with three diferent community level")

ggsave("Result/c_newcase.jpg", new_case, height=4,width=8,scale=1.65)


acc_rate = ggplot((community_level_county_computed), aes(x=community_level,
                                                         y=bed_utilization,
                                                         color=community_level))+
    geom_jitter(position = position_jitter(width = 0.02))+
    geom_boxplot(alpha = 0.7, outlier.shape = NA)+
    coord_flip()+
    geom_rug()+
    labs(title="Accupied Rate in counties with three diferent community level")

ggsave("Result/c_accupiedRate.jpg", acc_rate, height=4,width=8,scale=1.65)








