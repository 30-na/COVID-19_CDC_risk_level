library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)
library(tidyr)
library(ggpubr)
library(usmap)
library(maps)
library(tidycensus)
library(gridExtra)

# load datasets
load("Data/hospital_utilization_county.csv")
load("Data/CDC_community_transmission_county_historical.csv")


new_cases = CDC_community_risk_historical %>%
    select(date,
           fips_code,
           new_case)

new_cases[new_cases$new_case < 0, ]$new_case = NA



#When the total new case rate metric ("cases_per_100K_7_day_count_change")
#is greater than zero and less than 10, this metric is set to "suppressed"

merged_newcase = merge(hospital_utilization,
                       new_cases,
                       by=c("date", "fips_code"))

# add county population from community level files
load("Data/CDC_community_level_county.csv")
county_pop = CDC_community_level_county %>%
    dplyr::filter(date_updated == "2022-03-24") %>%
    dplyr::select(county_fips, 
                  population) %>%
    rename(fips_code = county_fips)

merged_data = merge(merged_newcase,
                    county_pop,
                    by="fips_code")

community_level_county = merged_data %>%
    mutate(hospital_admission_per100 = round((hospital_admissions/population)*100000))




# remove NA value
community_level_county = community_level_county %>%
    drop_na(new_case,
            hospital_admission_per100,
            bed_utilization)


community_level_county$community_level = NA


low_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 < 10) | 
    (community_level_county$new_case < 200 &
         community_level_county$bed_utilization < 10)

medium_index = 
    (community_level_county$new_case < 200 &
         (community_level_county$hospital_admission_per100 > 10 &
              community_level_county$hospital_admission_per100 < 20)) |
    (community_level_county$new_case < 200 &
         (community_level_county$bed_utilization > 10 &
              community_level_county$bed_utilization < 25)) |
    (community_level_county$new_case > 200 &
         community_level_county$hospital_admission_per100 < 10) |
    (community_level_county$new_case > 200 &
         community_level_county$bed_utilization < 10)

high_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 >= 20) | 
    (community_level_county$new_case < 200 & 
         community_level_county$bed_utilization >= 15) |
    (community_level_county$new_case >= 200 & 
         community_level_county$hospital_admission_per100 >= 10) | 
    (community_level_county$new_case >= 200 & 
         community_level_county$bed_utilization >= 10) 


community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "High"
community_level_county$community_level[high_index] = "High"



community_level_county_computed = community_level_county %>%
    drop_na(community_level)

save(community_level_county_computed, file="Data/CDC_community_level_county_computed_merged_Medium_With_High.csv")



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

fig_consis_rate_line01_HM = ggplot(consis_plot_3, aes(x=date, y=consisRate,
                                    color=community_level))+
    
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    facet_wrap(~community_level, nrow=3)+
    labs(title="consistancy Rate for each Comunity risk level in 3weeks (High and Medium merged)")

#ggsave("Result/consistancy_rate_each_level.jpg", fig_consis_rate_line01_HM, height=4, width=8, scale=1.65)




fig_consis_rate_line02_HM = ggplot(consis_plot_3, aes(x=date, y=consisRate,
                                    color=community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="Proportion of county with consistance Comunity level risk in 3weeks(High and Medium merged)")

#ggsave("Result/consistancy_rate_each_level02.jpg", fig_consis_rate_line02_HM, height=4, width=8, scale=1.65)





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



fig_consis_rate_total_line_HM = ggplot(consis_plot_3_3, aes(x=date,
                                      y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21))+
    geom_hline(yintercept=mean(consis_plot_3_3$consisRate),
               linetype="dashed")+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="total proportion of consistant county in 3weeks (High and Medium merged)")

#ggsave("Result/consistancy_rate_total.jpg", fig_consis_rate_total_line_HM, height=4,width=8,scale=1.65)

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

fig_county_map = ggplot(data = us_county,
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

#ggsave("Result/available_data_county_map.jpg", fig_county_map, height=4,width=8,scale=1.65)




length(unique(community_level_county_computed$state))
length(unique(community_level_county_computed$fips_code))
length(unique(common_counties_df$state))
length(unique(common_counties_df$fips_code))


fig_consisRate_box_HM = ggplot(consis_plot_3_3, aes(y=consis_3weeks, x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    labs(title="Total proportion of consistant county in 3weeks (High and Medium merged)")

#ggsave("Result/consistancy_rate_box.jpg", fig_consisRate_box_HM, height=2,width=8,scale=1.65)



fig_risk_level_proportion_line_HM = community_level_county_computed %>%
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
    labs(title="proportion of counties in each risk level per week (High and Medium merged)")

#ggsave("Result/proportion_each_risk_level.jpg", fig_risk_level_proportion_line_HM, height=4,width=8,scale=1.65)

fig_facet_proportion_RL_consisRate_HM = grid.arrange(fig_consis_rate_line01_HM,
                                                     fig_risk_level_proportion_line_HM,
                                                     nrow=1)
#ggsave("Result/consisRate_RLProportion_facet.jpg", fig_facet_proportion_RL_consisRate_HM, height=4,width=8,scale=1.65)


###############################LOW AND MEDIUM MERGED############################



new_cases = CDC_community_risk_historical %>%
    select(date,
           fips_code,
           new_case)

new_cases[new_cases$new_case < 0, ]$new_case = NA



#When the total new case rate metric ("cases_per_100K_7_day_count_change")
#is greater than zero and less than 10, this metric is set to "suppressed"

merged_newcase = merge(hospital_utilization,
                       new_cases,
                       by=c("date", "fips_code"))

# add county population from community level files
load("Data/CDC_community_level_county.csv")
county_pop = CDC_community_level_county %>%
    dplyr::filter(date_updated == "2022-03-24") %>%
    dplyr::select(county_fips, 
                  population) %>%
    rename(fips_code = county_fips)

merged_data = merge(merged_newcase,
                    county_pop,
                    by="fips_code")

community_level_county = merged_data %>%
    mutate(hospital_admission_per100 = round((hospital_admissions/population)*100000))




# remove NA value
community_level_county = community_level_county %>%
    drop_na(new_case,
            hospital_admission_per100,
            bed_utilization)


community_level_county$community_level = NA


low_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 < 10) | 
    (community_level_county$new_case < 200 &
         community_level_county$bed_utilization < 10)

medium_index = 
    (community_level_county$new_case < 200 &
         (community_level_county$hospital_admission_per100 > 10 &
              community_level_county$hospital_admission_per100 < 20)) |
    (community_level_county$new_case < 200 &
         (community_level_county$bed_utilization > 10 &
              community_level_county$bed_utilization < 25)) |
    (community_level_county$new_case > 200 &
         community_level_county$hospital_admission_per100 < 10) |
    (community_level_county$new_case > 200 &
         community_level_county$bed_utilization < 10)

high_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 >= 20) | 
    (community_level_county$new_case < 200 & 
         community_level_county$bed_utilization >= 15) |
    (community_level_county$new_case >= 200 & 
         community_level_county$hospital_admission_per100 >= 10) | 
    (community_level_county$new_case >= 200 & 
         community_level_county$bed_utilization >= 10) 


community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "Low"
community_level_county$community_level[high_index] = "High"



community_level_county_computed = community_level_county %>%
    drop_na(community_level)

save(community_level_county_computed, file="Data/CDC_community_level_county_computed_merged_Medium_With_Low.csv")


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

fig_consis_rate_line01_LM = ggplot(consis_plot_3, aes(x=date, y=consisRate,
                                                      color=community_level))+
    
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    facet_wrap(~community_level, nrow=3)+
    labs(title="consistancy Rate for each Comunity risk level in 3weeks (Low and Medium merged)")

#ggsave("Result/consistancy_rate_each_level.jpg", fig_consis_rate_line01_LM, height=4, width=8, scale=1.65)




fig_consis_rate_line02_LM = ggplot(consis_plot_3, aes(x=date, y=consisRate,
                                                      color=community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="Proportion of county with consistance Comunity level risk in 3weeks(Low and Medium merged)")

#ggsave("Result/consistancy_rate_each_level02.jpg", fig_consis_rate_line02_LM, height=4, width=8, scale=1.65)





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



fig_consis_rate_total_line_LM = ggplot(consis_plot_3_3, aes(x=date,
                                                            y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21))+
    geom_point(alpha = .5)+
    geom_hline(yintercept = mean(consis_plot_3_3$consisRate),
               linetype = "dashed")+
    theme_bw()+
    labs(title="total proportion of consistant county in 3weeks (Low and Medium merged)")

#ggsave("Result/consistancy_rate_total.jpg", fig_consis_rate_total_line_LM, height=4,width=8,scale=1.65)


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

fig_county_map = ggplot(data = us_county,
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

#ggsave("Result/available_data_county_map.jpg", fig_county_map, height=4,width=8,scale=1.65)




length(unique(community_level_county_computed$state))
length(unique(community_level_county_computed$fips_code))
length(unique(common_counties_df$state))
length(unique(common_counties_df$fips_code))


fig_consisRate_box_LM = ggplot(consis_plot_3_3, aes(y=consis_3weeks, x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    labs(title="Total proportion of consistant county in 3weeks (Low and Medium merged)")

#ggsave("Result/consistancy_rate_box.jpg", fig_consisRate_box_LM, height=2,width=8,scale=1.65)



fig_risk_level_proportion_line_LM = community_level_county_computed %>%
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
    labs(title="proportion of counties in each risk level per week (Low and Medium merged)")

#ggsave("Result/proportion_each_risk_level.jpg", fig_risk_level_proportion_line_LM, height=4,width=8,scale=1.65)



fig_facet_proportion_RL_consisRate_LM = grid.arrange(fig_consis_rate_line01_LM,
                                                     fig_risk_level_proportion_line_LM,
                                                     nrow=1)
#ggsave("Result/consisRate_RLProportion_facet.jpg", fig_facet_proportion_RL_consisRate_LM, height=4,width=8,scale=1.65)




#########################LOW MEDIUM HIGH #################################

new_cases = CDC_community_risk_historical %>%
    select(date,
           fips_code,
           new_case)

new_cases[new_cases$new_case < 0, ]$new_case = NA



#When the total new case rate metric ("cases_per_100K_7_day_count_change")
#is greater than zero and less than 10, this metric is set to "suppressed"

merged_newcase = merge(hospital_utilization,
                       new_cases,
                       by=c("date", "fips_code"))

# add county population from community level files
load("Data/CDC_community_level_county.csv")
county_pop = CDC_community_level_county %>%
    dplyr::filter(date_updated == "2022-03-24") %>%
    dplyr::select(county_fips, 
                  population) %>%
    rename(fips_code = county_fips)

merged_data = merge(merged_newcase,
                    county_pop,
                    by="fips_code")

community_level_county = merged_data %>%
    mutate(hospital_admission_per100 = round((hospital_admissions/population)*100000))




# remove NA value
community_level_county = community_level_county %>%
    drop_na(new_case,
            hospital_admission_per100,
            bed_utilization)


community_level_county$community_level = NA


low_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 < 10) | 
    (community_level_county$new_case < 200 &
         community_level_county$bed_utilization < 10)

medium_index = 
    (community_level_county$new_case < 200 &
         (community_level_county$hospital_admission_per100 > 10 &
              community_level_county$hospital_admission_per100 < 20)) |
    (community_level_county$new_case < 200 &
         (community_level_county$bed_utilization > 10 &
              community_level_county$bed_utilization < 25)) |
    (community_level_county$new_case > 200 &
         community_level_county$hospital_admission_per100 < 10) |
    (community_level_county$new_case > 200 &
         community_level_county$bed_utilization < 10)

high_index = 
    (community_level_county$new_case < 200 & 
         community_level_county$hospital_admission_per100 >= 20) | 
    (community_level_county$new_case < 200 & 
         community_level_county$bed_utilization >= 15) |
    (community_level_county$new_case >= 200 & 
         community_level_county$hospital_admission_per100 >= 10) | 
    (community_level_county$new_case >= 200 & 
         community_level_county$bed_utilization >= 10) 


community_level_county$community_level[low_index] = "Low"
community_level_county$community_level[medium_index] = "Medium"
community_level_county$community_level[high_index] = "High"



community_level_county_computed = community_level_county %>%
    drop_na(community_level)

save(community_level_county_computed, file="Data/CDC_community_level_county_computed.csv")




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

fig_consis_rate_line01 = ggplot(consis_plot_3, aes(x=date, y=consisRate,
                                                  color=community_level))+
    
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    facet_wrap(~community_level, nrow=3)+
    labs(title="consistancy Rate for each Comunity risk level in 3weeks")

ggsave("Result/consistancy_rate_each_level.jpg", fig_consis_rate_line01, height=4, width=8, scale=1.65)




fig_consis_rate_line02 = ggplot(consis_plot_3, aes(x=date, y=consisRate,
                                                   color=community_level))+
    
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="Proportion of county with consistance Comunity level risk in 3weeks")

ggsave("Result/consistancy_rate_each_level02.jpg", fig_consis_rate_line02, height=4, width=8, scale=1.65)





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


fig_consis_rate_total_line = ggplot(consis_plot_3_3, aes(x=date,
                                                         y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21))+
    geom_hline(yintercept = mean(consis_plot_3_3$consisRate),
               linetype = "dashed")+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="total proportion of consistant county in 3weeks")

ggsave("Result/consistancy_rate_total.jpg", fig_consis_rate_total_line, height=4,width=8,scale=1.65)



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

county_map = ggplot(data = us_county,
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


ggsave("Result/available_data_county_map.jpg", county_map, height=4,width=8,scale=1.65)




length(unique(community_level_county_computed$state))
length(unique(community_level_county_computed$fips_code))
length(unique(common_counties_df$state))
length(unique(common_counties_df$fips_code))


fig_consisRate_box = ggplot(consis_plot_3_3, aes(y=consis_3weeks, x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    labs(title="Total proportion of consistant county in 3weeks")

ggsave("Result/consistancy_rate_box.jpg", fig_consisRate_box, height=2,width=8,scale=1.65)



fig_risk_level_proportion_line = community_level_county_computed %>%
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

ggsave("Result/proportion_each_risk_level.jpg", fig_risk_level_proportion_line, height=4,width=8,scale=1.65)



fig_facet_proportion_RL_consisRate = grid.arrange(fig_consis_rate_line01,
                                                  fig_risk_level_proportion_line,
                                                  nrow=1)

ggsave("Result/consisRate_RLProportion_facet.jpg",
       fig_facet_proportion_RL_consisRate, height=4,width=8,scale=1.65)
############################## Grid Arrange##########################3


fig_compare_consisRate_total_line = grid.arrange(fig_consis_rate_total_line,
                                          fig_consis_rate_total_line_LM,
                                          fig_consis_rate_total_line_HM,
                                          nrow = 3)

ggsave("Result/compare_consisRate_total.jpg",
       fig_compare_consisRate_total_line, 
       height=4,width=8,scale=1.65)


fig_compare_consisRate_line = grid.arrange(fig_consis_rate_line01,
                                           fig_consis_rate_line01_LM,
                                           fig_consis_rate_line01_HM,
                                                 nrow = 3)

ggsave("Result/compare_consisRate.jpg",
       fig_compare_consisRate_line, 
       height=8,width=8,scale=1.65)




fig_compare_consisRate_box = grid.arrange(fig_consisRate_box ,
                                          fig_consisRate_box_LM,
                                          fig_consisRate_box_HM,
                                           nrow = 3)

ggsave("Result/compare_consisRate_box.jpg",
       fig_compare_consisRate_box, 
       height=4,width=8,scale=1.65)

