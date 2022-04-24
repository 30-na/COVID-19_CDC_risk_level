library(dplyr)
library(usdata)
library(ggplot2)
library(scales)
library(tidyr)
library(ggpubr)
library(usmap)
library(maps)
library(tidycensus)
library(gridExtra)


#######################PART I (HIGH amd MEDIUM MERGED)###################
load("Data/CDC_community_level_county_computed_merged_Medium_With_High.csv")

# days list
days = unique(community_level_county_computed$date)

# the counties that have consistent data for all weeks in the time interval
common_counties_df = community_level_county_computed %>%
    group_by(state, fips_code)%>%
    count(fips_code)%>%
    filter(n == length(days))%>%
    select(state, fips_code) %>%
    mutate(state = tolower(abbr2state(state)))

# list of counties which are common in all days
# common_counties = names(table(community_level_county_computed$fips_code)[table(community_level_county_computed$fips_code) == length(days)])


#filter the counties that have consistent data in community_level_county_computed dataset
consis = community_level_county_computed %>% 
    dplyr::filter(fips_code %in% common_counties_df$fips_code) %>%
    select(date,
           fips_code,
           community_level) %>%
    group_by(fips_code) %>%
    arrange(fips_code,
            date)

# number of unique community level in three weeks interval
consis_3weeks = c()
for(i in 1:nrow(consis)){
    consis_3weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i+1],
                                       consis$community_level[i+2])))
}

consis$consis_3weeks = consis_3weeks


# plot the graphs

# plot the map of counties with consistent available data 
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


# plot consistancy Rate for each Comunity risk level
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

# plot consistancy Rate for each Comunity risk level
fig_consis_rate_line02_HM = ggplot(data = consis_plot_3,
                                   aes(x = date,
                                       y = consisRate,
                                       color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .3)+
    scale_color_manual(values = c("#e34a33", "#2b8cbe", "#fdbb84"))+
    labs(title="Consistancy Rate for each Comunity Risk in 3weeks (High and Medium merged)")

# ggsave("Result/consistancy_rate_each_level02.jpg",
#        fig_consis_rate_line02,
#        height=3, width=8, scale=1.65)

# 
# 
# consis_plot_3_2 = consis %>%
#     filter(date > "2020-08-07") %>%
#     mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
#     arrange(date) %>%
#     group_by(date, community_level) %>%
#     count(consis_3weeks) %>%
#     mutate(total_community_level = sum(n)) %>%
#     mutate(consisRate = n/total_community_level)%>%
#     arrange(date, community_level)
# 


# plot the total consistency Rate line
consis_plot_3_3 = consis %>%
    filter(date <= "2022-03-04") %>%
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



## plot the total consistency Rate (Box plot)
fig_consisRate_box_HM = ggplot(consis_plot_3_3, aes(y=consis_3weeks, x=consisRate))+
    
    geom_jitter( alpha=.3, height=.05)+
    geom_boxplot(fill="steelblue", alpha=.3)+
    xlim(0, 1)+
    geom_rug()+
    theme_bw()+
    labs(title="Total proportion of consistant county in 3weeks (High and Medium merged)")

#ggsave("Result/consistancy_rate_box.jpg", fig_consisRate_box_HM, height=2,width=8,scale=1.65)


# the Proportion of each risk level per week
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
    scale_color_manual(values = c("#e34a33", "#2b8cbe", "#fdbb84"))+
    labs(title="proportion of counties in each risk level per week (High and Medium merged)")

#ggsave("Result/proportion_each_risk_level.jpg", fig_risk_level_proportion_line_HM, height=4,width=8,scale=1.65)


fig_facet_proportion_RL_consisRate_HM = grid.arrange(fig_consis_rate_line01_HM,
                                                     fig_risk_level_proportion_line_HM,
                                                     nrow=1)
#ggsave("Result/consisRate_RLProportion_facet.jpg", fig_facet_proportion_RL_consisRate_HM, height=4,width=8,scale=1.65)


###############################LOW AND MEDIUM MERGED############################
load("Data/CDC_community_level_county_computed_merged_Medium_With_Low.csv")

# days list
days = unique(community_level_county_computed$date)

# the counties that have consistent data for all weeks in the time interval
common_counties_df = community_level_county_computed %>%
    group_by(state, fips_code)%>%
    count(fips_code)%>%
    filter(n == length(days))%>%
    select(state, fips_code) %>%
    mutate(state = tolower(abbr2state(state)))

# list of counties which are common in all days
# common_counties = names(table(community_level_county_computed$fips_code)[table(community_level_county_computed$fips_code) == length(days)])


#filter the counties that have consistent data in community_level_county_computed dataset
consis = community_level_county_computed %>% 
    dplyr::filter(fips_code %in% common_counties_df$fips_code) %>%
    select(date,
           fips_code,
           community_level) %>%
    group_by(fips_code) %>%
    arrange(fips_code,
            date)

# number of unique community level in four weeks interval
consis_3weeks = c()
for(i in 1:nrow(consis)){
    consis_3weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i+1],
                                       consis$community_level[i+2])))
}

consis$consis_3weeks = consis_3weeks


# plot the graphs
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



fig_consis_rate_line02_LM = ggplot(data = consis_plot_3,
                                aes(x = date,
                                    y = consisRate,
                                    color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .3)+
    scale_color_manual(values = c("#e34a33", "#2b8cbe", "#fdbb84"))+
    labs(title="Consistancy Rate for each Comunity Risk in 3weeks (Low and Medium merged)")

# ggsave("Result/consistancy_rate_each_level02.jpg",
#        fig_consis_rate_line02,
#        height=3, width=8, scale=1.65)


consis_plot_3_3 = consis %>%
    filter(date <= "2022-03-04") %>%
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
    scale_color_manual(values = c("#e34a33", "#2b8cbe", "#fdbb84"))+
    labs(title="proportion of counties in each risk level per week (Low and Medium merged)")

#ggsave("Result/proportion_each_risk_level.jpg", fig_risk_level_proportion_line_LM, height=4,width=8,scale=1.65)



fig_facet_proportion_RL_consisRate_LM = grid.arrange(fig_consis_rate_line01_LM,
                                                     fig_risk_level_proportion_line_LM,
                                                     nrow=1)
#ggsave("Result/consisRate_RLProportion_facet.jpg", fig_facet_proportion_RL_consisRate_LM, height=4,width=8,scale=1.65)



#########################LOW MEDIUM HIGH #################################
load("Data/CDC_community_level_county_computed.csv")

# days list
days = unique(community_level_county_computed$date)

# the counties that have consistent data for all weeks in the time interval
common_counties_df = community_level_county_computed %>%
    group_by(state, fips_code)%>%
    count(fips_code)%>%
    filter(n == length(days))%>%
    select(state, fips_code) %>%
    mutate(state = tolower(abbr2state(state)))

# list of counties which are common in all days
# common_counties = names(table(community_level_county_computed$fips_code)[table(community_level_county_computed$fips_code) == length(days)])


#filter the counties that have consistent data in community_level_county_computed dataset
consis = community_level_county_computed %>% 
    dplyr::filter(fips_code %in% common_counties_df$fips_code) %>%
    select(date,
           fips_code,
           community_level) %>%
    group_by(fips_code) %>%
    arrange(fips_code,
            date)

# number of unique community level in four weeks interval
consis_3weeks = c()
for(i in 1:nrow(consis)){
    consis_3weeks[i] = length(unique(c(consis$community_level[i],
                                       consis$community_level[i+1],
                                       consis$community_level[i+2])))
}
consis$consis_3weeks = consis_3weeks


# plot the graphs
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

# ggsave("Result/consistancy_rate_each_level.jpg", fig_consis_rate_line01, height=4, width=8, scale=1.65)




fig_consis_rate_line02 = ggplot(data = consis_plot_3,
                                aes(x = date,
                                    y = consisRate,
                                    color = community_level))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 15))+
    geom_point(alpha = .3)+
    scale_color_manual(values = c("#e34a33", "#2b8cbe", "#fdbb84"))+
    labs(title="Consistancy Rate for each Comunity Risk Level in 3weeks")

# ggsave("Result/consistancy_rate_each_level02.jpg",
#        fig_consis_rate_line02,
#        height=3, width=8, scale=1.65)


consis_plot_3_3 = consis %>%
    filter(date <= "2022-03-04") %>%
    mutate(consis_3weeks = replace(consis_3weeks, consis_3weeks != 1, 0)) %>%
    arrange(date) %>%
    group_by(date) %>%
    count(consis_3weeks) %>%
    mutate(total_community_level = sum(n)) %>%
    mutate(consisRate = n/total_community_level)%>%
    filter(consis_3weeks == 1)



mean_total = mean(consis_plot_3_3$consisRate)
median_total = median(consis_plot_3_3$consisRate)
mean_low = mean(filter(consis_plot_3,
                       community_level == "Low")$consisRate)
mean_med = mean(filter(consis_plot_3,
                       community_level == "Medium")$consisRate)
mean_high = mean(filter(consis_plot_3,
                        community_level == "High")$consisRate)


result_table_original = data.frame("Total Mean" = mean_total,
                          "Low Risk Mean" = mean_low,
                          "Medium Risk Mean" = mean_med,
                          "High Risk Mean" = mean_high)


fig_consis_rate_total_line = ggplot(consis_plot_3_3, aes(x=date,
                                                         y=consisRate))+
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 21))+
    geom_hline(yintercept = mean(consis_plot_3_3$consisRate),
               linetype = "dashed")+
    geom_point(alpha = .5)+
    theme_bw()+
    labs(title="total proportion of consistant county in 3weeks")

# ggsave("Result/consistancy_rate_total.jpg", fig_consis_rate_total_line, height=4,width=8,scale=1.65)



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

# ggsave("Result/consistancy_rate_box.jpg", fig_consisRate_box, height=2,width=8,scale=1.65)



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
    scale_color_manual(values = c("#e34a33", "#2b8cbe", "#fdbb84"))+
    labs(title="proportion of counties in each risk level per week")

# ggsave("Result/proportion_each_risk_level.jpg", fig_risk_level_proportion_line, height=4,width=8,scale=1.65)


fig_risk_level_proportion02_line = community_level_county_computed %>%
    group_by(date) %>%
    count(community_level) %>%
    mutate(total = sum(n)) %>%
    mutate(counties_proportion = round(n/total, 2)) %>%
    
    ggplot(aes(x = date,
               y = counties_proportion,
               color = community_level)) +
    geom_point(alpha=.3)+
    geom_smooth(method = "lm",
                formula = y~ poly(x, 15))+
    scale_color_manual(values = c("#e34a33", "#2b8cbe", "#fdbb84"))+
    labs(title="Proportion of Counties in Three Risk Level each week")

# ggsave("Result/proportion_each_risk_level02.jpg",
#        fig_risk_level_proportion02_line, height=3,width=8,scale=1.65)

############################## Grid Arrange###########################

fig_facet_proportion_RL_consisRate = grid.arrange(fig_consis_rate_line02,
                                                  fig_risk_level_proportion02_line,
                                                  nrow=2)
ggsave("Result/consisRate_RLProportion_facet.jpg",
       fig_facet_proportion_RL_consisRate,
       height=4,width=8,scale=1.65)


fig_compare_consisRate_total_line = grid.arrange(fig_consis_rate_total_line,
                                          fig_consis_rate_total_line_HM,
                                          fig_consis_rate_total_line_LM,
                                          nrow = 3)
ggsave("Result/compare_consisRate_total.jpg",
       fig_compare_consisRate_total_line, 
       height=4,width=8,scale=1.65)


fig_compare_consisRate_line = grid.arrange(fig_consis_rate_line02,
                                           fig_consis_rate_line02_LM,
                                           fig_consis_rate_line02_HM,
                                                 nrow = 3)
ggsave("Result/compare_consisRate.jpg",
       fig_compare_consisRate_line, 
       height=5,width=8,scale=1.65)


fig_compare_consisRate_box = grid.arrange(fig_consisRate_box ,
                                          fig_consisRate_box_HM,
                                          fig_consisRate_box_LM,
                                           nrow = 3)
ggsave("Result/compare_consisRate_box.jpg",
       fig_compare_consisRate_box, 
       height=4,width=8,scale=1.65)

