library(dplyr)
library(tidyr)
library(data.table)
library(maps)
library(usdata)
library(gganimate)
library(gifski)
library(gridExtra)

# load CDC vaccine county dataset
vaccine_file = fread("Data/COVID-19_Vaccinations_in_the_United_States_County.csv")

# clean Dataset
vaccine_df = vaccine_file %>%
    select(Date,
           Recip_State,
           FIPS,
           Recip_County,
           Series_Complete_Pop_Pct) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
    mutate(Recip_State = tolower(abbr2state(Recip_State)))%>%
    filter(Date > "2021-01-01")%>%
    arrange(Date, Recip_State, Recip_County)


# Load Test positive
community_transmission = fread("Data/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes.csv")

# clean Dataset
positive_test_df = community_transmission %>%
    select(date,
           state_name,
           fips_code,
           county_name,
           percent_test_results_reported_positive_last_7_days) %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
    filter(date > "2021-01-01") %>%
    rename("positive_rate" = percent_test_results_reported_positive_last_7_days) %>%
    rename("Recip_County" = county_name) %>%
    rename("Recip_State" = state_name) %>%
    rename("FIPS" = fips_code) %>%
    rename("Date" = date) %>%
    arrange(Date, Recip_County) %>%
    mutate(FIPS = as.character(FIPS))%>%
    mutate(nchar = nchar(FIPS))%>%
    mutate(FIPS = ifelse(nchar == 4,
                           paste(0, FIPS, sep = ""),
                           FIPS))%>%
    select(-nchar)%>%
    mutate(Recip_State = tolower(Recip_State))%>%
    arrange(Date, Recip_State, Recip_County)
    

# Merge Two Datasets
merge_vacc_posit = inner_join(positive_test_df, 
                        vaccine_df,
                        by = c("Date","Recip_State", "FIPS", "Recip_County"))

##########################################################################
# correlation for full time interval
cor_vacc_posit = merge_vacc_posit %>%
    drop_na(any_of(c('positive_rate',
                     "Series_Complete_Pop_Pct"))) %>%
    group_by(Recip_State, FIPS, Recip_County) %>%
    summarize(corr = cor(Series_Complete_Pop_Pct,
                        positive_rate))
 
# USA map with vaccine rate category
us_county = map_data("county")
us_state = map_data("state")

county_corr = cor_vacc_posit %>%
    ungroup() %>%
    rename("state" = Recip_State)%>%
    mutate(Recip_County = tolower(gsub(" County", "", Recip_County)))%>%
    mutate(Recip_County = tolower(gsub(" parish", "", Recip_County)))%>%
    rename("county" = Recip_County) %>%
    mutate(state_county = paste(state, county, sep="_")) %>%
    select(state_county, corr)

us_county = us_county %>%
    mutate(state_county = paste(region, subregion, sep="_"))

county_corr_map = left_join(us_county, county_corr,
                               by = "state_county")

fig_corr_map_full = ggplot(data = county_corr_map,
                         mapping = aes(x = long,
                                       y = lat, 
                                       group = group,
                                       fill = corr))+
    geom_polygon(color = "#636363",
                 size = 0.05) +
    geom_polygon(data = us_state,
                 mapping = aes(long,
                               lat,
                               group = group),
                 fill = NA, 
                 color = "black",
                 size = .3) +
    coord_equal()+
    labs(title = "",
         subtitle = "Map of the counties with different correlation between vaccination and positive test (Full time iterval).")+
    scale_fill_gradientn(colours = c("#d6604d", "#f7f7f7" , "#4393c3"),
                         limits=c(-1,1)) +
    theme_void()

ggsave("Result/corr_county_full.jpg", fig_corr_map_full, height=4,width=8,scale=1.65)


#Texas Vaccination rate

  
 
##########################################################################
# for loop for different interval
time_interval = c("2021-01-01",
                  "2021-07-01",
                  "2021-09-01",
                  "2022-01-01",
                  "2022-02-01",
                  "2022-03-30")
for( i in 1:5){
cor_vacc_posit = merge_vacc_posit %>%
    filter(Date > time_interval[i] & Date < time_interval[i+1]) %>%
    drop_na(any_of(c('positive_rate',
                     "Series_Complete_Pop_Pct"))) %>%
    group_by(Recip_State, FIPS, Recip_County) %>%
    summarize(corr = cor(Series_Complete_Pop_Pct,
                         positive_rate))

# USA map with vaccine rate category
us_county = map_data("county")
us_state = map_data("state")

county_corr = cor_vacc_posit %>%
    ungroup() %>%
    rename("state" = Recip_State)%>%
    mutate(Recip_County = tolower(gsub(" County", "", Recip_County)))%>%
    mutate(Recip_County = tolower(gsub(" parish", "", Recip_County)))%>%
    rename("county" = Recip_County) %>%
    mutate(state_county = paste(state, county, sep="_")) %>%
    select(state_county, corr)

us_county = us_county %>%
    mutate(state_county = paste(region, subregion, sep="_"))

county_corr_map = left_join(us_county, county_corr,
                            by = "state_county")

fig_corr_map_full = ggplot(data = county_corr_map,
                           mapping = aes(x = long,
                                         y = lat, 
                                         group = group,
                                         fill = corr))+
    geom_polygon(color = "#636363",
                 size = 0.05) +
    geom_polygon(data = us_state,
                 mapping = aes(long,
                               lat,
                               group = group),
                 fill = NA, 
                 color = "black",
                 size = .3) +
    coord_equal()+
    labs(title = "",
         subtitle = paste("Map of the counties with different correlation between vaccination and positive test (",
                          time_interval[i],
                          "to",
                          time_interval[i+1],
                          "time iterval)."))+
    scale_fill_gradientn(colours = c("#d6604d", "#f7f7f7" , "#4393c3"),
                         limits=c(-1,1)) +
    theme_void()

ggsave(paste("Result/corr_county", 
             i, ".jpg"),
       fig_corr_map_full, height=4,width=8,scale=1.65)

}











