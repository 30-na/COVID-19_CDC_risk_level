library(dplyr)
library(tidyr)
library(data.table)
library(maps)
library(usdata)
library(gganimate)
library(gifski)

# load CDC vaccine county dataset
vaccine_file = fread("Data/COVID-19_Vaccinations_in_the_United_States_County.csv")


# clean Dataset
vaccine_df = vaccine_file %>%
    select(Date,
           FIPS,
           Recip_County,
           Recip_State,
           Series_Complete_Pop_Pct) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
    mutate(vaccine_category = cut(Series_Complete_Pop_Pct,
                                  breaks = c(-Inf, 30, 40, 50, 60, 70, Inf),
                                  labels = c("0%-29.9%", "30%-39.9%",
                                             "40%-49.9%", "50%-59.9%",
                                             "60%-69.9%","70%-100%"))) %>%
    filter(Date > "2021-01-01")

# Load Test positive
community_transmission = fread("Data/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes.csv")
positive_rate = community_transmission %>%
    select(county_name,
           fips_code,
           date,
           percent_test_results_reported_positive_last_7_days) %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
    rename("positive_rate" = percent_test_results_reported_positive_last_7_days) %>%
    rename("Recip_County" = county_name) %>%
    rename("FIPS" = fips_code) %>%
    rename("Date" = date) %>%
    arrange(Date, Recip_County) %>%
    filter(Date > "2021-01-01") %>%
    mutate(FIPS = as.character(FIPS))
    
merge_data = inner_join(positive_rate, 
                        vaccine_df,
                        by = c("Date","FIPS", "Recip_County"))
   
str(positive_rate)


# time series for each vaccine category rate
vaccine_time = vaccine_df %>%
    group_by(Date) %>%
    arrange(Date, vaccine_category) %>%
    count(vaccine_category, .drop=FALSE) %>%
    rename("count_county" = "n")%>%
    filter(!is.na(vaccine_category))%>%
    mutate(sum_county = sum(count_county)) %>%
    mutate(category_rate = round(count_county / sum_county, 2))



fig_vaccine_category_rate = ggplot(data = vaccine_time,
                                   aes(x = Date,
                                       y = category_rate,
                                       color = vaccine_category)) +
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 27),
                se = FALSE)+
    geom_point(size = .5, alpha=.1)+
    labs(title = "Vaccine Category Rate")+
    scale_color_manual(values=c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850'))


ggsave("Result/vaccine_category_rate.jpg", fig_vaccine_category_rate, height=4,width=8,scale=1.65)




# USA map with vaccine rate category
us_county = map_data("county")
us_state = map_data("state")

county_vaccine = vaccine_df %>%
    filter(Date == "2022-04-30") %>%
    mutate(state = tolower(abbr2state(Recip_State))) %>%
    mutate(county = tolower(gsub(" County", "", Recip_County)))%>%
    mutate(state_county = paste(state, county, sep="_")) %>%
    select(state_county,
           vaccine_category)

us_county = us_county %>%
    mutate(state_county = paste(region, subregion, sep="_"))

county_vaccine_map = left_join(us_county, county_vaccine,
                               by = "state_county")

fig_vaccine_map = ggplot(data = county_vaccine_map,
                         mapping = aes(x = long,
                                       y = lat, 
                                       group = group,
                                       fill = vaccine_category))+
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
    labs(title = "US Counties",
         subtitle = "Map of the counties with different vaccine category in 2022-04-30.")+
    scale_fill_manual(values=c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494','#cccccc'))

ggsave("Result/vaccine_county_2022-04-30.jpg", fig_vaccine_map, height=4,width=8,scale=1.65)


# 
# library(gapminder)
# head(gapminder)
# 
# 
# myPlot <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
#     geom_point(alpha = 0.7, show.legend = FALSE) +
#     scale_colour_manual(values = country_colors) +
#     scale_size(range = c(2, 12)) +
#     scale_x_log10() +
#     # Here comes the gganimate specific bits
#     labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
#     transition_time(year) +
#     ease_aes('linear')
# 
# animate(myPlot, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
# anim_save("output.gif", path="Result/")
# 
# Result/vaccine_category_county.jpg
# 
# ?anim_save


