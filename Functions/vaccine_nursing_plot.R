
library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(ggthemes)
library(tidycensus)
library(viridis)


census_api_key("7e83aa1d195fd7cd921e4ac747998c618f05460d")

load("Data/nurse_categoryRate.csv")

###### Vaccine VS Death Rate ####

#Get data from the US Census Bureau Population Estimates
popAcs = get_acs(geography = "county",
                 variable = "B04004_001")
county_population =  popAcs %>%
    mutate(state = tolower(gsub(".*, ", "", NAME))) %>%
    mutate(county = tolower(gsub(" County,.*", "", NAME))) %>%
    rename("population" = estimate) %>%
    select(state,
           county,
           population)

# list of county with continuse data in time period
continous_available_county = nurse_categoryRate %>%
    group_by(state_county) %>%
    count(state_county) %>%
    ungroup() %>%
    filter(n == max(n)) %>%
    dplyr::select(state_county)

###### VACCINE VS DEATH #####
nurse_vaccine_death = nurse_categoryRate %>%
    mutate(state = gsub(",.*", "", state_county)) %>%
    mutate(county = gsub(".*,", "", state_county)) %>%
    left_join(county_population, by = c("state",
                                        "county")) %>%
    drop_na(county_vaccineRate,
            county_deathRate) %>%
    dplyr::filter(state_county %in% continous_available_county$state_county) %>%
    mutate(county_deathRate = ifelse(county_deathRate > 1, NA, county_deathRate))

# plot geom point
vaccine_death_point =  ggplot(data = nurse_vaccine_death,
                             aes(x = county_deathRate,
                                 y = county_vaccineRate,
                                 size = population,
                                 color = state)) +
    geom_point(show.legend = FALSE,
               alpha = 0.7)+
    
    scale_colour_viridis_d() +
    theme_classic() + 
    transition_time(date) +
    labs(title = "Date: {frame_time}") +
    view_follow(fixed_y = TRUE,
                fixed_x = TRUE)



anim_vaccine_death = animate(vaccine_death_point,
                  #fps = 4,
                  start_pause = 1,
                  end_pause = 5,
                  duration = 20,
                  detail = 1,
                  rewind = FALSE,
                  width = 1800,
                  height = 1800,
                  res = 300,
                  renderer = gifski_renderer())

anim_save(filename = "Result/anim_vaccine_death_less1.gif",
          animation = anim_vaccine_death)

# ggsave("Result/vaccine_death_point.jpg",
#        vaccine_death_point,
#        height=4,width=8,scale=1.65)

###### VACCINE VS POSITIVE TEST #####
nurse_vaccine_positive = nurse_categoryRate %>%
    mutate(state = gsub(",.*", "", state_county)) %>%
    mutate(county = gsub(".*,", "", state_county)) %>%
    left_join(county_population, by = c("state",
                                        "county")) %>%
    dplyr::filter(state_county %in% continous_available_county$state_county) %>%
    mutate(county_positiveRate = ifelse(county_positiveRate > 25, NA, county_positiveRate)) %>%
    group_by(date) %>%
    drop_na(county_vaccineRate,
            county_positiveRate) %>%
    mutate(mean_vaccin = mean(county_vaccineRate))



# plot geom point
vaccine_positive_point =  ggplot(data = nurse_vaccine_positive,
                              aes(x = county_positiveRate,
                                  y = county_vaccineRate,
                                  size = population,
                                  color = state)) +
    geom_point(show.legend = FALSE,
               alpha = 0.7) +
    scale_colour_viridis_d() +
    theme_classic() + 
    transition_time(date) +
    labs(title = "Date: {frame_time}") +
    view_follow(fixed_y = TRUE,
                fixed_x = TRUE)



anim_vaccine_positive = animate(vaccine_positive_point,
                             #fps = 4,
                             start_pause = 1,
                             end_pause = 5,
                             duration = 20,
                             detail = 1,
                             rewind = FALSE,
                             width = 1800,
                             height = 1800,
                             res = 300,
                             renderer = gifski_renderer())

anim_save(filename = "Result/anim_vaccine_positive_less25.gif",
          animation = anim_vaccine_positive)

# ggsave("Result/vaccine_positive_point.jpg",
#        vaccine_positive_point,
#        height=4,width=8,scale=1.65)

###### Death Rate VS POSITIVE TEST #####
nurse_death_positive = nurse_categoryRate %>%
    mutate(state = gsub(",.*", "", state_county)) %>%
    mutate(county = gsub(".*,", "", state_county)) %>%
    left_join(county_population, by = c("state",
                                        "county")) %>%
    mutate(county_positiveRate = ifelse(county_positiveRate > 25, NA, county_positiveRate)) %>%
    mutate(county_deathRate = ifelse(county_deathRate > 3, NA, county_deathRate)) %>%
    drop_na(county_positiveRate, 
            county_deathRate) 
    
    # mutate(county_positiveRate = ifelse(county_positiveRate > 25, NA, county_positiveRate)) %>%



# plot geom point
death_positive_point =  ggplot(data = nurse_death_positive,
                                 aes(x = county_positiveRate,
                                     y = county_deathRate,
                                     size = population,
                                     color = state)) +
    geom_point(show.legend = FALSE,
               alpha = 0.7) +
    scale_colour_viridis_d() +
    theme_classic() + 
    transition_time(date) +
    labs(title = "Date: {frame_time}") +
    view_follow(fixed_y = TRUE,
                fixed_x = TRUE)



anim_death_positive = animate(death_positive_point,
                                #fps = 4,
                                start_pause = 1,
                                end_pause = 5,
                                duration = 20,
                                detail = 1,
                                rewind = FALSE,
                                width = 1800,
                                height = 1800,
                                res = 300,
                                renderer = gifski_renderer())

anim_save(filename = "Result/anim_death_positive.gif",
          animation = anim_death_positive)



library(data.table)
library(sf)
library(usdata)
state_map = get_acs(geography = "state",
                    variable = "B04004_001",
                    geometry = TRUE)

vaccine_state_file = fread("Data/COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv")


vaccine_state = vaccine_state_file %>%
    select(Date,
           Location,
           Series_Complete_Pop_Pct) %>%
    rename("date" = Date,
           "NAME" = Location,
           "vaccine_pct" = Series_Complete_Pop_Pct) %>%
    mutate(date = as.Date(date, format = ))
    mutate(NAME = abbr2state(NAME)) %>%
    na.omit() %>%
    left_join(state_map, by = "NAME") 


    vaccine_state_map = ggplot(data = vaccine_state) + 
    geom_sf(aes(fill = vaccine_pct)) + 
    # Code below is just minor visual editing, not necessary for the map to run!
    ggthemes::theme_map() + 
    theme(legend.position = "right") + 
    scale_fill_gradient2(name = "Vaccination rate", 
                         low = "white", 
                         high = "orange",
                         labels = scales::percent)+
    transition_time(date) +
    #view_follow(fixed_x=T, fixed_y=T) +
    labs(title = 'Date: {frame_time}',
         x = NULL,
         y = NULL) 
#ease_aes('linear')



anim_p1 = animate(vaccine_state,
                  #fps = 4,
                  start_pause = 1,
                  end_pause = 5,
                  duration = 10,
                  detail = 1,
                  rewind = FALSE,
                  width = 900,
                  height = 900,
                  res = 150,
                  renderer = gifski_renderer())

anim_save(filename = "Result/state_vaccine.gif",
          animation = anim_p1)




# ggsave("Result/death_positive_point.jpg",
#        death_positive_point,
#        height=4,width=8,scale=1.65)

