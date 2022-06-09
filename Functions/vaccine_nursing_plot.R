
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

