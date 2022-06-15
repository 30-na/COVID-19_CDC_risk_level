library(tidycensus)
library(sf)
library(usdata)
library(ggthemes)
library(gganimate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tigris)
census_api_key("7e83aa1d195fd7cd921e4ac747998c618f05460d")

#getting state map geometry and merge it with mortality rate data
stateGeo = get_acs(geography = "state",
                   variable = "B04004_001",
                   geometry = TRUE) %>%
    shift_geometry() %>%
    rename("state" = NAME) 



# load nursery home datasets
load( "Data/nurse_df.csv")

# summerize data to state level
# Note: mortality rate = death / occupied bed
nurse_geo_state = nurse_df %>%
    drop_na(state) %>%
    group_by(date, state) %>%
    summarize(deaths = sum(deaths, na.rm = TRUE),
              occupied_bed = sum(occupied_bed, na.rm = TRUE)) %>%
    mutate(mortalityRate = (deaths / occupied_bed) ) %>%
    left_join(stateGeo, by = "state") %>%
    mutate(state_ab = state2abbr(state)) %>%
    dplyr::filter(date >= "2021-06-30")

unique(nurse_geo_state$date)

# map of state with mortality rate
mortality_state_map = ggplot(data = nurse_geo_state) + 
    geom_sf(aes(geometry = geometry,
                fill = mortalityRate)) + 
    geom_sf_text(aes(geometry = geometry,
                     label = state_ab),
                 size = 3)+
    ggthemes::theme_map() + 
    theme(legend.position = "right") + 
    scale_fill_gradient(name = "Mortality Rate", 
                         low = "#fff5f0", 
                         high = "#a50f15",
                         labels = scales::percent) +
    transition_time(date) +
    #view_follow(fixed_x=T, fixed_y=T) +
    labs(title = 'Date: {frame_time}') +
    ease_aes('cubic-in-out') 

anim_p1 = animate(mortality_state_map,
                  #fps = 4,
                  start_pause = 1,
                  end_pause = 1,
                  duration = 30,
                  detail = 1,
                  rewind = FALSE,
                  width = 1800,
                  height = 1800,
                  res = 170,
                  renderer = gifski_renderer())

anim_save(filename = "Result/state_vaccine.gif",
          animation = anim_p1)



ggsave("Result/temp.jpg",
       mortality_state_map,
       height=4,width=8,scale=1.65)

