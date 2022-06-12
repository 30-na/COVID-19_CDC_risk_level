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
nurse_state_cum = nurse_df %>%
    drop_na(state) %>%
    group_by(date, state) %>%
    summarize(deaths = sum(deaths, na.rm = TRUE),
              occupied_bed = sum(occupied_bed, na.rm = TRUE)) %>%
    mutate(mortalityRate = (deaths / occupied_bed) ) %>%
    mutate(state_ab = state2abbr(state)) %>%
    dplyr::filter(date >= "2021-06-30") %>%
    group_by(state) %>%
    mutate(mortalityRate_cum = cumsum(mortalityRate)) 
    filter(mortalityRate_cum %in% head(mortalityRate_cum, 10))

# mortality rate cumulative state level
fig_mortalityRate_cum_state = ggplot(data = nurse_state_cum,
       aes(x = date,
           y = mortalityRate_cum,
           color = state_ab)) +
    geom_point(alpha = .2)+
    geom_line()+
    labs(title = "Cumulative mortality rate from July 2021") +
    scale_color_viridis_d() +
    theme_minimal()

ggsave("Result/fig_mortalityRate_cum_state.jpg",
       fig_mortalityRate_cum_state,
       height=4,
       width=8,
       scale=1.65)


# mortality rate cumulative state level (Top 5)
# top 5 state
nurse_state_cum_top5_list = nurse_state_cum %>%
    group_by(state) %>%
    dplyr::arrange(desc(mortalityRate_cum)) %>%
    dplyr::select(state) %>%
    distinct() %>%
    head(5)
    
nurse_state_cum_top5 = nurse_state_cum %>%
    filter(state %in% nurse_state_cum_top5_list$state)
    


fig_mortalityRate_cum_state_top5 = ggplot(data = nurse_state_cum_top5,
                                     aes(x = date,
                                         y = mortalityRate_cum,
                                         color = state)) +
    geom_point(alpha = .2)+
    geom_line(size = 1.5)+
    labs(title = "Cumulative mortality rate from July 2021 (Top 5)") +
    scale_color_viridis_d() +
    theme_minimal()

ggsave("Result/fig_mortalityRate_cum_state_top5.jpg",
       fig_mortalityRate_cum_state_top5,
       height=4,
       width=8,
       scale=1.65)
