
#####
# load libraries
library(dplyr)
library(ggplot2)
library(broom)
library(MASS)


#####
# load nursery home datasets
load( "Data/nurse_df.csv")


#####
# summerize data to state level and filter top 5 state with cumulative mortality rate (after "2021-06-30")
# Note: mortality rate = death / occupied bed
nurse_state_cum_top5_list = nurse_df %>%
    drop_na(state) %>%
    group_by(date, state) %>%
    summarize(deaths = sum(deaths, na.rm = TRUE),
              occupied_bed = sum(occupied_bed, na.rm = TRUE)) %>%
    mutate(mortalityRate = (deaths / occupied_bed) ) %>%
    mutate(state_ab = state2abbr(state)) %>%
    dplyr::filter(date >= "2021-06-30") %>%
    group_by(state) %>%
    mutate(mortalityRate_cum = cumsum(mortalityRate)) %>%
    group_by(state) %>%
    dplyr::arrange(desc(mortalityRate_cum)) %>%
    dplyr::select(state) %>%
    distinct() %>%
    head(5)


#####
# summarize data to county level and filter by date and top 5 state
# Note: mortality rate = death / occupied bed
# Note: accupancyRate = occupied_bed / total_bed
nurse_county = nurse_df %>%
    drop_na(state, county) %>%
    filter(date >= "2021-06-30") %>%
    filter(state %in% nurse_state_cum_top5_list$state) %>%
    group_by(date, state, county) %>%
    mutate(nurse_shortage = dplyr::case_when(nurse_shortage == "Y" ~ 1,
                                             nurse_shortage == "N" ~ 0))  %>%
    summarize(deaths = sum(deaths, na.rm = TRUE),
              occupied_bed = sum(occupied_bed, na.rm = TRUE),
              nurse_shortage = mean(nurse_shortage, na.rm = TRUE),
              total_bed = sum(total_bed, na.rm = TRUE),
              resident_vaccineRate = mean(resident_vaccineRate, na.rm = TRUE),
              personnel_vaccineRate = mean(personnel_vaccineRate, na.rm = TRUE),
              resident_vaccineRate_boost = mean(resident_vaccineRate_boost, na.rm = TRUE),
              personnel_vaccineRate_boost = mean(personnel_vaccineRate_boost, na.rm = TRUE))%>% 
    ungroup() %>%
    mutate(mortalityRate = (deaths / occupied_bed) * 100,
           occupancyRate = (occupied_bed / total_bed) * 100,
           state_abb = state2abbr(state),
           occupancyRate = if_else(occupancyRate < 100, occupancyRate, NULL)) %>%
    drop_na(mortalityRate) %>%
    mutate(mortalityRate = ifelse(mortalityRate == Inf, NA, mortalityRate))

##### 
#Linear models for mortality rate
fit = lm(mortalityRate ~ cbind(resident_vaccineRate, personnel_vaccineRate,
                               resident_vaccineRate_boost, personnel_vaccineRate_boost,
                               occupancyRate, nurse_shortage),
         data = nurse_county)

fit = lm(mortalityRate ~ nurse_shortage,
         data = nurse_county)

summary(fit)
models = summary(fit) %>%
    map_dfr(glance) %>%
    mutate(features = c("resident_vaccineRate", "personnel_vaccineRate",
                         "resident_vaccineRate_boost", "personnel_vaccineRate_boost",
                         "occupancyRate", "nurse_shortage")) %>%
    arrange(p.value)
