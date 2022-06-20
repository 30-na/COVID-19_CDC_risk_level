library(dplyr)
library(usdata)
library(ggplot2)
library(ggExtra)
library(tidyverse)


# load nursery home datasets
load( "Data/nurse_df.csv")

# summerize data to state level
# Note: mortality rate = death / occupied bed
# Note: accupancyRate = occupied_bed / total_bed
nurse_state = nurse_df %>%
    drop_na(state) %>%
    group_by(date, state) %>%
    mutate(nurse_shortage = dplyr::case_when(nurse_shortage == "Y" ~ 1,
                                               nurse_shortage == "N" ~ 0))  %>%
    summarize(deaths = sum(deaths, na.rm = TRUE),
              occupied_bed = sum(occupied_bed, na.rm = TRUE),
              nurse_shortage = mean(nurse_shortage, na.rm = TRUE),
              total_bed = sum(total_bed, na.rm = TRUE),
              resident_vaccineRate = mean(resident_vaccineRate, na.rm = TRUE),
              personnel_vaccineRate = mean(personnel_vaccineRate, na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(mortalityRate = (deaths / occupied_bed) * 100,
           occupancyRate = (occupied_bed / total_bed) * 100,
           state_abb = state2abbr(state),
           occupancyRate = if_else(occupancyRate < 100, occupancyRate, NULL))


###### Mortality rate VS resident vaccine rate #####
fig_resident_vaccineRate_mortalityRate_point = ggplot(data = nurse_state,
             mapping = aes(x = resident_vaccineRate,
                           y = mortalityRate,
                           color = state_abb)) +
    geom_point() +
    #geom_smooth(method = "loess") +
    labs(title="\n \n Mortality rate Vs Residents vaccine rate in state level",
         x = "Residents vaccine rate",
         y = "Mortality rate") +
    theme_bw()

fig_resident_vaccineRate_mortalityRate_point = ggMarginal(fig_resident_vaccineRate_mortalityRate_point,
                                                          type = "histogram")

ggsave("Result/fig_resident_vaccineRate_mortalityRate_point.jpg",
       fig_resident_vaccineRate_mortalityRate_point, height=4,width=8,scale=1.65)
    




fig_resident_vaccineRate_mortalityRate_point_lim2 = ggplot(data = nurse_state,
                                                      mapping = aes(x = resident_vaccineRate,
                                                                    y = mortalityRate)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(title="\n \n Mortality rate(between 0% to 0.5% percent) Vs Residents vaccine rate in state level",
         x = "Residents vaccine rate",
         y = "Mortality rate") +
    ylim(0,.5)+
    theme_bw()

fig_resident_vaccineRate_mortalityRate_point_lim2 = ggMarginal(fig_resident_vaccineRate_mortalityRate_point_lim2,
                                                          type = "histogram")

ggsave("Result/fig_resident_vaccineRate_mortalityRate_point_lim2.jpg",
       fig_resident_vaccineRate_mortalityRate_point_lim2, height=4,width=8,scale=1.65)






###### Mortality rate VS personnel vaccine rate #####
fig_personnel_vaccineRate_mortalityRate_point = ggplot(data = nurse_state,
                                                      mapping = aes(x = personnel_vaccineRate,
                                                                    y = mortalityRate,
                                                                    color = state_abb)) +
    geom_point() +
    #geom_smooth(method = "loess") +
    labs(title="\n \n Mortality rate Vs personnel vaccine rate in state level",
         x = "personnel vaccine rate",
         y = "Mortality rate") +
    theme_bw()

fig_personnel_vaccineRate_mortalityRate_point = ggMarginal(fig_personnel_vaccineRate_mortalityRate_point,
                                                          type = "histogram")

ggsave("Result/fig_personnel_vaccineRate_mortalityRate_point.jpg",
       fig_personnel_vaccineRate_mortalityRate_point, height=4,width=8,scale=1.65)





fig_personnel_vaccineRate_mortalityRate_point_lim2 = ggplot(data = nurse_state,
                                                           mapping = aes(x = personnel_vaccineRate,
                                                                         y = mortalityRate)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(title="\n \n Mortality rate(between 0% to 0.5% percent) Vs personnel vaccine rate in state level",
         x = "personnel vaccine rate",
         y = "Mortality rate") +
    ylim(0,.5)+
    theme_bw()

fig_personnel_vaccineRate_mortalityRate_point_lim2 = ggMarginal(fig_personnel_vaccineRate_mortalityRate_point_lim2,
                                                               type = "histogram")

ggsave("Result/fig_personnel_vaccineRate_mortalityRate_point_lim2.jpg",
       fig_personnel_vaccineRate_mortalityRate_point_lim2, height=4,width=8,scale=1.65)


###### Mortality rate VS nurse_shortage rate #####
fig_nurse_shortage_mortalityRate_point = ggplot(data = nurse_state,
                                                mapping = aes(x = nurse_shortage,
                                                              y = mortalityRate)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(title="\n \n Mortality rate Vs nurse shortage mean in state level",
         x = "nurese shortage mean",
         y = "Mortality rate") +
    theme_bw()

fig_nurse_shortage_mortalityRate_point = ggMarginal(fig_nurse_shortage_mortalityRate_point,
                                                                type = "histogram")

ggsave("Result/fig_nurse_shortage_mortalityRate_point.jpg",
       fig_nurse_shortage_mortalityRate_point, height=4,width=8,scale=1.65)

###### Mortality rate VS occupancyRate rate #####
fig_occupancyRate_mortalityRate_point = ggplot(data = nurse_state,
                                                mapping = aes(x = occupancyRate,
                                                              y = mortalityRate)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(title="\n \n Mortality rate Vs Bed occupancy rate in state level",
         x = "Bed occupancy rate",
         y = "Mortality rate") +
    theme_bw()

fig_occupancyRate_mortalityRate_point = ggMarginal(fig_occupancyRate_mortalityRate_point,
                                                    type = "histogram")

ggsave("Result/fig_occupancyRate_mortalityRate_point.jpg",
       fig_occupancyRate_mortalityRate_point, height=4,width=8,scale=1.65)





fig_state_mortality_box = ggplot(data = nurse_state,
       mapping = aes(x = mortalityRate,
                     y = reorder(state, mortalityRate, median, na.rm = TRUE),
                     group = state)) +
    geom_boxplot() +
    stat_summary(fun = median,
                 colour = "blue",
                 geom = "point",
                 shape = 18,
                 size = 2) +
    labs(title = "\nMortality rate in state level\n",
         x = "Mortality rate",
         y = NULL) +
    # geom_vline(yintercept = .5,
    #            linetype="dashed",
    #            color = "red",
    #            size = 1) +
    xlim(0, .5)+
    theme_bw()

ggsave("Result/fig_state_mortality_box.jpg",
       fig_state_mortality_box, height=8,width=4,scale=1.65)

