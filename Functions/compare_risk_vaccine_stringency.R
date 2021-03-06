
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)


# load alldata 
load("Data/alldata.csv")
load("Data/stringency.index.csv")
load("Data/CDC.vaccine.overall.csv")


# calculate CDC risk level percentage in US
risk_data = alldata %>% 
    group_by(date) %>%
    count(risk_level) %>%
    na.omit() %>%
    spread(key=risk_level,
           value=n) %>%
    mutate(sum_all = sum(low, moderate, substantial, high, na.rm=TRUE),
           low_percent = (low/sum_all)*100,
           moderate_percent = (moderate/sum_all)*100,
           substantial_percent = (substantial/sum_all)*100,
           high_percent = (high/sum_all)*100)


# merge all dataset    
joined_risk_stringency = merge(risk_data,
                            stringency.index.clean,
                            by = "date")
covid_data = merge(joined_risk_stringency,
                                       CDC.vaccine.overall.clean,
                                       by = "date")

covid_longdata = gather(covid_data, 
                        key = "key",
                        value="value",
                        low_percent:Series_Complete_Pop_Pct) %>%
    arrange(date, key)


fig1 = ggplot(covid_data, aes(x=date)) +
    geom_line(aes(y = low_percent, color="Low Community Risk Level"))+
    geom_line(aes(y = moderate_percent, color="moderate Community Risk Level"))+
    geom_line(aes(y = substantial_percent, color="substantial Community Risk Level"))+
    geom_line(aes(y = high_percent, color="high Community Risk Level"))+
    scale_x_date(date_labels = "%Y %b %d")+
    scale_color_brewer(palette="Dark2")+
    theme_classic()+
    labs(title="CDC Comunity Risk Levels (%)")
    
    
ggsave("Result/Fig1.jpg",fig1, height=4,width=8,scale=1.65)  


fig2 = ggplot(covid_data, aes(x=date)) +
    geom_line(aes(y = stringency_index, color="Stringency Index"))+
    geom_line(aes(y = high_percent, color="high Community Risk Level"))+
    scale_x_date(date_labels = "%Y %b %d")+
    scale_color_brewer(palette="Dark2")+
    theme_classic()+
    labs(title="Compare CDC High Risk level VS Stringency Index")
ggsave("Result/Fig2.jpg",fig2, height=4,width=8,scale=1.65)


fig3 = ggplot(covid_data, aes(x=date)) +
    geom_line(aes(y = Series_Complete_Pop_Pct, color="total Vaccination rate"))+
    geom_line(aes(y = stringency_index, color="Stringency Index"))+
    scale_x_date(date_labels = "%Y %b %d")+
    scale_color_brewer(palette="Dark2")+
    theme_classic()+
    labs(title="Compare Total Vaccination rate VS Stringency Index")
ggsave("Result/Fig3.jpg",fig3, height=4,width=8,scale=1.65)


fig4 = ggplot(covid_data, aes(x=date)) +
    geom_line(aes(y = Series_Complete_Pop_Pct, color="total Vaccination rate"))+
    geom_line(aes(y = high_percent, color="high Community Risk Level"))+
    scale_x_date(date_labels = "%Y %b %d")+
    scale_color_brewer(palette="Dark2")+
    theme_classic()+
    labs(title="Compare CDC High Risk level VS Total Vaccination rate")
ggsave("Result/Fig4.jpg",fig4, height=4,width=8,scale=1.65)


load("Data/CDC.risk.level.csv")

risk_level_states = CDC.risk.clean %>%
    group_by(date,state)%>%
    na.omit()%>%
    count(risk_level, name="county")%>%
    group_by(state,date)%>%
    mutate(sum_county=sum(county))%>%
    arrange(date,state,risk_level)%>%
    mutate(percent_counties= (county/sum_county)*100)


fig5 = ggplot(risk_level_states, aes(x=date, percent_counties, color=state))+
    geom_line()+
    facet_wrap(~risk_level)

ggsave("Result/Fig5.jpg",fig5, height=4,width=8,scale=1.65)


