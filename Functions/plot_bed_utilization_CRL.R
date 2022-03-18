library(dplyr)
library(usdata)
library(data.table)
library(ggplot2)
library(scales)

load("Data/CDC_risk_level_new.csv")

fig7 = ggplot(na.omit(CDC_risk_clean_new), aes(x=risk_level,y=bed_utilization, color=risk_level))+
    geom_jitter(position = position_jitter(width = 0.02))+
    geom_boxplot(alpha = 0.7)+
    coord_flip()+
    geom_rug()
    

ggsave("Result/Fig7.jpg",fig7, height=4,width=8,scale=1.65)

