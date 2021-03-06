

library(dplyr)
library(usdata)
library(data.table)

# read data from CDC Community Risk level 
CDC.risk = fread("Data/United_States_COVID-19_County_Level_of_Community_Transmission_Historical_Changes.csv")

#clean data
CDC.risk.clean = CDC.risk %>%
  select(date,
         state_name,
         county_name,
         community_transmission_level)%>%
  rename(state = state_name,
         county = county_name,
         risk_level = community_transmission_level) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y"),
         state = state2abbr(state),
         risk_level = factor(risk_level,
                             levels=c("low",
                                      "moderate",
                                      "substantial",
                                      "high"))) %>%
  arrange(date, state, county) %>%
  filter(date >= "2021/01/01")

save(CDC.risk.clean, file="Data/CDC.risk.level.csv") 





# read data from CDC vaccinations for county
CDC.vaccine = fread("Data/COVID-19_Vaccinations_in_the_United_States_County.csv")

#clean data
CDC.vaccine.clean = CDC.vaccine %>%
  select(Date,
         Recip_State,
         Recip_County,
         FIPS,
         Completeness_pct)%>%
  rename(state = Recip_State,
         county = Recip_County,
         fips = FIPS,
         fully_vaccined = Completeness_pct,
         date = Date) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>%
  arrange(date, state, county) %>%
  filter(date >= "2021/01/01")
save(CDC.vaccine.clean, file="Data/CDC.vaccine.county.csv")





# read data from CDC vaccinations overall
CDC.vaccine.overall = fread("Data/COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv")


#clean data
CDC.vaccine.overall.clean = CDC.vaccine.overall %>%
  filter(Location == "US") %>%
  select(Date,
         Series_Complete_Pop_Pct)%>%
  rename(date = Date) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>%
  arrange(date) %>%
  filter(date >= "2021/01/01")
save(CDC.vaccine.overall.clean, file="Data/CDC.vaccine.overall.csv")





# read data from Our World in Data
stringency.inedx = fread("Data/covid-stringency-index.csv")

#clean data
stringency.index.clean = stringency.inedx %>%
  mutate(date = as.Date(Day, format="%Y-%m-%d")) %>%
  filter(Entity == "United States",
         date >= "2021/01/01") %>%
  dplyr::select(date,
         stringency_index) %>%
  arrange(date) 
save(stringency.index.clean, file="Data/stringency.index.csv")



# merge all datasets
joined_vaccine_risk = merge(CDC.risk.clean,
                CDC.vaccine.clean,
                by = c("date", "state", "county"),
                all.y=FALSE,
                all.x=FALSE)

alldata = merge(joined_vaccine_risk,
                stringency.index.clean,
                by="date",
                sort=FALSE,
                all=TRUE) %>%
  arrange(date, state, county)

stringency.inedx = fread("Data/covid-stringency-index.csv")


save(alldata, file="Data/alldata.csv")
