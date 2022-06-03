library(dplyr)
library(tidyr)
library(data.table)
library(maps)
library(usdata)
library(gganimate)
library(gifski)
library(gridExtra)


##### load and clean data ####
# load nursing home data for 2020, 2021 and 2022
nurse_file_2020 = fread("Data/faclevel_2020.csv")
nurse_file_2021 = fread("Data/faclevel_2021.csv")
nurse_file_2022 = fread("Data/faclevel_2022.csv")

# merge all data
nurse_file = rbind(nurse_file_2020,
      nurse_file_2021,
      nurse_file_2022)


# colnames(nurse_file) = gsub("-", "", names(nurse_file))
# colnames(nurse_file) = gsub("[()]", "", names(nurse_file))
# colnames(nurse_file) = gsub(" " , "_", names(nurse_file) )
names(nurse_file)

# clean Dataset

# confirmCase = Number of residents with new laboratory positive
# deaths = Number of residents with new suspected or laboratory positive COVID-19 who died in the facility or another location (COVID-19 DEATHS)
# positiveTest = Number of residents with a new positive COVID-19 viral test result.
# positiveTest_NAAT = Of the number of residents with a new positive COVID-19 viral test result, how many had positive NAAT (PCR) only


nurse_df = nurse_file %>%
    select("Week Ending",
           "Provider State",
           "County",
           "Passed Quality Assurance Check",
           "Residents Weekly Confirmed COVID-19",
           "Weekly Resident Confirmed COVID-19 Cases Per 1,000 Residents",
           "Residents Weekly COVID-19 Deaths",
           "Weekly Resident COVID-19 Deaths Per 1,000 Residents",
           "Number of Residents with a New Positive COVID-19 Test Result",
           "Number of Residents with a New Positive COVID-19 Test Result with Positive NAAT (PCR) Test Only",
           "Percentage of Current Residents who Received a Completed COVID-19 Vaccination at Any Time",
           "COVID-19 Non-Point-of-Care Tests Performed on Residents Since Last Report",
           "COVID-19 Point-of-Care Tests Performed on Residents Since Last Report") %>%
  rename(date = "Week Ending",
         state = "Provider State",
         county = "County",
         qualityCheck = "Passed Quality Assurance Check",
         confirmCase = "Residents Weekly Confirmed COVID-19",
         confirmCase_per1000 = "Weekly Resident Confirmed COVID-19 Cases Per 1,000 Residents",
         deaths = "Residents Weekly COVID-19 Deaths",
         deaths_per1000 = "Weekly Resident COVID-19 Deaths Per 1,000 Residents",
         positiveTest = "Number of Residents with a New Positive COVID-19 Test Result",
         positiveTest_NAAT = "Number of Residents with a New Positive COVID-19 Test Result with Positive NAAT (PCR) Test Only",
         vaccineRate = "Percentage of Current Residents who Received a Completed COVID-19 Vaccination at Any Time",
         NonePointOfCare_test = "COVID-19 Non-Point-of-Care Tests Performed on Residents Since Last Report",
         pointOfCare_test = "COVID-19 Point-of-Care Tests Performed on Residents Since Last Report") %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  dplyr::filter(qualityCheck == "Y")  %>%
  mutate(state = abbr2state(state)) %>%
  mutate(state_county = paste(state, county, sep = ",")) %>%
  mutate(state_county = tolower(state_county))



##### map of county with available data #############

available_countyList = nurse_df %>%
  dplyr::select(state_county) %>%
  distinct() %>%
  arrange(state_county)

us_county = map_data("county") %>%
  mutate(state_county = paste(region, subregion, sep = ",")) %>%
  arrange(state_county)

us_state = map_data("state")

available_countyList_map = us_county %>%
  filter(state_county %in% available_countyList$state_county)


cnames = us_state %>%
  group_by(region) %>%
  mutate(long = mean(range(long)))%>%
  mutate(lat = mean(range(lat))) %>%
  mutate(region = state2abbr(region)) %>%
  select(region, long, lat, group) %>%
  distinct() %>%
  mutate(long = replace(long, region == "FL", -81.2)) %>%
  mutate(long = replace(long, region == "MI", -84)) %>%
  mutate(long = replace(long, region == "LA", -92.5)) %>%
  mutate(long = replace(long, region == "VA", -79)) %>%
  mutate(lat = replace(lat, region == "VT", 44.7)) %>%
  mutate(lat = replace(lat, region == "MA", 42.5)) %>%
  mutate(lat = replace(lat, region == "MD", 39.50))


county_map = ggplot(data = us_county,
                    mapping = aes(x = long,
                                  y = lat, 
                                  group = group))+
  geom_polygon(color = "#636363",
               fill = NA,
               size = 0.05) +
  geom_polygon(data = us_state,
               mapping = aes(long,
                             lat,
                             group = group),
               fill = NA, 
               color = "black",
               size = .3) +
  geom_polygon(data = available_countyList_map,
               fill = "#31a354",
               alpha=.5)+
  geom_text(data=cnames, aes(long, lat, label = region), size=3)+
  coord_equal()+
  labs(title = "Counties with available nursing home data",
       subtitle = "") +
  theme_void()

ggsave("Result/available_nursing_data_county_map.jpg", county_map, height=4,width=8,scale=1.65)




##### percentage of positive test rate in total test ####
positiveTest_rate = nurse_df %>%
  mutate(totalTest = pointOfCare_test + NonePointOfCare_test) %>%
  mutate(positiveTest_rate = (positiveTest / totalTest) * 100) %>%
  mutate(positive_test_category = cut(positiveTest_rate,
                                      breaks = c(-Inf, 5, 10, 15, 20, 25, 100),
                                      labels = c("0%-4.9%", "5%-9.9%",
                                                 "10%-14.9%", "15%-19.9%",
                                                 "20%-24.99%", "25%-100%"))) %>%
  group_by(date) %>%
  arrange(date, positive_test_category)
  count(positive_test_category, .drop=FALSE) 
  rename("count_county" = "n") %>%
  filter(!is.na(positive_test_category))%>%
  mutate(sum_county = sum(count_county)) %>%
  mutate(positive_test_category_rate = round(count_county / sum_county, 2))
length(unique(nurse_df$state_county))


# plot the positive test category rate column
fig_positive_test_category_rate_col = ggplot(data = positive_test_time,
                                             aes(x = Date,
                                                 y = positive_test_category_rate,
                                                 fill = positive_test_category)) +
  geom_col()+
  labs(title = "Positive Test Category Rate")+
  scale_fill_manual(values=c('#f0f9e8','#ccebc5','#a8ddb5','#7bccc4','#43a2ca','#0868ac'))

ggsave("Result/positive_test_category_rate_col.jpg",
       fig_positive_test_category_rate_col,
       height=4,width=8,scale=1.65)


