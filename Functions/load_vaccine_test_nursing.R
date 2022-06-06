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

# deaths = Number of residents with new suspected or laboratory positive COVID-19 who died in the facility or another location (COVID-19 DEATHS)
# positiveTest = Number of residents with a new positive COVID-19 viral test result.
# positiveTest_NAAT = Of the number of residents with a new positive COVID-19 viral test result, how many had positive NAAT (PCR) only


nurse_df = nurse_file %>%
    select("Week Ending",
           "Provider State",
           "County",
           "Passed Quality Assurance Check",
           "Total Number of Occupied Beds",
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
         bedOccupied = "Total Number of Occupied Beds",
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





# density positive rate
# positiveTest_rate_time_na = positiveTest_rate_time %>%
#   drop_na(positive_test_category)
# fig_vaccine_category_rate_dens = ggplot(data = positiveTest_rate_time_na,
#                                         aes(date,
#                                             fill = positive_test_category)) +
#     geom_density(position = "stack", alpha = .8) +
#     scale_fill_brewer(palette = "Reds") +
#     labs(title = "Positive Test Category Density")
# 
# ggsave("Result/fig_vaccine_category_rate_dens.jpg",
#        fig_vaccine_category_rate_dens,
#        height=4,width=8,scale=1.65)



#####  vaccination and positive rate and death rate in county level #####

nurse_categoryRate = nurse_df %>%
  mutate(totalTest = pointOfCare_test + NonePointOfCare_test) %>%
  mutate(totalTest = ifelse(totalTest < positiveTest, NA, totalTest)) %>%
  group_by(date, state_county) %>%
  summarize(county_vaccineRate = mean(vaccineRate),
            county_death = sum(deaths),
            county_bedOccupied = sum(bedOccupied),
            county_positiveTest = sum(positiveTest),
            county_totalTest = sum(totalTest)) %>%
  mutate(county_deathRate = (county_death / county_bedOccupied) * 100) %>%
  mutate(county_postiveRate = (county_positiveTest / county_totalTest) * 100) %>%
  mutate(positiveRate_category = cut(county_postiveRate,
                                    breaks = c(-Inf, 1, 2, 3, 4, 5, 100),
                                    labels = c("0%-0.9%", "1%-1.9%",
                                               "2%-2.9%", "3%-3.9%",
                                               "4%-4.9%", "5%-100%"))) %>%
  mutate(deathRate_category = cut(county_deathRate,
                                     breaks = c(-Inf, .2, .4, .6, .8, 1, 100),
                                     labels = c("0%-0.19%", "0.2%-0.39%",
                                                "0.4%-5.9%", "0.6%-7.9%",
                                                "0.8%-9.9%", "1%-100%"))) %>%
  mutate(vaccineRate_category = cut(county_vaccineRate,
                                  breaks = c(-Inf, 75, 80, 85, 90, 95, 100),
                                  labels = c("0%-74.9%", "75%-79.9%",
                                             "80%-84.9%", "85%-89.9%",
                                             "90%-94.9%", "95%-100%")))
 

##### counties positiveTest rate ####
counties_positiveRate = nurse_categoryRate %>%
  group_by(date) %>%
  arrange(date, state_county) %>%
  count(positiveRate_category, .drop=FALSE) %>%
  rename("count_county" = "n") %>%
  filter(!is.na(positiveRate_category)) %>%
  mutate(sum_county = sum(count_county)) %>%
  mutate(counties_positive_test_rate = round(count_county / sum_county, 2))

# plot the positiveRate category rate column 
fig_positive_test_category_rate_col = ggplot(data = counties_positiveRate,
                                             aes(x = date,
                                                 y = counties_positive_test_rate,
                                                 fill = positiveRate_category)) +
  geom_col()+
  labs(title = "Proportion of counties with different positive test Rate")+
  scale_fill_manual(values=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'))

ggsave("Result/counties_positive_test_category_rate_nursing_col.jpg",
       fig_positive_test_category_rate_col,
       height=4,width=8,scale=1.65)



##### counties Death Rate ####
counties_deathRate = nurse_categoryRate %>%
  group_by(date) %>%
  arrange(date, state_county) %>%
  count(deathRate_category, .drop=FALSE) %>%
  rename("count_county" = "n") %>%
  filter(!is.na(deathRate_category)) %>%
  mutate(sum_county = sum(count_county)) %>%
  mutate(counties_death_rate = round(count_county / sum_county, 2))

# plot the positiveRate category rate column 
fig_death_category_rate_col = ggplot(data = counties_deathRate,
                                             aes(x = date,
                                                 y = counties_death_rate,
                                                 fill = deathRate_category)) +
  geom_col()+
  labs(title = "Proportion of counties with different death test Rate")+
  scale_fill_manual(values=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'))

ggsave("Result/counties_death_category_rate_nursing_col.jpg",
       fig_death_category_rate_col,
       height=4,width=8,scale=1.65)





##### counties vaccine Rate ####
counties_vaccineRate = nurse_categoryRate %>%
  group_by(date) %>%
  arrange(date, state_county) %>%
  count(vaccineRate_category, .drop=FALSE) %>%
  rename("count_county" = "n") %>%
  filter(!is.na(vaccineRate_category)) %>%
  mutate(sum_county = sum(count_county)) %>%
  mutate(counties_vaccine_rate = round(count_county / sum_county, 2))

# plot the positiveRate category rate column 
fig_vaccine_category_rate_col = ggplot(data = counties_vaccineRate,
                                     aes(x = date,
                                         y = counties_vaccine_rate,
                                         fill = vaccineRate_category)) +
  geom_col()+
  labs(title = "Proportion of counties with different vaccine test Rate")+
  scale_fill_manual(values=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'))

ggsave("Result/counties_vaccine_category_rate_nursing_col.jpg",
       fig_vaccine_category_rate_col,
       height=4,width=8,scale=1.65)



##### positive rate category ####

positiveRate_time = nurse_categoryRate %>%
  drop_na(positiveRate_category) %>%
  group_by(date) %>%
  count(positiveRate_category, .drop = FALSE) %>%
  rename("count_county" = "n")

fig_positiveRate_category = ggplot(data = positiveRate_time,
                                         aes(x = date,
                                             y = count_county,
                                             color = positiveRate_category)) +
  geom_line(size = 1.5)+
  # geom_area(aes(color = positiveRate_category, fill = positiveRate_category), 
  #           alpha = 0.05, position = position_dodge(0.8)) +
  geom_point(size = 3, alpha=.2)+
  labs(title = "Positive Rate Category")+
  scale_color_manual(values=c('#1a9850','#91cf60','#d9ef8b','#fee08b','#fc8d59','#d73027'))+
  theme_minimal()

ggsave("Result/positiveRate_category_rate.jpg",
       fig_positiveRate_category, height=4,width=8,scale=1.65)





##### Death rate category ####

deathRate_time = nurse_categoryRate %>%
  drop_na(deathRate_category) %>%
  group_by(date) %>%
  count(deathRate_category, .drop = FALSE) %>%
  rename("count_county" = "n")

fig_deathRate_category = ggplot(data = deathRate_time,
                                   aes(x = date,
                                       y = count_county,
                                       color = deathRate_category)) +
  geom_line(size = 1.5)+
  geom_point(size = 3, alpha=.2)+
  labs(title = "Death Rate Category")+
  scale_color_manual(values=c('#1a9850','#91cf60','#d9ef8b','#fee08b','#fc8d59','#d73027'))+
theme_minimal()

ggsave("Result/deathRate_category_rate.jpg",
       fig_deathRate_category, height=4,width=8,scale=1.65)



##### Vaccine rate category ####

vaccineRate_time = nurse_categoryRate %>%
  drop_na(vaccineRate_category) %>%
  group_by(date) %>%
  count(vaccineRate_category, .drop = FALSE) %>%
  rename("count_county" = "n")

fig_vaccineRate_category = ggplot(data = vaccineRate_time,
                                aes(x = date,
                                    y = count_county,
                                    color = vaccineRate_category)) +
  geom_line(size = 1.5)+
  geom_point(size = 3, alpha=.2)+
  labs(title = "Vaccine Rate Category")+
  scale_color_manual(values=c('#1a9850','#91cf60','#d9ef8b','#fee08b','#fc8d59','#d73027'))+
  theme_minimal()

ggsave("Result/vaccineRate_category_rate.jpg",
       fig_vaccineRate_category, height=4,width=8,scale=1.65)

fig_mix = grid.arrange(fig_positive_test_category_rate_col,
                       fig_death_category_rate_col,
                       nrow = 2)

ggsave("Result/mixed.jpg",
       fig_mix, height=4,width=8,scale=1.65)

fig_mix1 = grid.arrange(fig_positiveRate_category,
                       fig_deathRate_category,
                       nrow = 2)

ggsave("Result/mixed1.jpg",
       fig_mix1, height=4,width=8,scale=1.65)


a =select(nurse_file, "Number of Residents Staying in this Facility for At Least 1 Day This Week")
unique(a)

Positive Antigen Tests Only: Number of Residents
Not Vaccinated with COVID-19 Vaccine Before
Positive Test
Positive Antigen Tests Only: Number of Residents
who Received Pfizer-BioNTech COVID-19 Vaccine
Dose 1 Only Before Positive Test
Positive Antigen Tests Only: Number of Residents
who Received Pfizer-BioNTech COVID-19 Vaccine
Doses 1 and 2 Before Positive Test

Positive Antigen Tests Only: Number of Residents
who Received Complete Unspecified COVID-19
Vaccine Before Positive Test
Any Other Combination of Antigen Test and/or

AAT (PCR) Test with At Least One Positive Test:
  Number of Residents Not Vaccinated with COVID-19

Vaccine Before Positive Test
Any Other Combination of Antigen Test and/or

NAAT (PCR) Test with At Least One Positive Test:
  Number of Residents who Received Complete

Unspecified COVID-19 Vaccine Before Positive Test
Any Other Combination of Antigen Test and/or

NAAT (PCR) Test with At Least One Positive Test:
  Number of Residents who Received Partial

Unspecified COVID-19 Vaccine Before Positive Test
Any Other Combination of Antigen Test and/or

NAAT (PCR) Test with At Least One Positive Test:
  COVID-19 Vaccine Booster Received 14 days or
More Before the Specimen Collection Date

Number of Residents Staying in this Facility for At
Least 1 Day This Week

Percentage of Current Residents who Received a

Completed COVID-19 Vaccination at Any Time
Percentage of Current Residents who Received a

Partial COVID-19 Vaccination at Any Time

Percentage of Current Healthcare Personnel who
Received a Completed COVID-19 Vaccination at Any
Time

Percentage of Current Healthcare Personnel who
Received a Partial COVID-19 Vaccination at Any
Time

Percentage of Current Residents with a Completed
Vaccination who Received a COVID-19 Vaccine
Booster at Any Time

Percentage of Current Healthcare Personnel with a
Completed Vaccination who Received a COVID-19
Vaccine Booster at Any Time
