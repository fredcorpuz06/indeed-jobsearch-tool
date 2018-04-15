library(readr)
library(dplyr)
library(readr)
library(zipcode)
library(magrittr)
library(ggmap)


# read all data
data <- read_csv("../data/datafest2018NewApril6.csv")

# filter US only 
US_indeed <- data %>%
  filter(country == "US")

# get all unique "city, state"
city_states <- US_indeed %>%
  select(city, stateProvince) %>% 
  distinct()

# # count number of postings per "city, state"
# city_states_counts <- city_states %>%
#   group_by(city, stateProvince) %>%
#   summarise(count = n()) %>%
#   mutate(perc = count / sum(count))


# import state StateAbbreviation data - state_abb.csv
state_abb <- read_csv("../data/state_abb.csv") %>%
  mutate(State = toupper(State)) %>%
  rename(StateAbbreviation = Abbreviation)

#import zillow housing data - zillow.csv
zillow <- read.csv("../data/zillow.csv", 
                   stringsAsFactors = F) %>%            # read_csv throws an error 
  select(RegionID, RegionName, StateName,	
         SizeRank, X2016.10:X2017.11) %>%               # select variables
  mutate(avg_price = rowMeans(.[5:18], na.rm = T)) %>%
  select(RegionName:SizeRank, avg_price) %>%
  mutate(State = toupper(StateName)) %>%
  left_join(state_abb)                                  # join state_abb to add state StateAbbreviation  


# get all city, state that are found in both zillow and indeed data sets
city_match <- merge(zillow, city_states, 
                    by.x = c("StateAbbreviation", "RegionName"), 
                    by.y = c("stateProvince", "city"), 
                    all = F)

# combine salary, rent prices to get sli (standard of living) metric: average salary / average rent price
salary_sli_statistics <- US_indeed %>%
  group_by(city, stateProvince) %>%
  summarise(avg_salary = mean(estimatedSalary, na.rm = T)) %>%
  rename(StateAbbreviation = stateProvince,
         RegionName = city) %>%
  inner_join(city_match) %>%
  mutate(sli = avg_salary / avg_price) %>%
  as.data.frame()

#geocoding
latlon <- salary_sli_statistics %$%
  geocode(paste(RegionName, State, sep=", "), 
          output = "latlon", 
          source="dsk", 
          messaging=FALSE)

salary_sli_statistics <- cbind(salary_sli_statistics, latlon)

saveRDS(salary_sli_statistics, file="../data/salary_sli_statistics.RDS")


# population
pop <- read_csv("../data/population_by_zip_2010.csv")

  # create age group variable

pop$agegroup <- cut(pop$minimum_age,
                    breaks = c(0, 21, 30, 65, Inf),
                    labels = c("Children", "YoungAdult", "Adult", "OldAdult"),
                    right = T) %>%
  as.character()
pop$agegroup[is.na(pop$agegroup)] <- "Others"

# activate zipcode data
data(zipcode)

pop %>%
  select(population, zipcode, agegroup) %>%
  rename(zip = zipcode) %>%
  inner_join(zipcode %>%
               select(zip, city, state)) %>%
  group_by(agegroup, state, city) %>% 
  summarise_at(vars(-zip), sum) %>%
  spread(agegroup, population) %>%
  mutate(Total = Children+YoungAdult+Adult+OldAdult+Others,
         ChildrenPCT = Children / Total,
         YoungAdultPCT = YoungAdult / Total,
         AdultPCT = Adult / Total,
         OldAdultPCT = OldAdult / Total) -> population_distribution_by_age_zipcode


city_statistics <- merge(population_distribution_by_age_zipcode, 
                         salary_sli_statistics, 
                         by.x = c("city", "state"), 
                         by.y = c("RegionName", "StateAbbreviation"))

# add categories
city_statistics <- city_statistics %>%
  mutate(old = cut(OldAdultPCT, 
                   quantile(OldAdultPCT), 
                   labels = 1:4, 
                   right = T),
         young = cut(YoungAdultPCT, 
                     quantile(YoungAdultPCT), 
                     labels = 1:4, 
                     right = T))
  
saveRDS(city_statistics, file = "../data/city_statistics.RDS")



#### FIXING ERRORS IN INDEED DATA SET ####

# education requirement
# adding imputed industries

imputed_industries <- readRDS("../data/predicted_industries.RDS")

US_indeed <- US_indeed %>%
  mutate(educationRequirement = ifelse(educationRequirement == "None", 0,
                                       ifelse(educationRequirement == "High school", 1,
                                              ifelse(educationRequirement == "Higher education", 2, NA)))) %>%
  full_join(imputed_industries) %>%
  mutate(industry_combined = ifelse(!is.na(industry), industry, industry_pred))

saveRDS(US_indeed, "../data/indeed_US_clean.RDS")




