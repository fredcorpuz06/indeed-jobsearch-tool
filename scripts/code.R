library(readr)

data <- read_csv("E:/Senior/DataFest/DataFest/datafest2018NewApril6.csv")

save(data, file="data.RData")

load("data.RData")

str(data)

#Exloring Country
length(unique(data$country))
c <- data.frame(country=data$country)
countries <-  aggregate(x = c, 
                     by = list(c = c$country), 
                     FUN = length)

names(countries) <- c("country", "count")
countries$pct <- countries$count/17635296


#Exloring City
length(unique(data$city))
c1 <- data.frame(city=data$city)
cities <-  aggregate(x = c1, 
                        by = list(c1 = c1$city), 
                        FUN = length)

names(cities) <- c("city", "count")
cities$pct <- cities$count/17635296

data_CA <- data[which(data$country=="CA"),]



data_US <- data[which(data$country=="US"),]
length(unique(data_US$city))
c1 <- data.frame(city=data_US$city, state=data_US$stateProvince)
cities_US <-  aggregate(x = c1, 
                     by = list(city = c1$city, state=c1$state), 
                     FUN = length)
names(cities_US) <- c("RegionName", "Abbreviation", "count")
cities_US$pct <- cities_US$count/14154437

#import zillow housing data
zillow <- read_csv("E:/Senior/DataFest/DataFest/datafest2018NewApril6.csv")

#only keep columns for dates of interest (2016-10 to 2017-11) 
zillow<- zillow[, c(1:4, 109:121)]
#calculate the average cost per city
zillow$avg_price <- rowSums(zillow[,5:17])/13
head(zillow[,c(1:5,18)])
zillow_new <- zillow[,c(2:4,18)]
zillow_new$State <- toupper(zillow_new$StateName)
state_abb$State <- toupper(state_abb$State)

zillow_full <- merge(zillow_new, state_abb, by="State")


city_match <- merge(zillow_full, cities_US, by=c("Abbreviation", "RegionName"))

c_Sal <- data.frame(RegionName=data_US$city, Abbreviation=data_US$stateProvince, Salary=data_US$estimatedSalary)
cities_Salary <-  aggregate(x = c_Sal, 
                        by = list(RegionName = c_Sal$RegionName, Abbreviation=c_Sal$Abbreviation), 
                        FUN = mean, na.rm=TRUE)
cities_Salary <- cities_Salary[,c(1,2,5)]
names(cities_Salary) <- c("RegionName", "Abbreviation", "Salary")

sal_city_full <- merge(cities_Salary, city_match, by=c("Abbreviation", "RegionName"))
sal_city_full <-  sal_city_full[,c(1,4,2,3,7,8,10)]
sal_city_full$SLI <- sal_city_full$Salary/sal_city_full$avg_price

#geocoding
library(opencage)
test <- opencage_forward(placename = as.character(sal_city_full$RegionName))


library(ggmap)
test <- as.numeric(geocode(paste(as.character(sal_city_full$RegionName), as.character(sal_city_full$State), 
                      sep=", "), output = "latlon", source="dsk", messaging=FALSE ))
sal_city_locs <- cbind(sal_city_full,test)

library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(shiny)
library(plotly)
library(Hmisc)
library(stringr)
library(rsconnect)
library(maps)
library(maptools)
library(raster)
library(rgdal)
library(mapproj)

save(sal_city_locs, file="sal_city_locs.RData")

usa <- map_data('state')
usa <- map_data('state', region='massachusetts')

temp <- sal_city_locs[which(sal_city_locs$Abbreviation=='MA'),]

ggplot()+ 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "white", colour = 'black')  + 
  geom_point(aes(x=lon, y=lat, size = SLI*SLI*100, color=log(SLI*SLI)*100), data=temp) 


sal_city_locs$circlesize <- sal_city_locs$SLI*sal_city_locs$SLI*50
sal_city_locs$circlesize <- ifelse(sal_city_locs$circlesize>30,25, sal_city_locs$circlesize)


#pop
pop <- read_csv("E:/Senior/DataFest/pop_data/population_by_zip_2010.csv")

pop$agegroup <- ifelse(pop$minimum_age >= 65, "Old Adult", "")
pop$agegroup <- ifelse(pop$minimum_age < 65 & pop$minimum_age >= 30, "Adult", pop$agegroup)
pop$agegroup <- ifelse(pop$minimum_age >=0 & pop$minimum_age < 21, "Children", pop$agegroup)
pop$agegroup <- ifelse(pop$minimum_age >=21 & pop$minimum_age<30, "Young Adult", pop$agegroup)
pop$agegroup <- ifelse(is.na(pop$agegroup), "All", pop$agegroup)
pop2 <- pop[,c(1,5,7)]
pop3 <- pop2 %>% group_by(agegroup, zipcode) %>% summarise_all(funs(sum))
library(tidyr)
pop4 <- spread(pop3, agegroup, population)

pop4$All2 <- pop4$Adult+pop4$Children+pop4$`Old Adult`+pop4$`Young Adult`
pop4$weird <- pop4$All2/pop4$All

pop4$Adult_PCT <- pop4$Adult/pop4$All2
pop4$YoungAdult_PCT <- pop4$`Young Adult`/pop4$All2
pop4$OldAdult_PCT <- pop4$`Old Adult`/pop4$All2
pop4$Children_PCT <- pop4$Children/pop4$All2

pop_fin <- pop4[,c(1:7)]

library(zipcode)
data(zipcode)
length(unique(c(zipcode$city, zipcode$state)))


meow_data <- merge(zipcode, sal_city_locs, by.x=c("state", "city"), by.y=c("Abbreviation", "RegionName"))
meow <- meow_data[,c(1,2,3,6:15)]


meow2 <- merge(meow, pop_fin, by.x='zip', by.y='zipcode')
vars <- names(meow2[,2:13])

meow3 <- meow2 %>% 
  group_by(state, city, State, Salary, avg_price, count, pct, SLI, lon, lat, circlesize, circlecolor) %>% 
  summarise_all(funs(sum))


meow3$Adult_PCT <- meow3$Adult/meow3$All2
meow3$YoungAdult_PCT <- meow3$`Young Adult`/meow3$All2
meow3$OldAdult_PCT <- meow3$`Old Adult`/meow3$All2
meow3$Children_PCT <- meow3$Children/meow3$All2

meow4 <- meow3[,c(1:12,19:23)]


meow4$Oldpeep_Z <- scale(meow4$OldAdult_PCT, center = T, scale=T)
meow4$Youngpeep_Z <- scale(meow4$YoungAdult_PCT, center=T, scale=T)

meow4$old <- ifelse(meow4$Oldpeep_Z < -0.5223, 1 ,NA)
meow4$old <- ifelse(meow4$Oldpeep_Z >= -0.5223& meow4$Oldpeep_Z<0.2674, 2 ,meow4$old)
meow4$old <- ifelse(meow4$Oldpeep_Z>=0.2674, 3 ,meow4$old)

meow4$young <- ifelse(meow4$Youngpeep_Z < -0.64516, 1 ,NA)
meow4$young <- ifelse(meow4$Youngpeep_Z >= -0.64516& meow4$Youngpeep_Z<0.56485, 2 ,meow4$young)
meow4$young <- ifelse(meow4$Youngpeep_Z>=0.56485, 3 ,meow4$young)


save(meow4, file="meow4.RData")



##### Packaging data_US #####
us <- filter(data_US, city %in% unique(meow4$city))
carlo <- readRDS("predicted_industries.RDS")
us2 <- merge(us, carlo, by="companyId",all=T)
us_data <- us2[,c(5:7,9,14,15,18:20,24)]

us_data$educationRequirement <- ifelse(us_data$educationRequirement=="None",0, us_data$educationRequirement)
us_data$educationRequirement <- ifelse(us_data$educationRequirement=="High school",1, us_data$educationRequirement)
us_data$educationRequirement <- ifelse(us_data$educationRequirement=="Higher education",2, us_data$educationRequirement)

us_data$educationRequirement <- as.factor(us_data$educationRequirement)

state_data <- meow4[,c(1:3,5,9:10,13,20:21)]


save(us_data, state_data, file="datafest_data.RData")


