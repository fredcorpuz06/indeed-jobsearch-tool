########Data Exploration##########

library(readr)
data_sample<- read_csv("~/Desktop/datafest2018-mini.csv")

#of missing values for each column
lapply(data_sample, function(x) {sum(is.na(x))})
# percent of missing values for each column
lapply(data_sample, function(x) {sum(is.na(x))/length(x)})
