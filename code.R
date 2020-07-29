#### COVID19 global forecasting week 5

library(dplyr)
library(tidyverse)

train_data <- read.csv("train.csv")
train_data <- train_data[,-1] %>% filter(Country_Region == "US")


test <- read.csv("test.csv")
test <- test %>% filter(Country_Region == "US")


# While the challenge involves developing quantile estimates intervals for confirmed cases and fatalities between May 12 and June 7 by region, 
# the primary goal isn't only to produce accurate forecasts. It's also to identify factors that appear to impact the transmission rate of COVID-19. 


### reshaping

train <- train_data[,-3] %>% spread(key = Target, value = TargetValue, fill = F) %>% # spread incidence and fatalities into two columns
    unite(Area, County, Province_State, sep = ".", remove = T) %>% # combine counties and states into one column
  group_by(Date, Area) %>%
  summarise(ConfirmedCases = sum(ConfirmedCases), Fatalities = sum(Fatalities), Population = Population, Weight = Weight) %>% # merge duplicate rows
  distinct(Date, Area, .keep_all = T) # remove duplicate rows
   

length(unique(train$Area))
# [1] 3198 counties were data are collected in 140 days 

train_df <- train %>% gather(variable, value, -c(Area, Date)) %>% unite(temp, Area, variable, sep = ".") %>% spread(temp, value) # separate series by time stamp
