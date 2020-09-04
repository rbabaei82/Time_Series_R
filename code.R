#### COVID19 global forecasting week 5
required_packages <- c("tidyverse", "xts", "TSstudio", "lubridate")
suppressPackageStartupMessages(lapply(required_packages, require, character.only = T))


train_data <- read.csv("train.csv")
train_data <- train_data[,-1] %>% filter(Country_Region == "US")

locations = train_data %>% distinct(County, Country_Region, Province_State) %>%
  arrange(County, Country_Region, Province_State)

train_data <- train_data %>% arrange(County, Country_Region, Province_State)



train <- train_data
index <- which(train$County == "")# find blank rows in counties and replace with state
train[index,"County"] <- "State"

index <- which(train$Province_State == "")
train[index, "Province_State"] <- "US"

rm(index)


## consider weighting scores
train$WeightedValue <- train$Weight*train$TargetValue


test <- read.csv("test.csv")
test <- test %>% filter(Country_Region == "US")


# While the challenge involves developing quantile estimates intervals for confirmed cases and fatalities between May 12 and June 7 by region, 
# the primary goal isn't only to produce accurate forecasts. It's also to identify factors that appear to impact the transmission rate of COVID-19. 


### reshaping

train <- train[,-c(3,5,8)] %>% spread(key = Target, value = WeightedValue, fill = F) %>% # spread incidence and fatalities into two columns
    unite(Area, County, Province_State, sep = ".", remove = T) %>% # combine counties and states into one column
  group_by(Date, Area) %>%
  summarise(ConfirmedCases = sum(ConfirmedCases), Fatalities = sum(Fatalities), Population = Population) %>% # merge duplicate rows
  distinct(Date, Area, .keep_all = T) # remove duplicate rows


 

length(unique(train$Area))
# [1] 3198 counties were data are collected in 140 days 

Area <- unique(train$Area)
train_ts <- list()

for(i in 1:length(Area)){
train_ts[[i]] <- xts(x = train[which(train$Area == Area[i]),3:5],
                     frequency = 7, order.by = as.Date(train[which(train$Area == Area[i]), 1]$Date))
names(train_ts)[i] <- Area[i]
}



tail(train_ts[[2]])
ts_info(train_ts[[1]])


## take new york as basic model to start

NewYork <- train_ts$`New York.New York`

# visualize
plot.xts(NewYork$ConfirmedCases,
         main = "Daily cases",
         col = "blue",
         major.ticks = "months",
         minor.ticks = "days")

lines(NewYork$Fatalities ,
      col = "red",
      type = "h",
      on = NA,
      main = "Daily Deaths")
## the incidence of death and its maximum values are happening after the incidence and the max of daily cases



