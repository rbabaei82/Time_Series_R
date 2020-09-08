#### COVID19 global forecasting week 5
required_packages <- c("tidyverse", "xts", "TSstudio", "lubridate", "forecast", "plotly", "h2o")
suppressPackageStartupMessages(lapply(required_packages, require, character.only = T))


train_data <- read.csv("train.csv")
train_data <- train_data[,-1] %>% filter(Country_Region == "US")

locations = train_data %>% distinct(County, Country_Region, Province_State) %>%
  arrange(County, Country_Region, Province_State)

train_data <- train_data %>% arrange(County, Country_Region, Province_State)
train_data$TargetValue <- abs(train_data$TargetValue)
train_data$Weight <- abs(train_data$Weight)



train <- train_data
index <- which(train$County == "")# find blank rows in counties and replace with state
train[index,"County"] <- "State"

index <- which(train$Province_State == "")
train[index, "Province_State"] <- "US"

rm(index, train_data)


## consider weighting scores
train$WeightedValue <- train$Weight*train$TargetValue


test <- read.csv("test.csv")
test <- test %>% filter(Country_Region == "US")


# While the challenge involves developing quantile estimates intervals for confirmed cases and fatalities between May 12 and June 7 by region, 
# the primary goal isn't only to produce accurate forecasts. It's also to identify factors that appear to impact the transmission rate of COVID-19. 


### reshaping

train <- train[,-c(3:5,8)] %>% spread(key = Target, value = WeightedValue, fill = F) %>% # spread incidence and fatalities into two columns
    unite(Area, County, Province_State, sep = ".", remove = T) %>% # combine counties and states into one column
  group_by(Date, Area) %>%
  summarise(ConfirmedCases = sum(ConfirmedCases), Fatalities = sum(Fatalities)) %>% # merge duplicate rows
  distinct(Date, Area, .keep_all = T) # remove duplicate rows


 

length(unique(train$Area))
# [1] 3198 counties were data are collected in 140 days 

Area <- unique(train$Area)
train_ts <- list()

for(i in 1:length(Area)){
train_ts[[i]] <- xts(x = train[which(train$Area == Area[i]),3:4],
                     frequency = 365.25, order.by = as.Date(train[which(train$Area == Area[i]), 1]$Date))
names(train_ts)[i] <- Area[i]
}



tail(train_ts[[2]])
ts_info(train_ts[[1]])


## take new york as basic model to start

NewYork_cases <- train_ts$`New York.New York`$ConfirmedCases
NewYork_fatalities <- train_ts$`New York.New York`$Fatalities


######### 
ts_info(NewYork_cases)
plot.xts(NewYork_cases,
         main = "Daily cases",
         col = "blue",
         major.ticks = "months",
         minor.ticks = "days")

lines(NewYork_fatalities ,
      col = "red",
      type = "h",
      on = NA,
      main = "Daily Deaths") # has only one cycle, which starts at 10th of March 2020


acf(NewYork_cases) # not tailing off
ts_lags(NewYork_cases, lags = c(1,7)) # strong relation to the first lag

## subset the series as data frame
df_cases <- ts_to_prophet(NewYork_cases)
names(df_cases) <- c("date", "cases")

head(df_cases)

ts_plot(df_cases)

#### feature engineering
df_cases <- df_cases %>% mutate(wday = factor(wday(date, label = T),
                                              ordered = F),
                                lag7 = lag(cases, n = 7)) %>% filter(!is.na(lag7))
df_cases$trend <- 1:nrow(df_cases)
df_cases$trend_sqr <- df_cases$trend^2


## split for modeling
h <- 45 # evaluation period
# train_df_cases <- df_cases[1:(nrow(df_cases)-h),]
test_df_cases <- df_cases[(nrow(df_cases)-h+1):nrow(df_cases),]
# forecasting df
fc_df_cases <- data.frame(date = seq.Date(from = max(df_cases$date)+ days(1),
                                          length.out = 7, by = "day"),
                          trend = seq(from = max(df_cases$trend) + 1,
                                      length.out = 7, by = 1))
fc_df_cases$trend_sqr <- fc_df_cases$trend^2
fc_df_cases$wday <- factor(wday(fc_df_cases$date, label = T), ordered = F)
fc_df_cases$lag7 <- tail(df_cases$cases, 7)


#### model benchmark
lr <- lm(cases ~ lag7 + wday +trend + trend_sqr, data = df_cases)
summary(lr) # Adjusted R-squared:  0.77

# predict with testing
test_df_cases$yhat <- predict(lr, newdata = test_df_cases)

# model evaluation

mape_lr <- mean(abs(test_df_cases$cases - test_df_cases$yhat)/test_df_cases$cases)
mape_lr # 0.52   52% will be used to benchmark the performance of ML models


##### h2o cluster

h2o.init(max_mem_size = "16G")
# train_cases_h <- as.h2o(train_df_cases)
train_h <- as.h2o(df_cases)
test_cases_h <- as.h2o(test_df_cases)

fc_cases_h <- as.h2o(fc_df_cases)

x <- c("wday", "lag7", "trend", "trend_sqr")
y <- "cases"

### training random forest

rf_md <- h2o.randomForest(training_frame = train_h,
                          nfolds = 5,
                          x = x, y = y,
                          ntrees = 500,
                          stopping_rounds = 10,
                          stopping_metric = "RMSE",
                          score_each_iteration = T,
                          stopping_tolerance = 0.0001,
                          seed = 1234)
h2o.varimp_plot(rf_md)
## trend_sqr and lag are the most important variables to the model


rf_md@model$model_summary # 38 trees have been used

## learning process of the model
tree_score <- rf_md@model$scoring_history$training_rmse

plot_ly(x = seq_along(tree_score), y = tree_score,
        type = "scatter", mode = "line") %>%
  layout(title = "Score History of rf-_md",
         yaxis = list(title="RMSE"),
         xaxis = list(title = "number of trees"))

## model performance

test_cases_h$pred_rf <- h2o.predict(rf_md, test_cases_h)
test_1 <- as.data.frame(test_cases_h)

mape_rf <- mean(abs(test_1$cases - test_1$pred_rf)/test_1$cases)
mape_rf # 0.10 


## model tuning
hyper_params_rf <- list(mtries = c(2,3,4),
                        sample_rate = c(0.632, 0.8, 0.95),
                        col_sample_rate_per_tree = c(0.5, 0.9, 1.0),
                        max_depth = c(seq(1, 30, 3)),
                        min_rows = c(1, 2, 5, 10))
search_criteria_rf <- list(strategy = "RandomDiscrete",
                           stopping_metric = "RMSE",
                           stopping_tolerance = 0.0001,
                           stopping_rounds = 10,
                           max_runtime_secs = 60 * 20)

rf_md2 <- h2o.grid(algorithm = "randomForest",
                   search_criteria = search_criteria_rf,
                   hyper_params = hyper_params_rf,
                   x = x, y = y, training_frame = train_h,
                   ntrees = 5000,
                   nfolds = 5,
                   grid_id = "rf_grid",
                   seed = 1234)

rf_grid_search <- h2o.getGrid(grid_id = "rf_grid",
                              sort_by = "rmse",
                              decreasing = F)
rf_grid_model <- h2o.getModel(rf_grid_search@model_ids[[1]])

# model performance
test_cases_h$rf_grid <- h2o.predict(rf_grid_model, test_cases_h)
test_1 <- as.data.frame(test_cases_h)
mape_rf_grid <- mean(abs(test_1$cases - test_1$rf_grid)/test_1$cases)
mape_rf_grid # 0.16

## plot all models
plot_ly(data = test_1) %>%
  add_lines(x = ~date, y = ~cases, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", line = list(dash = "dot")) %>%
  add_lines(x = ~ date, y = ~ pred_rf, name = "Random Forest", line = list(dash = "dash")) %>%
  add_lines(x = ~ date, y = ~rf_grid, name = "Random Forest (grid)", line = list(dash = "dash")) %>%
  layout(title = "daily cases-models comparison",
         yaxis = list(title = "daily cases"),
         xaxis = list(title = "date"))




####### training GBM
gbm_md <- h2o.gbm(training_frame = train_h,
                  nfolds = 5,
                  x = x, y = y,
                  max_depth = 20,
                  distribution = "gaussian",
                  ntrees = 500,
                  learn_rate = 0.1,
                  score_each_iteration = T)
h2o.varimp_plot(gbm_md) # lag7 and trend are the most important to the model

test_cases_h$pred_gbm <- h2o.predict(gbm_md, test_cases_h)
test_1 <- as.data.frame(test_cases_h)

mape_gbm <- mean(abs(test_1$cases - test_1$pred_gbm)/test_1$cases)
mape_gbm # 0.033 better performance thatn rf

plot_ly(data = test_1) %>%
  add_lines(x = ~date, y = ~cases, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_rf, name = "Random Forest", line = list(dash = "dash")) %>%
  add_lines(x = ~ date, y = ~pred_gbm, name = "Gradient Boosting", line = list(dash = "dash")) %>%
  layout(title = "daily cases-models comparison",
         yaxis = list(title = "daily cases"),
         xaxis = list(title = "date"))


#### training autoML

autoML <- h2o.automl(training_frame = train_h,
                     x = x, y = y,
                     nfolds = 5,
                     max_runtime_secs = 60*20,
                     seed = 1234)
autoML@leaderboard
# GBM_grid models at top

test_cases_h$pred_autoML <- h2o.predict(autoML@leader, test_cases_h)
test_1 <- as.data.frame(test_cases_h)
mape_autoML <- mean(abs(test_1$cases - test_1$pred_autoML)/test_1$cases)
mape_autoML # 0.18



##### GBM showing the best performance in all trained models

fc_cases_h$pred_gbm <- h2o.predict(gbm_md, fc_cases_h)
fc_df <- as.data.frame(fc_cases_h)

### plot forcast

plot_ly(x = df_cases$date, y = df_cases$cases,
        type = "scatter",
        mode = "line",
        name = "Actual") %>%
  add_lines(x = fc_df$date, y = fc_df$pred_gbm,
            name = "Gradient Boosting Forecast") %>%
  layout(title = "Final Forecast of Daily Cases",
         yaxis = list(title = "daily cases"),
         xaxis = list(title = "Date"))


#### ARIMA model
auto.arima_md <- auto.arima(NewYork_cases)
auto.arima_md # AIC=1401.35 ARIMA(2,1,2)

checkresiduals(auto.arima_md) # p-value = 0.08558 residuals defined as white noise


cases_fc1 <- forecast(auto.arima_md, h = 7)
plot_forecast(cases_fc1,
              title = "daily cases",
              Ytitle = "number of infected",
              Xtitle = "date")





######### fatalities series
ts_info(NewYork_fatalities)
plot.xts(NewYork_cases,
         main = "Daily cases",
         col = "blue",
         major.ticks = "months",
         minor.ticks = "days")

lines(NewYork_fatalities ,
      col = "red",
      type = "l",
      on = NA,
      main = "Daily Deaths") # has only one cycle, which starts at 10th of March 2020


acf(NewYork_fatalities) # not tailing off
ts_lags(NewYork_fatalities) # strong relation to the first lag

## subset the series as data frame
df_fatalities <- ts_to_prophet(NewYork_fatalities)
names(df_fatalities) <- c("date", "cases")

head(df_fatalities)

ts_plot(df_fatalities)

#### feature engineering
df_fatalities <- df_fatalities %>% mutate(wday = factor(wday(date, label = T),
                                              ordered = F),
                                lag7 = lag(cases, n = 7)) %>% filter(!is.na(lag7))
df_fatalities$trend <- 1:nrow(df_fatalities)
df_fatalities$trend_sqr <- df_fatalities$trend^2


## split for modeling
h <- 45 # evaluation period
# train_df_cases <- df_cases[1:(nrow(df_cases)-h),]
test_df_fatalities <- df_fatalities[(nrow(df_fatalities)-h+1):nrow(df_fatalities),]
# forecasting df
fc_df_fatalities <- data.frame(date = seq.Date(from = max(df_fatalities$date)+ days(1),
                                          length.out = 7, by = "day"),
                          trend = seq(from = max(df_fatalities$trend) + 1,
                                      length.out = 7, by = 1))
fc_df_fatalities$trend_sqr <- fc_df_fatalities$trend^2
fc_df_fatalities$wday <- factor(wday(fc_df_fatalities$date, label = T), ordered = F)
fc_df_fatalities$lag7 <- tail(df_fatalities$cases, 7)


#### model benchmark
lr <- lm(cases ~ lag7 + wday +trend + trend_sqr, data = df_fatalities)
summary(lr) # Adjusted R-squared:  0.538

# predict with testing
test_df_fatalities$yhat <- predict(lr, newdata = test_df_fatalities)

# model evaluation

mape_lr <- mean(abs(test_df_fatalities$cases - test_df_fatalities$yhat)/test_df_fatalities$cases)
mape_lr # 2.54   52% will be used to benchmark the performance of ML models


##### h2o cluster

h2o.init(max_mem_size = "16G")
# train_cases_h <- as.h2o(train_df_cases)
train_fatalities <- as.h2o(df_fatalities)
test_fatalities_h <- as.h2o(test_df_fatalities)

fc_fatalities_h <- as.h2o(fc_df_fatalities)

x <- c("wday", "lag7", "trend", "trend_sqr")
y <- "cases"

### training random forest

rf_md <- h2o.randomForest(training_frame = train_fatalities,
                          nfolds = 5,
                          x = x, y = y,
                          ntrees = 500,
                          stopping_rounds = 10,
                          stopping_metric = "RMSE",
                          score_each_iteration = T,
                          stopping_tolerance = 0.0001,
                          seed = 1234)
h2o.varimp_plot(rf_md)
## trend_sqr and lag7 are the most important variables to the model


rf_md@model$model_summary # 34 trees have been used

## learning process of the model
tree_score <- rf_md@model$scoring_history$training_rmse

plot_ly(x = seq_along(tree_score), y = tree_score,
        type = "scatter", mode = "line") %>%
  layout(title = "Score History of rf-_md",
         yaxis = list(title="RMSE"),
         xaxis = list(title = "number of trees"))

## model performance

test_fatalities_h$pred_rf <- h2o.predict(rf_md, test_fatalities_h)
test_2 <- as.data.frame(test_fatalities_h)

mape_rf <- mean(abs(test_2$cases - test_2$pred_rf)/test_2$cases)
mape_rf # 0.64


## model tuning
hyper_params_rf <- list(mtries = c(2,3,4),
                        sample_rate = c(0.632, 0.8, 0.95),
                        col_sample_rate_per_tree = c(0.5, 0.9, 1.0),
                        max_depth = c(seq(1, 30, 3)),
                        min_rows = c(1, 2, 5, 10))
search_criteria_rf <- list(strategy = "RandomDiscrete",
                           stopping_metric = "RMSE",
                           stopping_tolerance = 0.0001,
                           stopping_rounds = 10,
                           max_runtime_secs = 60 * 20)

rf_md2 <- h2o.grid(algorithm = "randomForest",
                   search_criteria = search_criteria_rf,
                   hyper_params = hyper_params_rf,
                   x = x, y = y, training_frame = train_fatalities,
                   ntrees = 5000,
                   nfolds = 5,
                   grid_id = "rf_grid",
                   seed = 1234)

rf_grid_search <- h2o.getGrid(grid_id = "rf_grid",
                              sort_by = "rmse",
                              decreasing = F)
rf_grid_model <- h2o.getModel(rf_grid_search@model_ids[[1]])

# model performance
test_fatalities_h$rf_grid <- h2o.predict(rf_grid_model, test_fatalities_h)
test_2 <- as.data.frame(test_fatalities_h)
mape_rf_grid <- mean(abs(test_2$cases - test_2$rf_grid)/test_2$cases)
mape_rf_grid # 1.14

## plot all models
plot_ly(data = test_2) %>%
  add_lines(x = ~date, y = ~cases, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", line = list(dash = "dot")) %>%
  add_lines(x = ~ date, y = ~ pred_rf, name = "Random Forest", line = list(dash = "dash")) %>%
  add_lines(x = ~ date, y = ~rf_grid, name = "Random Forest (grid)", line = list(dash = "dash")) %>%
  layout(title = "daily cases-models comparison",
         yaxis = list(title = "daily cases"),
         xaxis = list(title = "date"))




####### training GBM
gbm_md <- h2o.gbm(training_frame = train_fatalities,
                  nfolds = 5,
                  x = x, y = y,
                  max_depth = 20,
                  distribution = "gaussian",
                  ntrees = 500,
                  learn_rate = 0.1,
                  score_each_iteration = T)
h2o.varimp_plot(gbm_md) # lag7 and trend_sqr are the most important to the model

test_fatalities_h$pred_gbm <- h2o.predict(gbm_md, test_fatalities_h)
test_2 <- as.data.frame(test_fatalities_h)

mape_gbm <- mean(abs(test_2$cases - test_2$pred_gbm)/test_2$cases)
mape_gbm # 0.08 better performance than rf

plot_ly(data = test_2) %>%
  add_lines(x = ~date, y = ~cases, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ pred_rf, name = "Random Forest", line = list(dash = "dash")) %>%
  add_lines(x = ~ date, y = ~pred_gbm, name = "Gradient Boosting", line = list(dash = "dash")) %>%
  layout(title = "daily cases-models comparison",
         yaxis = list(title = "daily cases"),
         xaxis = list(title = "date"))


#### training autoML

autoML <- h2o.automl(training_frame = train_fatalities,
                     x = x, y = y,
                     nfolds = 5,
                     max_runtime_secs = 60*20,
                     seed = 1234)
autoML@leaderboard
# GBM_grid models at top

test_fatalities_h$pred_autoML <- h2o.predict(autoML@leader, test_fatalities_h)
test_2 <- as.data.frame(test_fatalities_h)
mape_autoML <- mean(abs(test_2$cases - test_2$pred_autoML)/test_2$cases)
mape_autoML # 1.38



##### GBM showing the best performance in all trained models

fc_fatalities_h$pred_gbm <- h2o.predict(gbm_md, fc_fatalities_h)
fc_fatalities <- as.data.frame(fc_fatalities_h)

### plot forcast

plot_ly(x = df_fatalities$date, y = df_fatalities$cases,
        type = "scatter",
        mode = "line",
        name = "Actual") %>%
  add_lines(x = fc_fatalities$date, y = fc_fatalities$pred_gbm,
            name = "Gradient Boosting Forecast") %>%
  layout(title = "Final Forecast of Daily Death",
         yaxis = list(title = "daily death"),
         xaxis = list(title = "Date"))


#### ARIMA model
auto.arima_fa <- auto.arima(NewYork_fatalities)
auto.arima_fa # AIC=1524.45 ARIMA(2,1,3)

checkresiduals(auto.arima_fa) # p-value = 0.006054 residuals not defined as white noise


death_fc1 <- forecast(auto.arima_fa, h = 7)
plot_forecast(death_fc1,
              title = "daily death",
              Ytitle = "number of death",
              Xtitle = "date")
