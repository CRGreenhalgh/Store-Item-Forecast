#############################
#### Sales Prophet Model ####
#############################

#### Libraries I need ####
library(tidyverse)
library(forecast)
library(prophet)
library(lubridate)
#library(DataExplorer)
#library(data.table)

#### Read/Clean data ####
train <- read_csv("train.csv")
test <- read_csv("test.csv")
alldata <- bind_rows(train=train, test=test, .id="Set")

#Create monthly and weekly averages and turn item and store into factors
alldata <- alldata %>% 
  mutate(item = factor(item),
         store = factor(store),
         month = month(date), 
         day = weekdays(date)) %>% 
  group_by(store, item, month) %>% 
  mutate(monthly_avg = mean(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(store, item, day) %>% 
  mutate(weekday_avg = mean(sales, na.rm = TRUE)) %>% 
  ungroup()

#### EDA ####
plot_missing(alldata)
ggplot(data = train_prophet, mapping = aes(x = ds, y = y)) + 
  geom_point()

lam <- BoxCox.lambda(, method = "loglik")
train_prophet$y <- BoxCox(train_prophet, lam)
df.m <- melt(df, measure.vars=c("value", "y"))


 
#### Modelling ####

#Split back into train and test and change variable names to prophet requirements and transform sales
train <- alldata %>% 
  filter(Set == "train") %>% 
  mutate(ds = date,
         y= log1p(sales)) %>%
  select(ds, y, monthly_avg, weekday_avg)

test <- alldata %>% 
  filter(Set == "test") %>% 
  mutate(ds = date) %>%
  select(ds, id, monthly_avg, weekday_avg)

#Run prophet model with yearly, weekly, and daily seasonalities, monthly and weekly average regressors, and US holdiays.
m <- prophet(yearly.seasonality = TRUE,
             weekly.seasonality = TRUE,
             daily.seasonality = TRUE) %>% 
  add_regressor("monthly_avg") %>% 
  add_regressor("weekday_avg") %>% 
  add_country_holidays("US") %>% 
  fit.prophet(train)

#Predict for test set
forecast <- predict(m, test)

preds <- tibble(sales = expm1(forecast$yhat))
preds_final <- data.frame(id = test$id, sales = round(preds))
write_csv(preds_final, "./submission.csv")
