library(readr)
library(ggplot2)
library(dplyr)
library(GGally)
library(gridExtra)
library(e1071)
library(tidyr)
library(randomForest)
library(data.table)
library(tidyverse)
library(lubridate)
library(caret)
library(mice)
library(VIM)

# Data
patient <- read_csv("Desktop/patient.csv")

View(patient)

# Feature man
# start reading from here run code above (the chuck of factors first
# , then the imputes, then the completing the data set)
# side by side of NAs vs filling the data using a multi log regs 
# Feature manu
patient[is.na(patient)] <- 0
patient$sex = as.factor(patient$sex)
patient$country = as.factor(patient$country)
patient$region = as.factor(patient$region)
patient$disease = as.factor(patient$disease)
patient$group = as.factor(patient$group)
patient$infection_reason = as.factor(patient$infection_reason)
patient$infection_order = as.factor(patient$infection_order)
patient$infected_by = as.factor(patient$infected_by)
patient$confirmed_date = as.factor(patient$confirmed_date)
patient$released_date = as.factor(patient$released_date)
patient$deceased_date = as.factor(patient$deceased_date)
patient[patient$state == "isolated",]$state = 0 
patient[patient$state == "released",]$state = 1
patient[patient$state == "deceased",]$state = 2
patient$state = as.factor(patient$state)

# replace sex nulls with the portion of male to females in china 

summary(patient)


# RF 

set.seed(300)
sample = sample(3, nrow(patient), replace = T, prob = c(.6,.2,.2))
train = patient[sample ==1,]
test = patient[sample ==2,]
val = patient[sample ==3,]

model1 = randomForest(train$sex~., data = train, mtry = 3, importance = T)

print(model1)

#Error
plot(model1, 
     main = "Model One Error")

# Tune
head(train)

train <- as.data.frame(train)

t = tuneRF(train[,-2], train[,2],
           stepFactor = .5,
           plot = T,
           ntreeTry = 500, 
           trace = T,
           improve = 1)

# Prediction 
p1 = predict(model1, train)
confusionMatrix(p1, train$sex)

p2 = predict(model1, test)
confusionMatrix(p2, test$sex)

p3 = predict(model1, val)
confusionMatrix(p3, val$sex)

# 
varImpPlot(model1)


# Model two 

dim(train)

# tuning 
t2 = tuneRF(train[,-15], train[,15],
           stepFactor = .5,
           plot = T,
           ntreeTry = 500, 
           trace = T,
           improve = 1)

model2 = randomForest(train$state~., 
                      data = train, mtry = 3, 
                      ntree=250, importance = T)

print(model2)

# Error 

plot(model2,
     main = "Model Two Error")

# Prediction 

a1 = predict(model2, train)
confusionMatrix(p1, train$sex)

a2 = predict(model2, test)
confusionMatrix(p2, test$sex)

a3 = predict(model2, val)
confusionMatrix(p3, val$sex)

# Var importance 

varImpPlot(model2)


ggplot(newDATA)+aes(newDATA$birth_year, fill = newDATA$state)+geom_bar()
