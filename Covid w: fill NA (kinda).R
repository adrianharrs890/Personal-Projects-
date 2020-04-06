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

# Haven't run the code yet on this script
# Just copied and paste from (Covid w/ NA)
# So look at the first before looking at this one

# Data
patient2 <- read_csv("Desktop/patient.csv")

patient2$sex = as.factor(patient2$sex)
patient2$country = as.factor(patient2$country)
patient2$region = as.factor(patient2$region)
patient2$disease = as.factor(patient2$disease)
patient2$group = as.factor(patient2$group)
patient2$infection_reason = as.factor(patient2$infection_reason)
patient2$infection_order = as.factor(patient2$infection_order)
patient2$infected_by = as.factor(patient2$infected_by)
patient2$confirmed_date = as.factor(patient2$confirmed_date)
patient2$released_date = as.factor(patient2$released_date)
patient2$deceased_date = as.factor(patient2$deceased_date)
patient2[patient2$state == "isolated",]$state = 0 
patient2[patient2$state == "released",]$state = 1
patient2[patient2$state == "deceased",]$state = 2
patient2$state = as.factor(patient2$state)

impute <- mice(patient2[,-1], m=5, seed = 123,nnet.MaxNWts = 5000)
newDATA <- complete(impute, 1)
newDATA[is.na(newDATA)] <- 0
View(newDATA)

# Randomn Forest 

set.seed(300)
sample2 = sample(3, nrow(newDATA), replace = T, prob = c(.6,.2,.2))
train2 = newDATA[sample2 ==1,]
test2 = newDATA[sample2 ==2,]
val2 = newDATA[sample2 ==3,]

# Tuning 
head(train2)

train2 <- as.data.frame(train2)

tuneRF(train2[,-2], train2[,2],
           stepFactor = .5,
           plot = T,
           ntreeTry = 500, 
           trace = T,
           improve = .05)

# Model prediting sex 
newmodel1 = randomForest(train$sex~., data = train, mtry = 3, importance = T)
print(newmodel1)

# Error 
plot(newmodel1, 
     main = "Model One Error")


# Predictions 
pp1 = predict(newmodel1, train2)
confusionMatrix(pp1, train2$sex)

pp2 = predict(model1, test2)
confusionMatrix(p2, test$sex)

pp3 = predict(model1, val2)
confusionMatrix(p3, val$sex)

# Variable importance 
varImpPlot(model1)





# Model two predicting state (recoverd, isolated, dead)

#Tuning - still run code 
tuneRF(train2[,-15], train2[,15],
            stepFactor = .5,
            plot = T,
            ntreeTry = 500, 
            trace = T,
            improve = 1)
# Model 
newmodel2 = randomForest(train2$state~., 
                      data = train, mtry = 3, 
                      ntree=250, importance = T)


print(newmodel2)

# Error 

plot(newmodel2,
     main = "Model Two Error")

# Predictions

aa1 = predict(model2, train)
confusionMatrix(p1, train$sex)

aa2 = predict(model2, test)
confusionMatrix(p2, test$sex)

aa3 = predict(model2, val)
confusionMatrix(p3, val$sex)


# Var importance 

varImpPlot(newmodel2)

