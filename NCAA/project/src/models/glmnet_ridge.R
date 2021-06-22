#- start fresh
rm(list = ls())
#- load in libraries
install.packages("glmnet", dependencies=TRUE)
library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)



#- read in data
train <- fread("project/volume/data/interim/train.csv")
test <- fread("project/volume/data/interim/test.csv")


#----------------------------------#
#      Prep Data for Modeling      #
#----------------------------------#



#- subset out only the columns to model


#- save the response var because dummyVars will remove
train_y <- train$Result
# 
# #- needed for dummyVars
test$Result <- 0
team1<- test$team_1
team2<- test$team_2


#- work with dummies
dummies <- dummyVars(Result ~ ., data = train)

train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)

train <- data.table(train)
test <- data.table(test)


#----------------------------------#
#       Use cv to find lambda      #
#----------------------------------#
#- glmnet requires matrix format
train <- as.matrix(train)
test <- as.matrix(test)

gl_model <- glmnet::cv.glmnet(x=train, y=train_y, alpha = 0, family="binomial")
bestlam <- gl_model$lambda.min
#----------------------------------#
# fit the model to all of the data #
#----------------------------------#
gl_model <- glmnet(train, train_y, alpha = 0, family="binomial")
#-create predictions

pred <- predict(gl_model, s = bestlam, newx = test, type ='response')

season<-2021
submit <- pred
submit<-data.table(submit)
submit$id <- paste(season,team1, team2, sep = "_")
colnames(submit)<- c("Pred", "id")


fwrite(submit,"project/volume/data/processed/midterm_sub.csv")








