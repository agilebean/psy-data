library(tidyverse)
library(caret)
library(recipes)

trainData <- data.frame(v1 = rnorm(15,3,1), v2 = rnorm(15,2,2))
testData <- data.frame(v1 = rnorm(5,3,1), v2 = rnorm(5,2,2))
normParam <- preProcess(trainData, c("center")) %T>% print
norm.testData <- predict(normParam, testData)
testData
trainData %>% summary
norm.trainData <- predict(normParam, trainData)
norm.trainData %>% summary
norm.testData %>% summary

trainData
rec <- recipe(v2~ v1, data = trainData) %>%
  step_center(all_numeric()) %>%
  step_nzv(all_numeric())
rec

trained <- prep(rec, training = trainData, verbose = TRUE, retain = TRUE) %T>% print
trained$template # training data processed
trainData


model.svm <- train(rec, data = trainData, method = "svmRadial")
model.svm %>% plot

model.knn <- train(rec, data = trainData, method = "knn")
model.knn %>% plot

model.knn %>% predict(testData)
model.knn %>% predict(norm.testData)
