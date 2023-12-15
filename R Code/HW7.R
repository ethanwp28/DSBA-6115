## install.packages("caret")
## install.packages("rpart")
## install.packages("readr")
## install.packages("rpart.plot")
## install.packages("randomForest")
## install.packages("gbm")
## install.packages("xgboost")

library(ISLR)
library(readr)
library(caret)
library(rpart)
library(randomForest)
library(xgboost)
library(ggplot2)
library(plotly)


trainingData <- read.csv("author_training.csv")
testingData <- read.csv("author_testing.csv")

trainingData$Author <- factor(trainingData$Author)
testingData$Author <- factor(testingData$Author)

## Classification Tree
authorshipModel <- rpart(Author ~ ., data = trainingData, method = "class")

raw_predictions <- predict(authorshipModel, testingData, type = "class")

predictions <- factor(raw_predictions, levels = levels(trainingData$Author))

levels(predictions)

table(predictions)

confusionMatrix(predictions, testingData$Author)

## Bagging
authorshipBaggingModel <- randomForest(Author ~ ., data = trainingData, ntree = 100)
baggingPredictions <- predict(authorshipBaggingModel, testingData)

confusionMatrix(baggingPredictions, testingData$Author)

## Boosting
trainingLabels <- as.numeric(trainingData$Author) - 1
trainingDataMatrix <- xgb.DMatrix(data.matrix(trainingData[, -which(names(trainingData) == "Author")]), label = trainingLabels)

testingLabels <- as.numeric(testingData$Author) - 1
testingDataMatrix <- xgb.DMatrix(data.matrix(testingData[, -which(names(testingData) == "Author")]), label = testingLabels)

params <- list(
  objective = "multi:softprob",
  num_class = length(unique(trainingData$Author)),
  eta = 0.3,
  max_depth = 6
)

nrounds <- 100

xgbModel <- xgb.train(params = params, data = trainingDataMatrix, nrounds = nrounds)

xgbPredictions <- predict(xgbModel, testingDataMatrix)

xgbPredictions <- matrix(xgbPredictions, ncol = length(unique(trainingData$Author)), byrow = TRUE)
predictedLabels <- max.col(xgbPredictions) - 1  # Convert back to original class labels

confusionMatrix(factor(predictedLabels, levels = unique(testingLabels)), factor(testingLabels, levels = unique(testingLabels)))

## Random Forest
authorshipRandomForestModel <- randomForest(Author ~ ., data = trainingData, ntree = 1000)

rfPredictions <- predict(authorshipRandomForestModel, testingData)

confusionMatrix(rfPredictions, testingData$Author)

## Plot of Word Importance
importance_matrix <- importance(authorshipRandomForestModel)

importance_df <- as.data.frame(importance_matrix)

importance_df$Word = rownames(importance_df)

top_n <- 20  

top_importance_df <- head(importance_df[order(-importance_df$MeanDecreaseGini), ], top_n)

# Graph the top 20 most important words

ggplot(top_importance_df, aes(x = reorder(Word, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(x = "Word", y = "Importance (Mean Decrease in Gini)", title = "Top Word Importance for Authorship Attribution")