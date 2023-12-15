# install.packages("e1071")

library(e1071)
library(rpart)
library(rpart.plot)
library(caret)

set.seed(6115)

# Prepare data
data <- data.frame(
  Day = 1:14,
  Thunder = c('Overcast', 'Sunny', 'Sunny', 'Overcast', 'Sunny', 'Sunny', 'Overcast', 'Sunny', 'Overcast', 'Overcast', 'Overcast', 'Sunny', 'Overcast', 'Sunny'),
  Hailstorm = c('Cool', 'Hot', 'Hot', 'Cool', 'Hot', 'Cool', 'Cool', 'Hot', 'Hot', 'Hot', 'Cool', 'Hot', 'Hot', 'Cool'),
  Homework = c('High', 'High', 'Normal', 'High', 'Normal', 'High', 'Normal', 'Normal', 'Normal', 'High', 'Normal', 'High', 'High', 'High'),
  Tsunami = c('Weak', 'Strong', 'Strong', 'Weak', 'Strong', 'Strong', 'Weak', 'Strong', 'Weak', 'Strong', 'Strong', 'Weak', 'Strong', 'Weak'),
  Beach = c('Yes', 'No', 'Yes', 'Yes', 'No', 'Yes', 'Yes', 'No', 'Yes', 'Yes', 'No', 'No', 'Yes', 'No')
)

data$Thunder <- as.factor(data$Thunder)
data$Hailstorm <- as.factor(data$Hailstorm)
data$Homework <- as.factor(data$Homework)
data$Tsunami <- as.factor(data$Tsunami)
data$Beach <- as.factor(data$Beach)

fold1 <- data[data$Day %% 2 == 1, ]
fold2 <- data[data$Day %% 2 == 0, ]

m_estimate <- function(n, N, m = 0.001, p = 0.5) {
  return((n + m * p) / (N + m))
}

nb_accuracies <- c()

for(i in 1:2) {
  train_data <- if(i == 1) fold2 else fold1
  test_data <- if(i == 1) fold1 else fold2
  
  nb_model <- naiveBayes(Beach ~ ., data=train_data[, -1], laplace=0.001)
  nb_model$apriori <- m_estimate(table(train_data$Beach), nrow(train_data))
  
  print(paste("Naive Bayes Model for Fold", i))
  print(nb_model)
  
  nb_predictions <- predict(nb_model, test_data[, -1])
  
  nb_accuracy <- sum(nb_predictions == test_data$Beach) / length(test_data$Beach)
  nb_accuracies[i] <- nb_accuracy
  
  print(paste("Naive Bayes Predictions for Fold", i))
  print(nb_predictions)
  print(paste("Naive Bayes Accuracy for Fold", i, ":", nb_accuracy))
}

print(paste("Average Naive Bayes 2-fold CV Accuracy:", mean(nb_accuracies)))

dt_accuracies <- c()

for(i in 1:2) {
  train_data <- if(i == 1) fold2 else fold1
  test_data <- if(i == 1) fold1 else fold2
  
  dt_model <- rpart(Beach ~ ., data=train_data[, -1], method="class", control=rpart.control(cp=0.001))
  
  print(paste("Decision Tree Model for Fold", i))
  print(dt_model)
  
  dt_predictions <- predict(dt_model, test_data[, -1], type="class")
  
  dt_accuracy <- sum(dt_predictions == test_data$Beach) / length(test_data$Beach)
  dt_accuracies[i] <- dt_accuracy
  
  print(paste("Decision Tree Predictions for Fold", i))
  print(dt_predictions)
  print(paste("Decision Tree Accuracy for Fold", i, ":", dt_accuracy))
}

print(paste("Average Decision Tree 2-fold CV Accuracy:", mean(dt_accuracies)))

full_nb_model <- naiveBayes(Beach~.-Day,data=data, laplace=0.001)
print(full_nb_model)

full_nb_predictions <- predict(full_nb_model, data, type="class")

conf_matrix <- table(Predicted = full_nb_predictions, Actual = data$Beach)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy of Naive Bayes Model on Full Data: ", accuracy, "\n")