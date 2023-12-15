## Question 3 - Problem 7 Chapter 2
## Part a)
library(dplyr)
training_data <- data.frame(
  Obs = 1:6,
  X1 = c(0, 2, 0, 0, -1, 1),
  X2 = c(3, 0, 1, 1, 0, 1),
  X3 = c(0, 0, 3, 2, 1, 1),
  Y = c("Red", "Red", "Red", "Green", "Green", "Red")
)
test_point <- data.frame(X1 = 0, X2 = 0, X3 = 0)
euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}
distances <- apply(training_data[, c("X1", "X2", "X3")], 1, function(row) 
{
  euclidean_distance(row, test_point)
})
training_data <- training_data %>%
  mutate(Distance = distances)
print(training_data)

## Part b)
training_data <- data.frame(
  Obs = 1:6,
  X1 = c(0, 2, 0, 0, -1, 1),
  X2 = c(3, 0, 1, 1, 0, 1),
  X3 = c(0, 0, 3, 2, 1, 1),
  Y = c("Red", "Red", "Red", "Green", "Green", "Red")
)

test_point <- c(X1 = 0, X2 = 0, X3 = 0)

euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}

distances <- apply(training_data[, c("X1", "X2", "X3")], 1, function(row) {
  euclidean_distance(row, test_point)
})

training_data$Distance <- distances

nearest_obs <- training_data[which.min(training_data$Distance),]
prediction <- nearest_obs$Y
print(paste("The prediction with K=1 is:", prediction)) 

## Part c)
training_data <- data.frame(
  Obs = 1:6,
  X1 = c(0, 2, 0, 0, -1, 1),
  X2 = c(3, 0, 1, 1, 0, 1),
  X3 = c(0, 0, 3, 2, 1, 1),
  Y = c("Red", "Red", "Red", "Green", "Green", "Red")
)

test_point <- c(X1 = 0, X2 = 0, X3 = 0)

euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}

distances <- apply(training_data[, c("X1", "X2", "X3")], 1, function(row) {
  euclidean_distance(row, test_point)
})

training_data$Distance <- distances

top_k <- training_data[order(training_data$Distance), ][1:3,]
prediction <- as.character(names(sort(table(top_k$Y), decreasing = TRUE)[1]))
print("Top 3 neighbors are:")
print(top_k)
print(paste("The prediction with K=3 is:", prediction))
