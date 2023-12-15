load("faces.rdata")

# install.packages("pls")
# install.packages("rda")
# install.packages("klaR")

library(pls)
library(MASS)
library(sda)
library(rda)
library(klaR)

## a)

reshape_portrait <- function(vector) {
  matrix(vector, nrow = 100, ncol = 100)
}

mean_male_portrait <- apply(faces[, 1:100], 1, mean)
mean_female_portrait <- apply(faces[, 101:200], 1, mean)

mean_male_image <- reshape_portrait(mean_male_portrait)
mean_female_image <- reshape_portrait(mean_female_portrait)

image(1:100, 1:100, mean_male_image[, 100:1], col = grey(0:255/255), main = "Mean Male Portrait")
image(1:100, 1:100, mean_female_image[, 100:1], col = grey(0:255/255), main = "Mean Female Portrait")

## b)
pca <- prcomp(faces, scale. = FALSE)
eigenvectors <- pca$x[, 1:3]


eigenmatrix1 <- matrix(eigenvectors[,1],nrow=100, ncol=100)

eigenmatrix2 <- matrix(eigenvectors[,2],nrow=100, ncol=100)

eigenmatrix3 <- matrix(eigenvectors[,3],nrow=100, ncol=100)

image(eigenmatrix1, col = gray.colors(256))
image(eigenmatrix2, col = gray.colors(256))
image(eigenmatrix3, col = gray.colors(256))

summary(pca)

## c)
pc1 <- pca$x[, 1]
pc2 <- pca$x[, 2]
pc3 <- pca$x[, 3]

gender <- c(rep("Male", 100), rep("Female", 100))


plot(pc1, pc2, col = ifelse(gender == "Male", "blue", "red"), xlab = "PC1", ylab = "PC2", main = "PC1 vs PC2 by Gender")
plot(pc1, pc3, col = ifelse(gender == "Male", "blue", "red"), xlab = "PC1", ylab = "PC3", main = "PC1 vs PC3 by Gender")

plot(pc1, pc2, col = ifelse(shoulder, "green", "purple"), xlab = "PC1", ylab = "PC2", main = "PC1 vs PC2 by Shoulders")
plot(pc1, pc3, col = ifelse(shoulder, "green", "purple"), xlab = "PC1", ylab = "PC3", main = "PC1 vs PC3 by Shoulders")

## d)
y <- ifelse(gender == "Male", -1, 1)

faces_df <- data.frame(faces)
faces_df <- cbind(y = y, faces_df)

set.seed(6115) 
pcr_model <- pcr(y ~ ., data = faces_df, scale = TRUE, validation = "LOO")

validation_stats <- summary(pcr_model)
opt_m <- which.min(validation_stats$val$MSEP)

predictions <- predict(pcr_model, ncomp = opt_m)
predicted_class <- ifelse(predictions > 0, 1, -1)
misclassification_rate <- mean(predicted_class != y)

print(misclassification_rate)

## e)
set.seed(6115)
pls_model <- plsr(y ~ ., data = faces_df, scale = TRUE, validation = "LOO")

validation_stats_pls <- summary(pls_model)
opt_m_pls <- which.min(validation_stats_pls$val$MSEP)

predictions_pls <- predict(pls_model, ncomp = opt_m_pls)
predicted_class_pls <- ifelse(predictions_pls > 0, 1, -1)
misclassification_rate_pls <- mean(predicted_class_pls != y)

print(misclassification_rate_pls)

## f)
faces_reduced <- as.data.frame(pca$rotation[, 1:5])

y_factor <- as.factor(ifelse(1:200 <= 100, "Male", "Female"))
qda_model <- qda(x = faces_reduced, grouping = y_factor)

qda_predictions <- predict(qda_model, faces_reduced)
misclassification_rate_qda <- mean(qda_predictions$class != y_factor)

print(misclassification_rate_qda)

mean_pc3 <- mean(faces_reduced[, 3])
mean_pc4 <- mean(faces_reduced[, 4])
mean_pc5 <- mean(faces_reduced[, 5])

xrange <- range(faces_reduced[, 1])
yrange <- range(faces_reduced[, 2])

grid <- expand.grid(PC1 = seq(from = xrange[1], to = xrange[2], length = 100),
                    PC2 = seq(from = yrange[1], to = yrange[2], length = 100),
                    PC3 = mean_pc3,
                    PC4 = mean_pc4,
                    PC5 = mean_pc5)

grid$predicted_class <- predict(qda_model, newdata = grid)$class

plot(faces_reduced[, 1], faces_reduced[, 2], col = as.numeric(y_factor), xlab = "PC1", ylab = "PC2", pch = 19)
plot(faces_reduced[, 1], faces_reduced[, 3], col = as.numeric(y_factor), xlab = "PC1", ylab = "PC3", pch = 19)

contour(xrange, yrange, matrix(as.numeric(grid$predicted_class), length(xrange), length(yrange)), add = TRUE)

## g)
gender_shoulder <- ifelse(y == -1, ifelse(shoulder, "maleShoulder", "maleNoShoulder"),
                          ifelse(shoulder, "femaleShoulder", "femaleNoShoulder"))

qda_model_gs <- qda(x = faces_reduced, grouping = as.factor(gender_shoulder))
qda_predictions_gs <- predict(qda_model_gs, faces_reduced)
misclassification_rate_gs <- mean(qda_predictions_gs$class != gender_shoulder)
print(misclassification_rate_gs)

## h)
lda_data <- as.data.frame(faces_pca)
lda_data$y_factor <- y_factor

lda_model <- lda(y_factor ~ ., data = lda_data)

lda_predictions <- predict(lda_model, newdata = lda_data)
predicted_gender <- as.numeric(lda_predictions$class)
misclassification_rate_lda <- mean(predicted_gender != as.numeric(lda_data$y_factor))

print(misclassification_rate_lda)

## Test alternative method
rda_model <- rda(x=data.matrix(faces),y=y)
rda_predictions <- predict(rda_model, x=data.matrix(faces),y=y,xnew=data.matrix(faces),alpha=0, delta=0.667)

misclassification_rate_rda <- mean(rda_predictions != y)

print(misclassification_rate_rda)