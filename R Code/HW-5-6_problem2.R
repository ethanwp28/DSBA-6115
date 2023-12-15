## Chapter 6, Question 9
## a) Split the data set into a training set and a test set.


library(glmnet)
library(ISLR)
library(pls)

data("College")

set.seed(6115)
index <- sample(1:nrow(College), nrow(College) * 0.7)

training_set <- College[index, ]
test_set <- College[-index, ]

## b) Fit a linear model using least squares on the training set, and report the
## test error obtained.
lm_model <- lm(Apps ~ ., data=training_set)

predictions <- predict(lm_model, newdata=test_set)

mse <- mean((test_set$Apps - predictions)^2)

## c) Fit a ridge regression model on the training set, with λ chosen by 
## cross-validation. Report the test error obtained.
## testing and converting data if needed:
# Check for non-numeric columns
training_set$Private <- as.numeric(training_set$Private == "Yes")  # Replace "Yes" with the appropriate level
test_set$Private <- as.numeric(test_set$Private == "Yes")          # Do the same for test_set
sapply(training_set, class)
sapply(test_set, class)


x_train <- as.matrix(training_set[, -which(names(training_set) == "Apps")])
y_train <- training_set$Apps

x_test <- as.matrix(test_set[, -which(names(test_set) == "Apps")])
y_test <- test_set$Apps

set.seed(6115)
cv_ridge <- cv.glmnet(x_train, y_train, alpha=0)

best_lambda <- cv_ridge$lambda.min

ridge_predictions <- predict(cv_ridge, s=best_lambda, newx=x_test)

ridge_mse <- mean((y_test - ridge_predictions)^2)

## d) Fit a lasso model on the training set, with λ chosen by cross validation. 
## Report the test error obtained, along with the number of non-zero coefficient 
## estimates.
x_train <- as.matrix(training_set[, -which(names(training_set) == "Apps")])
y_train <- training_set$Apps

x_test <- as.matrix(test_set[, -which(names(test_set) == "Apps")])
y_test <- test_set$Apps

set.seed(6115)
cv_lasso <- cv.glmnet(x_train, y_train, alpha=1)

best_lambda <- cv_lasso$lambda.min

lasso_predictions <- predict(cv_lasso, s=best_lambda, newx=x_test)

lasso_mse <- mean((y_test - lasso_predictions)^2)

lasso_model <- glmnet(x_train, y_train, alpha=1)
coef_lasso <- predict(lasso_model, type="coefficients", s=best_lambda)
num_nonzero_coefs <- sum(coef_lasso != 0)

## e) Fit a PCR model on the training set, with M chosen by cross validation. 
## Report the test error obtained, along with the value of M selected by 
## cross-validation.
set.seed(6115)

pcr_model <- pcr(Apps ~ ., data=training_set, scale=TRUE, validation="CV")

rmsep_results <- RMSEP(pcr_model)
str(rmsep_results)

M_optimal <- which.min(rmsep_results$RMSEP)

cv_rmsep <- rmsep_results$val[1, 1, ]

M_optimal <- which.min(cv_rmsep)
print(M_optimal)
summary(pcr_model)
M_optimal <- 17

pcr_predictions <- predict(pcr_model, newdata=test_set, ncomp=M_optimal)

pcr_mse <- mean((test_set$Apps - pcr_predictions)^2)



## f) Fit a PLS model on the training set, with M chosen by cross validation. 
## Report the test error obtained, along with the value of M selected by 
## cross-validation
set.seed(6115)
pls_model <- plsr(Apps ~ ., data=training_set, validation="CV")

pls_predictions <- predict(pls_model, newdata=test_set, ncomp=M_optimal)

pls_mse <- mean((test_set$Apps - pls_predictions)^2)

## g) Comment on the results obtained. How accurately can we predict the number 
## of college applications received? Is there much difference among the test errors 
## resulting from these five approaches?
