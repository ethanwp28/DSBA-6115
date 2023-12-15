# Chapter 6, Question 11
# a) Try out some of the regression methods explored in this chapter, such as 
# best subset selection, the lasso, ridge regression, and PCR. Present and discuss 
# results for the approaches that you consider.
library(MASS) 
library(leaps)
library(glmnet)
library(pls)
data(Boston)

# Best subset selection
best_subset_fit <- regsubsets(crim ~ ., data=Boston, nvmax=dim(Boston)[2]-1)
summary_best_subset <- summary(best_subset_fit)

# Prepare data
x <- as.matrix(Boston[, -which(names(Boston) == "crim")])
y <- Boston$crim

# Lasso regression
cv_lasso <- cv.glmnet(x, y, alpha=1)
lasso_best_lambda <- cv_lasso$lambda.min
lasso_model <- glmnet(x, y, alpha=1, lambda=lasso_best_lambda)

# Ridge regression
cv_ridge <- cv.glmnet(x, y, alpha=0)
ridge_best_lambda <- cv_ridge$lambda.min
ridge_model <- glmnet(x, y, alpha=0, lambda=ridge_best_lambda)

# PCR
pcr_model <- pcr(crim ~ ., data=Boston, scale=TRUE, validation="CV")

# Calculate RMSEP and Determine Optimal Number of Components
rmsep_results <- RMSEP(pcr_model)
cv_rmsep <- rmsep_results$val[1, 1, ]
M_optimal <- which.min(cv_rmsep)
print(M_optimal)

# Ensure M_optimal is Within Valid Range
summary(pcr_model)
M_optimal <- min(M_optimal, 13)

# Predict Using PCR Model
pcr_pred <- predict(pcr_model, newdata=Boston, ncomp=M_optimal)


# b) Propose a model (or set of models) that seem to perform well on this data 
# set, and justify your answer. Make sure that you are evaluating model performance 
# using validation set error, cross validation, or some other reasonable 
# alternative, as opposed to using training error.



# c) Does your chosen model involve all of the features in the data set? Why 
# or why not?
