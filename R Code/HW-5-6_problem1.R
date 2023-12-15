# Chapter 5 Question 6

# a)Using the summary() and glm() functions, determine the estimated standard 
# errors for the coefficients associated with income and balance in a multiple 
# logistic regression model that usesboth predictors

library(ISLR)
library(boot)

data("Default")
set.seed(6115)
fit <- glm(default ~ income + balance, data=Default, family=binomial)
summary_fit <- summary(fit)
std_errors <- summary_fit$coefficients[, "Std. Error"]

# b) Write a function, boot.fn(), that takes as input the Default data set as 
# well as an index of the observations, and that outputs the coefficient estimates 
# for income and balance in the multiple logistic regression model.

boot.fn <- function(data, index) {
  fit <- glm(default ~ income + balance, data=data, subset=index, family=binomial)
  return(coef(fit)[c('income', 'balance')])
}

# c) Use the boot() function together with your boot.fn() function to 
# estimate the standard errors of the logistic regression coefficients for income 
# and balance.
boot.fn <- function(data, index) {
  fit <- glm(default ~ income + balance, data=data, subset=index, family=binomial)
  return(coef(fit)[c('income', 'balance')])
}

set.seed(6115)
boot_results <- boot(Default, boot.fn, R=1000)

boot_se <- sqrt(diag(var(boot_results$t)))

# d) Comment on the estimated standard errors obtained using the glm() 
# function and using your bootstrap function.
