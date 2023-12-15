## Question 6 - Problem 9
## (a) Produce a scatterplot matrix which includes all of the variables
## in the data set.

library(ISLR)
data(Auto)

pairs(Auto)

## (b) Compute the matrix of correlations between the variables using
## the function cor(). You will need to exclude the name variable, cor() which is qualitative.

cor_matrix <- cor(Auto[, -which(names(Auto) == "name")])

print(cor_matrix)

## (c) Use the lm() function to perform a multiple linear regression
## with mpg as the response and all other variables except name as
## the predictors. Use the summary() function to print the results.
## Comment on the output. For instance:
##  i. Is there a relationship between the predictors and the response?
##  ii. Which predictors appear to have a statistically significant
##      relationship to the response?
##  iii. What does the coefficient for the year variable suggest?

fit <- lm(mpg ~ . - name, data = Auto)

summary(fit)

## (d) Use the plot() function to produce diagnostic plots of the linear
## regression fit. Comment on any problems you see with the fit.
## Do the residual plots suggest any unusually large outliers? Does
## the leverage p

plot(fit)

## (e) Use the * and : symbols to fit linear regression models with
## interaction effects. Do any interactions appear to be statistically
## significant?

## using *
fit_interaction_star <- lm(mpg ~ . - name + horsepower * weight, data = Auto)

summary(fit_interaction_star)

## using :
fit_interaction_colon <- lm(mpg ~ . - name + horsepower:weight, data = Auto)

summary(fit_interaction_colon)

## using multiple interaction effects

fit_multiple_interactions <- lm(mpg ~ . - name + horsepower:weight + displacement:cylinders, data = Auto)

summary(fit_multiple_interactions)

## (f) Try a few different transformations of the variables, such as
## log(X), √X, X2. Comment on your findings.

## log transformation
fit_log <- lm(mpg ~ . - name + log(horsepower), data = Auto)

summary(fit_log)

## square root transformation
fit_sqrt <- lm(mpg ~ . - name + sqrt(displacement), data = Auto)

summary(fit_sqrt)

## squared transformation

fit_square <- lm(mpg ~ . - name + I(weight^2), data = Auto)

summary(fit_square)

