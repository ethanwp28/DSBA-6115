## Question 7 - Problem 13

set.seed(1)

## (a) Using the rnorm() function, create a vector, x, containing 100
## observations drawn from a N(0, 1) distribution. This represents
## a feature, X.

x <- rnorm(100, mean = 0, sd = 1)

head(x) ## for verification

## (b) Using the rnorm() function, create a vector, eps, containing 100
## observations drawn from a N(0, 0.25) distribution—a normal
## distribution with mean zero and variance 0.25.

eps <- rnorm(100, mean = 0, sd = sqrt(0.25))

head(eps)

## (c) Using x and eps, generate a vector y according to the model
## Y = −1+0.5X + ϵ. (3.39)
## What is the length of the vector y? What are the values of β0
## and β1 in this linear model?

y <- -1 + 0.5 * x + eps

head(y)

length_y <- length(y)

print(paste("The length of y is:", length_y))

## (d) Create a scatterplot displaying the relationship between x and
## y. Comment on what you observe.

plot(x, y, main = "Scatterplot of x vs y",
     xlab = "x", ylab = "y", pch = 19, col = "blue")

## (e) Fit a least squares linear model to predict y using x. Comment
## on the model obtained. How do βˆ0 and βˆ1 compare to β0 and
## β1?

fit_model <- lm(y ~ x)

summary(fit_model)

## (f) Display the least squares line on the scatterplot obtained in (d).
## Draw the population regression line on the plot, in a different
## color. Use the legend() command to create an appropriate legend.

plot(x, y, main = "Scatterplot with Least Squares & Population Regression Lines",
     xlab = "x", ylab = "y", pch = 19, col = "blue")

abline(fit_model, col = "red", lwd = 2)

abline(a = -1, b = 0.5, col = "green", lwd = 2)

legend("topright", legend = c("Least Squares Line", "Population Line"),
       col = c("red", "green"), lwd = 2)

## (g) Now fit a polynomial regression model that predicts y using x
## and x2. Is there evidence that the quadratic term improves the
## model fit? Explain your answer.

poly_model <- lm(y ~ x + I(x^2))

summary(poly_model)

## (h) Repeat (a)–(f) after modifying the data generation process in
## such a way that there is less noise in the data. The model (3.39)
## should remain the same. You can do this by decreasing the variance of the normal distribution used to generate the error term
## ϵ in (b). Describe your results.
set.seed(1)

x <- rnorm(100)

eps <- rnorm(100, mean = 0, sd = 0.1) ## decreased standard deviation to 0.1

y <- -1 + 0.5*x + eps

length(y)

plot(x, y, main = "Scatterplot with Less Noise",
     xlab = "x", ylab = "y", pch = 19, col = "blue")

fit_model_less_noise <- lm(y ~ x)
summary(fit_model_less_noise)

abline(fit_model_less_noise, col = "red", lwd = 2)
abline(a = -1, b = 0.5, col = "green", lwd = 2)

legend("topright", legend = c("Least Squares Line", "True Line"),
       col = c("red", "green"), lwd = 2)

poly_model_less_noise <- lm(y ~ x + I(x^2))
summary(poly_model_less_noise)

## (i) Repeat (a)–(f) after modifying the data generation process in
## such a way that there is more noise in the data. The model
## (3.39) should remain the same. You can do this by increasing
## the variance of the normal distribution used to generate the
## error term ϵ in (b). Describe your results.

x <- rnorm(100)

eps <- rnorm(100, mean = 0, sd = 0.5)  ## increased standard deviation to 0.5

y <- -1 + 0.5*x + eps

length(y)

plot(x, y, main = "Scatterplot with More Noise",
     xlab = "x", ylab = "y", pch = 19, col = "blue")

fit_model_more_noise <- lm(y ~ x)
summary(fit_model_more_noise)

abline(fit_model_more_noise, col = "red", lwd = 2)

abline(a = -1, b = 0.5, col = "green", lwd = 2)

legend("topright", legend = c("Least Squares Line", "True Line"),
       col = c("red", "green"), lwd = 2)

poly_model_more_noise <- lm(y ~ x + I(x^2))
summary(poly_model_more_noise)

## (j) What are the confidence intervals for β0 and β1 based on the
## original data set, the noisier dataset, and the less noisy data
## set? Comment on your results.

## original data set
x_original <- rnorm(100)
eps_original <- rnorm(100, 0, 0.25)
y_original <- -1 + 0.5*x_original + eps_original

fit_original <- lm(y_original ~ x_original)

## noisier data set

x_noisy <- rnorm(100)
eps_noisy <- rnorm(100, 0, 0.5)
y_noisy <- -1 + 0.5*x_noisy + eps_noisy

fit_noisy <- lm(y_noisy ~ x_noisy)

## less noisy data set

x_less_noisy <- rnorm(100)
eps_less_noisy <- rnorm(100, 0, 0.1)
y_less_noisy <- -1 + 0.5*x_less_noisy + eps_less_noisy

fit_less_noisy <- lm(y_less_noisy ~ x_less_noisy)

## confidence intervals

confint(fit_original)
confint(fit_noisy)
confint(fit_less_noisy)
