set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)
plot(x,y)
mydata = data.frame(x,y)

set.seed(3)
fold = sample(rep(1:5,20))
mse = rep(0,4)

for (degree in 1:4){
  yhat = rep(0,100)
  for (i in 1:5){
    train = (1:100)[fold != i]
    lm1_fit = lm(y ~ poly(x, degree), data = mydata, subset = train)
    yhat[-train] = predict(lm1_fit, mydata[-train, ])
  }
  mse[degree] = mean((yhat - y)^2)
  cat("Degree:", degree, "MSE:", mse[degree], "\n")  # Print the MSE for each degree
}

plot(1:4, mse)

set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)
mydata = data.frame(x, y)

full.model = lm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = mydata)

selected.model = step(full.model, direction = "both")

summary(selected.model)

set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

mydata = data.frame(x, y)

model1 = lm(y ~ x, data = mydata)
model2 = lm(y ~ x + I(x^2), data = mydata)
model3 = lm(y ~ x + I(x^2) + I(x^3), data = mydata)
model4 = lm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = mydata)

anova(model1, model2)
anova(model2, model3)
anova(model3, model4)