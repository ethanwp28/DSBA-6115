library(ISLR)
library(splines)

data(Wage)
dim(Wage)
names(Wage)
attach(Wage)
W1=summary(Wage)

par(mfrow = c(1, 3))

agelims=range(age)
age.grid=seq(from=agelims[1], to=agelims[2])

# use the dataset to draw the graph 
# Fit a natural spline with 5 degrees of freedom with function "ns" #
fit2 = lm(wage~ns(age, df=5), data=Wage)
pred=predict(fit2, newdata=list(age=age.grid), se=T)

plot(x=age,y=wage,col="gray")
title("Natural Cubic Splines")
lines(age.grid,pred$fit,lwd=2, col="blue")
lines(age.grid,pred$fit+2*pred$se, lty="dashed")
lines(age.grid,pred$fit-2*pred$se, lty="dashed")

#Plot for Wage vs. year #
yearlims=range(year)
year.grid=seq(from=yearlims[1], to=yearlims[2])
fit3 = lm(wage ~ year, data=Wage)
pred2=predict(fit3, newdata=list(year=year.grid), se=T)

plot(x=year,y=wage,col="gray")
title("Wage vs Year")
lines(year.grid,pred2$fit,lwd=2, col="blue")


#Plot for Wage vs. education level #
boxplot(wage ~ education, data = Wage, col = c("cornflowerblue", "green", "yellow", "blue", "orange"))
title("Wage vs Education Level")
par(mfrow = c(1, 1))





