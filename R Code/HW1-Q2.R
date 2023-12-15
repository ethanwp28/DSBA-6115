library(ISLR)
library(ggplot2)

data = read.csv('C:\\Users\\28epi\\Documents\\DSBA 6115\\Advertising.csv', head=T)

y = data$Sales
x1 = data$TV
x2 = data$Radio
x3 = data$Newspaper

# Null Model
null_model = lm(Sales~1, data=data)
null_rss = sum(residuals(null_model)^2)

# Model with one variable
mod_1 = lm(y ~ x1)
rss_1 = sum(residuals(mod_1)^2)

# Model with two variables
mod_2 = lm(y ~ x1 + x2)
rss_2 = sum(residuals(mod_2)^2)

# Model with three variables
mod_3 = lm(y ~ x1 + x2 + x3)
rss_3 = sum(residuals(mod_3)^2)

# Plot
rss_data = data.frame(
  Variables = c("Null Model", "1 Variable", "2 Variables", "3 Variables"),
  RSS = c(null_rss, rss_1, rss_2, rss_3)
)

ggplot(rss_data, aes(x = Variables, y = RSS)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  geom_text(aes(label = round(RSS, 2)), vjust = -0.3) +
  ggtitle("RSS vs Number of Variables") +
  xlab("Number of Variables") +
  ylab("RSS")


















