# --- Simple Linear Regression ---
# Import data
data <- read.csv("brewdog.csv", header=TRUE)

# Plot data
plot(data$ABV, data$Price)
text(data$ABV, data$Price, labels=data$Name, cex=0.7, pos=2)

# Use linear regression
fit <- lm(Price ~ ABV, data=data)

# Add regression line
abline(fit)

# Extract coefficient
coef(fit)

# Show regression output
summary(fit)							

# show fitted values for each data point
fitted(fit)

# show residuals for each data point
residuals(fit)

# Work out the centroid value (mean of both)
mean(data$ABV)
mean(data$Price)

# Plot centroid
points(mean(data$ABV), mean(data$Price), col = 'red')
plot(fit)
anova(fit)

# Histogram of residuals
hist(fit$residuals)						

