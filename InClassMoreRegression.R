#Libraries
install.packages("car")
install.packages("sandwich")
install.packages("lmtest")
library(ggplot2)
library(car)
library(lmtest)
library(sandwich)
library(dplyr)

#Load data
data(mtcars)

plot(density(mtcars$mpg))

plot(density(log(mtcars$mpg)))

View(mtcars)

#Convert categorical variables
mtcars$am <- as.factor(mtcars$am) #transmission (0=automatic, 1=manual)
mtcars$cyl <- as.factor(mtcars$cyl) #cylinders as categorical 

#Linear regression model
linear_model <- lm(mpg ~ wt + hp + am, data = mtcars)
summary(linear_model)

#Testing assumptions for normality

#1.Normality of residuals (Shapiro-Wilk test and QQ plot)
shapiro.test(residuals(linear_model))
qqnorm(residuals(linear_model))
qqline(residuals(linear_model), col="red")

#2. Heteroscedasticity test (Breush-Pagan test)
bptest(linear_model)

#3. Linearitty check (residuals vs fitted)
plot(fitted(linear_model), residuals(linear_model),
     main= "Residuals vs Fitted",
     xlab = "Fitted vales", ylab = "Residuals")
abline(h=0, col = "red")

#4. Independence of Errors (Durbin-Watson test)
dwtest(linear_model)

# --- Alternative Models ---

#1. Log-Trasnformed model
log_model <- lm(log(mpg) - log(wt) + log(hp) + am, data = mtcars)
summary(log_model)

#2. Polynomial Regression Model (2nd Degree)
poly_model <- lm(mpg ~ poly(wt, 2) + poly(hp, 2) + am, data = mtcars)
summary(poly_model)

#3. Model with categorical varaibles
cat_model <- lm(mpg ~ wt + hp + am + cyl, data = mtcars)
summary(cat_model)
