#Load libraries
library(ggplot2)
library(tidyverse)
library(dplyr)

#Plot Rent against each of the three predictor variables and evaluate whether the relationship is best captured by a line or a curve. 
#Identify variables that may benefit from a log-transformation.
view(AnnArbor)
ggplot(AnnArbor, aes(x = Beds, y = Rent)) +
  geom_point(color = "blue") +
  labs(title = "Beds vs Rent", x = "Beds", y = "Rent") +
  theme_minimal()
#This relationship would best be captured with a line. There is a positive correlation between number of beds and rent,
#as the number of bedrooms increase so does the cost of rent. This variable would benefit from a log-transformation, but 
#not as much as the Baths varaible would. 

ggplot(AnnArbor, aes(x = Baths, y = Rent)) +
  geom_point(color = "hotpink") +
  labs(title = "Baths vs Rent", x = "Baths", y = "Rent") +
  theme_minimal()
#This relationship would best be captured with a line. There is a positive correlation between number of baths and rent,
#as the number of bathrooms increase so does the cost of rent. 
#This variable would benefit most out of a log-transformation because there are a few outputs that are either very far above or below
#the line of best fit (outliers) this would allow greater linearity, and stabilize variance in the data. 

ggplot(AnnArbor, aes(x = Sqft, y = Rent)) +
  geom_point(color = "purple") +
  labs(title = "Sqft vs Rent", x = "Sqft", y = "Rent") +
  theme_minimal()
#This relationship would best be captured with a line.  There is a positive correlation between the amount of sqaure feet and rent,
#as the property gets bigger, the rent also will increase. 


#Estimate a multiple regression model (with any appropriate log-transformations) to predict rent for a 
#1,600-square-foot rental with 3 bedrooms and 2 bathrooms.

multiple_regression_model <- lm(Rent ~ Beds + Baths + Sqft, data = AnnArbor)
summary(multiple_regression_model)
coef(multiple_regression_model)
#(Intercept)        Beds       Baths        Sqft 
#300.4116035 225.8099834  89.2661343   0.2095727 

Rent = 300.4116035 + (225.8099834*3)+ (89.2661343*2)+(.2095727*1600)

#Rent is $1,49169 for a rental property with 3 beds, 2 bathrooms and 1600 square feet. 

