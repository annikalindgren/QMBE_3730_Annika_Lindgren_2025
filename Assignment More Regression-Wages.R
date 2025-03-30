#Load libraries
library(tidyverse) 
library(ggplot2)
library(dplyr)

#Plot Wage against Age and evaluate whether a linear or quadratic model would better capture the relationship. 
view(wages)

ggplot(wages, aes(x = Age, y = Wage)) +
  geom_point(color = "blue") +
  labs(title = "Wage vs Age", x = "Age", y = "Wage") +
  theme_minimal()

linear_model <- lm(Wage ~ Age, data = wages)
quadratic_model <- lm(Wage ~ Age + I(Age^2), data = wages)

wages$linear_pred <- predict(linear_model, newdata = wages)
wages$quadratic_pred <- predict(quadratic_model, newdata = wages)

ggplot(wages, aes(x = Age, y = Wage)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_line(aes(y = linear_pred, color = "Linear"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = quadratic_pred, color = "Quadratic"), linewidth = 1) +
  labs(title = "Linear vs Quadratic", x = "Age", y = "Wage") +
  theme_minimal() +
  scale_color_manual(values = c("Linear" = "pink", "Quadratic" = "purple")) +
  theme(legend.title = element_blank())

summary(linear_model)
summary(quadratic_model)

#The quadratic model fits the data best because it has a higher r-squared meaning it captured the relationship better
#and a lower standard error meaning the models predictions are closer to the actual wages. 
#The plot shows that the distribution of age and wage is curved which the quadratic line fits it the best. 


#Estimate a multiple regression model of Wage using Age and Education as independent (X) variables; assume a standard linear relationship between Wage and Age.

multiple_regression_model <- lm(Wage ~ Age + Educ, data = wages)
summary(multiple_regression_model)

#The model's predicted wages vary $4.68 on average from actual wages (Residual Standard Error).
#Education is the most important factor when it comes to wage. For every year of schooling wage increases by 1.44. 
#The model explains 61.87% of wage variation which leaves 38% for outside factors. Since the p-value is < 0.0001 the model is statistically significant.
#Meaning Age and Education successfully can predict Wage.



#Estimate another multiple regression model of Wage using Age and Education as independent (X) variables; this time fit Age using a quadratic relationship. 
#Verify your choice from part a. by comparing the distribution of residuals and the goodness of fit between the models in parts b and c.

quadratic_model <- lm(Wage ~ Age + I(Age^2) + Educ, data = wages)
summary(quadratic_model)

anova(multiple_regression_model, quadratic_model)

#The RSS is the biggest difference dropping from 1685.39 to 741.17 which shows that the quadratic model fits much better.
#The F statistic is large, therefore showing improvement. The Sum of Sq (944.22) shows that the quadratic model explains that much more than the multiple regression model. 
#The P value is also very small meaning that it is significant. Age has a non-linear effect on wage. 


#Use the appropriate model to predict hourly wages for someone with 16 years of education and age equal 30, 50, or 70.

coef(quadratic_model)
#(Intercept)          Age     I(Age^2)         Educ 
#-22.72193555   1.35000183  -0.01332176   1.25395882 

#For 30 years old
#Wage = −22.72193555+(1.35000183*30)+(−0.01332176*30^2)+(1.25395882*16)
# 25.85 an hour for 30 year old with 16 years of education

#For 50 years old 
#Wage = −22.72193555+(1.35000183×50)+(−0.01332176×50^2)+(1.25395882*16)
#31.54 an hour for 50 years old with 16 years of education

#For 70 years old
#Wage = −22.72193555+(1.35000183×70)+(−0.01332176×70^2)+(1.25395882*16)
#26.56 an hour for 70 years old with 16 years of education

#According to the model, at what age will someone with 16 years of education attain the highest wages?
#The model predicts that someone around the age of 50 with 16 years of education will attain the highest wages. 

#age/(age^2)*2

1.35/0.026

#51.92 years old would attain the highest wage. 
