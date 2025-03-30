library(caret)
library(randomForest)
library(tidyverse)
install.packages("pROC")
library(pROC)
install.packages("dp")
library(dp)
library(dplyr)
library(ggplot2)
library(caTools)

#1 Description of dataset

view(loan_default_data_set)
loan_data<-loan_default_data_set

dim(loan_data)
summary(loan_data)
#the loan_default_data_set has 20,000 observations and 21 variables (columns)
str(loan_data)

#2 Missing values
colSums(is.na(loan_data))
#There are only three varaibles with missing values: "pct_card_over_50_uti" has 1958 missing values,
#rep_income has 1559 missing values and rep_education has 1 missing value. 
loan_data1 <- na.omit(loan_data)
#This is the code I would use to go about the missing values in the data set, it cleaned the data leaving
#us with 16653 observations due to deleting all the missing values. 

#3 Duplicates
sum(duplicated(loan_data))
#There are no duplicates in the data set 

#4 Duplicates continued
loan_data2 <- loan_data %>% distinct()
#This function removed all the duplicated values in the data set 
loan_data1$rep_education <- as.numeric(loan_data1$rep_education)
loan_data1$rep_education <- as.factor(loan_data1$rep_education)
#These two codes would be used to convert data into different classification types

#5 Plotting
ggplot(loan_data1, aes(x = tot_balance, y = rep_income)) +
  geom_point(color = "red") +
  labs(title = "Total Balance vs Reported Income",
       x = "Total Balance",
       y = "Reported Income") +
  theme_minimal()
#The scatter plot shows the correlation between the two variables "total balance" and "reported income" 
#The data points seem to be randomly distributed meaning there is no visable correlation between the two variables. 

#6 Education level under representation
count(loan_data1,rep_education)
#"other" is miss represented 
count_other <- sum(tolower(loan_data1$rep_education) == "other")
print(count_other)

#7 Are the classes balanced?
count(loan_data1,Def_ind)
#They are not balanced, there are 14956 non defaulted variables and 2697 defaulted variables.
#This is where we would use the F1 Score which focuses on precision and recall. 
#You could also use the AUC which helps show the models performance on different classifications. 

#8 Distribution of rep_income
summary(loan_data1$rep_income)
hist(loan_data1$rep_income, main = "rep_income", xlab = "Income", col = "blue", border = "black")

#The distribution of "rep_income" is approximately normally according to the histogram plotted. 
#The median is 166630 and mean of 166504 with the first quartile being 143751 and the third quartile of 189020. 

#9 Grouping default status by education level 
default_rates<-loan_data1%>%
  group_by(rep_education)%>%
  summarize(
    total_count=n(),
    default_count=sum(Def_ind),
    default_rate=mean(Def_ind)*100)
default_rates

#The likelihood of someone defaulting on a loan is 10.2% higher for those with the greatest degree of education.

#10 Anything else
cor_matrix <- cor(loan_data1[, sapply(loan_data1, is.numeric)], use = "complete.obs")
print(cor_matrix)

#This correlation matrix shows that credit age and credit card age are positively correlated along with
#credit age and credit age for good accounts. 

loan_data1$Def_ind<- as.factor(loan_data1$Def_ind)
# Split data set
set.seed(2)
split <- sample.split(loan_data1$Def_ind, SplitRatio = 0.8)
train <- subset(loan_data1,split == TRUE)
test <- subset(loan_data1,split == FALSE)

# Train and evaluate KNN
train$Def_ind<-as.factor(train$Def_ind)
test$Def_ind<-as.factor(test$Def_ind)
train_clean <- na.omit(train)
train_clean <- train[complete.cases(train), ]
colSums(is.na(train_clean))
knn_model <- train(Def_ind~ ., data=train_clean, method='knn', tuneLength=5) # Fit KNN model
pred_knn <- predict(knn_model, test)
print(confusionMatrix(pred_knn, test$Def_ind))

# Results from KNN
#Confusion matrix:
  #True Negatives is 2981 which shows that the model correctly predicted "no default".
  #False Positives is 319 which shows that the model incorrectly predicted as "default".
  #False Negatives is 10 which shows that the model incorrectly predicted as "no default" when it was a "default".
  #True Positives is 20 which shows that the model correctly predicted "default".
#Accuracy (TP + TN) / (TP + TN + FP + FN) = (20+2981)/(2981+319+10+20) was 90.12 % meaning the model predicted correctly 90.12% of the time.
#Precision TP/ (TP+FN) 20/(20+10) was 66.67% meaning the model correctly predicted default cases 66.67% of the time.  
#Recall = TP / (TP + FP) = 20 / (20 + 319) = 0.058 which means that 5.83% of the time the model was able to predict a default. 

#Plot ROC/AUC curve

pred_probs <- predict(knn_model, newdata = test, type = "prob")[, 2]
roc_curve <- roc(test$Def_ind, pred_probs)
plot(roc_curve)
auc(roc_curve)
#Area under the curve is 0.6461
#I think the most important variable is credit age. 


# Train and evaluate Decision Tree
train$Def_ind<-as.factor(train$Def_ind)
test$Def_ind<-as.factor(test$Def_ind)
train_clean <- na.omit(train)
train_clean <- train[complete.cases(train), ]
dt_model <- train(Def_ind~.,data=train,method='rpart')
pred_dt <- predict(dt_model, test)
print(confusionMatrix(pred_dt, test$Def_ind))

## Results from Decision Tree
#Confusion matrix:
#True Negatives is 2981 which shows that the model correctly predicted "no default".
#False Positives is 315 which shows that the model incorrectly predicted as "default".
#False Negatives is 12 which shows that the model incorrectly predicted as "no default" when it was a "default".
#True Positives is 18 which shows that the model correctly predicted "default".
#Accuracy (TP + TN) / (TP + TN + FP + FN) = (18+2981)/(2981+315+12+28) was 89.89 % meaning the model predicted correctly 89.89% of the time.
#Precision TP/ (TP+FN) 18/(18+12) was 60% meaning the model correctly predicted default cases 607% of the time.  
#Recall = TP / (TP + FP) = 18 / (18 + 315) = 0.054 which means that 0.054% of the time the model was able to predict a default. 

#I would say the average card balance is the most important variable which is significant at the .1% level. 

## Proceed with evaluation and interpretation of both models.
#F1 KNN
2*(0.6667*0.058)/(0.6667+0.058)
#F1= 10.67%

#F1 of DT
2*(0.6*0.054)/(0.6+0.054)
#F1= 9.91%

#The KNN model works better due to the higher F1 score. I would also say that the KNN model had a higher
#accuracy, and precision over the decision tree model. 
