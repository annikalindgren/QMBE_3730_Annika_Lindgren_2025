# Load libraries
library(tidyverse)
library(caTools)

View(admit)
class(admit)

#Shape of the data set rows and columns
dim(admit)
#The data set has 400 rows and four columns "admit", "gre", "gpa", and "rank"

#Data types
str(admit)
#all four columns consist of numerical data 

#Missing values
sum(is.na(admit))sa
#there are 0 missing values in the data set

#Duplicates
duplicates <- duplicated(admit)
duplicates
#the data set seams to have 5 duplicates

#For classification models check the class distribution

# Convert 'am' to a factor (categorical variable)
admit$am <- as.factor(admit$am)

# View structure of dataset
str(admit)

# Summary statistics
summary(admit)

#Check for class balance
table(admit$am)
class_proportions<-prop.table(table(admit$am))
class_proportions

#Are the classes (admit/ don't admit) balanced in the data set?
#They are not balanced, 59.4% of students were not admitted, and 40.6% of the students were admitted.
#So out of the 400 observations (400*.594 =237 students not admitted) and (400 * .406 = 162 admitties)

# Split the data into training and testing sets.
# Set seed for reproducibility
set.seed(1)

# Split the dataset into training (70%) and testing (30%)
split <- sample.split(admit$admit, SplitRatio = 0.7)

# Create training and testing sets
# https://search.r-project.org/CRAN/refmans/caTools/html/sample.split.html
train_data <- subset(admit, split == TRUE)
test_data <- subset(admit, split == FALSE)

# Check dataset dimensions
dim(train_data)
dim(test_data)


#Fit the Logistic Regression Model


# Train the logistic regression model
log_model <- glm(admit ~ gre + gpa + rank, data = train_data, family = binomial) 

# Display model summary
summary(log_model)

#The model's intercept is -3.09. For each 1 point increase in GRE scores, the probability of admission 
#increases by 0.000809. For each 1 point increase in GPA, the probability of admission increases by 0.9, 
#while a 1 point increase in class rank results in a decrease of 0.5 in the probability of admission.
#Among the variables, GPA and Rank are significant. GPA is significant at the 5% level.

#I would say that data is a little right skewed, but is approximately normally distributed. 

# Predict probabilities on the test dataset
pred_probs <- predict(log_model, test_data, type = "response")
pred_probs

# Convert probabilities to binary predictions (threshold = 0.5) if higher/lower than threshold assign it to class
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Convert to factor for comparison
pred_classes <- as.factor(pred_classes)

# Display predictions
head(pred_probs)
head(pred_classes)

### Print predictions and true y values as dataframe
#how you evaluate classification models
do.call(rbind, Map(data.frame, predicted_classes=pred_classes, admit=test_data$admit))


# Create confusion matrix
# Actual vs. Predicted 
conf_matrix <- table(Predicted = pred_classes, Actual = test_data$admit)
conf_matrix

#This confusion matrix represents 77 true negatives implying that the model correctly predicted that 77 instances
#of negative (0) when they were actually negative (0), there are also 29 false positives which states that the model 
#incorrectly predicted 29 instances as negative (0) when they were actually. positive (0), it also states 
#that there are 5 false positives which means that the model falsely predicted 5 occurrences as positive (1) when they 
#were actually negative (0), and finally 9 true positives showing that the model correctly predicted 9 instances as positve (1)
#when they were actually positive (1)

# Compute accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

# Print results: check for accuracy
print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

#The accuracy of the model is 71.67% meaning that the model 72% can correctly predict if a student is admitted or not
#the precision is 77 out of 82 students, which means out of all 82 students that the model predicted to be admitted, 77 of them were actually admitted
#the recall is 77 out of 106. This means that out of the 106 students that were actually admitted, the model correctly identified 77 of them. 

#According to the model, rank is the most important factor when it comes to admitting and denying students. Significance = 0.1

# Visualizing predictions vs. actuals
ggplot(test_data, aes(x = gre, y = as.numeric(as.character(admit)), color = as.factor(pred_classes))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual Admission",
       x = "Graduate Record Exam scores (GRE)",
       y = "Admission (0 = Rejection, 1 = admission)") +
  scale_color_manual(values = c("red", "blue"), name = "Prediction")

