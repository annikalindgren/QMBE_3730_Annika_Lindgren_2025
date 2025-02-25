# Load libraries
library(tidyverse)
library(caTools)

# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars 
# We'll use the mtcars dataset and predict whether a car is automatic (am = 0) or manual (am = 1).

# Load dataset
data("mtcars")

View(mtcars) #View data set.

#Exploratory Data Analysis. FOR HOMEWORK ASSIGNMENT 2
#Shape of the data set rows and columns
#Data types
#Plot the variables
#Missing values
#Duplicates

#For classification models check the class distribution

# Convert 'am' to a factor (categorical variable)
mtcars$am <- as.factor(mtcars$am)

# View structure of dataset
str(mtcars)

# Summary statistics
summary(mtcars)

#Check for class balance
table(mtcars$am)
class_proportions<-prop.table(table(mtcars$am))
class_proportions

# Split the data into training and testing sets.
# Set seed for reproducibility
set.seed(1)

# Split the dataset into training (70%) and testing (30%)
split <- sample.split(mtcars$am, SplitRatio = 0.7)

# Create training and testing sets
# https://search.r-project.org/CRAN/refmans/caTools/html/sample.split.html
train_data <- subset(mtcars, split == TRUE)
test_data <- subset(mtcars, split == FALSE)

# Check dataset dimensions
dim(train_data)
dim(test_data)


#Fit the Logistic Regression Model


# Train the logistic regression model
log_model <- glm(am ~ mpg + hp + wt, data = train_data, family = binomial) # Binomial Distribution, Y variable is binary
# https://www.datacamp.com/doc/r/glm 

# Display model summary
summary(log_model)

# what fits best? when model's output is in absolute terms (the bigger the more important it is)
# glm() fits a generalized linear model.
# am ~ mpg + hp + wt means we predict am based on mpg (miles per gallon), hp (horsepower), and wt (weight).
# family = binomial specifies logistic regression (since it models probabilities).
# summary(log_model) displays coefficients, significance levels, and model fit statistics.

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
do.call(rbind, Map(data.frame, predicted_classes=pred_classes, am=test_data$am))

#Evaluate model performance

# Create confusion matrix
# Actual vs. Predicted 
conf_matrix <- table(Predicted = pred_classes, Actual = test_data$am)
conf_matrix

# Compute accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

# Print results: check for accuracy
print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))


# Visualizing predictions vs. actuals
ggplot(test_data, aes(x = mpg, y = as.numeric(as.character(am)), color = as.factor(pred_classes))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual Transmission Type",
       x = "Miles Per Gallon (mpg)",
       y = "Transmission Type (0 = Auto, 1 = Manual)") +
  scale_color_manual(values = c("red", "blue"), name = "Prediction")


