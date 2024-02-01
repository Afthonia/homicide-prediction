#options(java.parameters = "-Xmx4g")

# Install and load the required packages
install.packages("C50")
install.packages("rpart")
install.packages("rpart.plot")
library(C50)
library(rpart)
#library(RWeka)

# Read the homicide dataset
# PLEASE CHANGE THE LOCATION TO THE ORIGINAL FILE LOCATION
# PLEASE CHANGE FOR USING THE CLEANED, 21 COLUMN DATASET OR THE DATASET AFTER FEATURE SELECTION
# 'murders-cleaned.csv' or 'murders-feature-selected.csv'
data <- read.csv("../murders-cleaned.csv")

# Convert the outcome variable to a factor
data$Relationship <- as.factor(data$Relationship)

# Split the data into training and test sets (you may need to adjust the split ratio)
set.seed(42)
train_index <- sample(1:nrow(data), 0.7*nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# C4.5 Decision Tree (not working - about memory)
#c45_model <- J48(formula = Relationship ~ ., data = train_data)
#c45_pred <- predict(c45_model, test_data, type = "class")
#c45_accuracy <- sum(c45_pred == test_data$Relationship) / nrow(test_data)
#print(paste("C4.5 Accuracy:", c45_accuracy))

# C5.0 Decision Tree
c50_model <- C5.0(formula = Relationship ~ ., data = train_data)
c50_pred <- predict(c50_model, test_data)
c50_accuracy <- sum(c50_pred == test_data$Relationship) / nrow(test_data)
print(paste("C5.0 Accuracy:", c50_accuracy))

# C5.0 Boosted Decision Tree
c50boost_model <- C5.0(formula = Relationship ~ ., data = train_data, trials = 10)
c50boost_pred <- predict(c50boost_model, test_data)
c50boost_accuracy <- sum(c50boost_pred == test_data$Relationship) / nrow(test_data)
print(paste("C5.0 Boosted Accuracy:", c50boost_accuracy))

# Gini Decision Tree
gini_model <- rpart(Relationship ~ ., data = train_data, method = "class")
gini_pred <- predict(gini_model, newdata = test_data, type = "class")
gini_accuracy <- sum(gini_pred == test_data$Relationship) / nrow(test_data)
print(paste("Gini Accuracy:", gini_accuracy))
