# Load the required library
library(randomForest)

# Read the homicide dataset
data <- read.csv("murders-feature-selected.csv")

# Convert the outcome variable to a factor
data$Relationship <- as.factor(data$Relationship)

# Split the data into training and test sets (you may need to adjust the split ratio)
set.seed(123)
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Random Forest
rf_model <- randomForest(Relationship ~ ., data = train_data)
rf_pred <- predict(rf_model, test_data, type = "class")
rf_accuracy <- sum(rf_pred == test_data$Relationship) / nrow(test_data)
print(paste("Random Forest Accuracy:", rf_accuracy))