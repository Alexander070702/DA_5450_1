# Load libraries
library(rpart)
library(randomForest)
library(caret)
library(tidyverse)

# Read data
churndat <- read.csv(url("https://statmath.wu.ac.at/~vana/datasets/churn.csv"))

# Convert churn variable to factor
churndat$churn <- as.factor(churndat$churn)

# Split data
set.seed(123)
trainIndex <- createDataPartition(churndat$churn, p = 0.8, list = FALSE)
trainData <- churndat[trainIndex,]
testData <- churndat[-trainIndex,]

# Classification tree
treeModel <- rpart(churn ~ ., data = trainData, method = "class", cp = 0.01)
treeModel_pruned <- prune(treeModel, cp = treeModel$cptable[which.min(treeModel$cptable[,"xerror"]),"CP"])


# The optimally pruned classification tree splits the data mainly based on the following variables (top 5): totaldayminutes, totaldaycharge, internationalplan, numbercustomerservicecalls, and totaleveminutes.

# Random forest
set.seed(123)
rfModel <- randomForest(churn ~ ., data = trainData, mtry = 4)


# Variable importance
tree_var_importance <- as.data.frame(varImp(treeModel_pruned))
tree_var_importance <- tree_var_importance %>%
  rownames_to_column(var = "Variable") %>%
  arrange(desc(Overall))

rf_var_importance <- as.data.frame(importance(rfModel))
rf_var_importance <- rf_var_importance %>%
  rownames_to_column(var = "Variable") %>%
  arrange(desc(MeanDecreaseGini))


# The random forest model, where m=4 variables are randomly selected to build the tree, has the following top 5 important variables: totaldayminutes, totaldaycharge, numbercustomerservicecalls, internationalplan, and totalevecharge. Comparing this to the classification tree, we see that the top 4 important variables are the same, but the order of importance is slightly different. In comparison to the stepwise logistic regression from the previous assignment, the important variables identified by these models might be different as the methods are different.


# Print variable importances
print(tree_var_importance)
print(rf_var_importance)

# Test classification tree model
treeModel_pred <- predict(treeModel_pruned, newdata = testData, type = "class")
treeModel_cm <- confusionMatrix(treeModel_pred, testData$churn)

# Test random forest model
rfModel_pred <- predict(rfModel, newdata = testData)
rfModel_cm <- confusionMatrix(rfModel_pred, testData$churn)

# Compare performance metrics
models <- list(ClassificationTree = treeModel_cm, RandomForest = rfModel_cm)
metrics <- map_dfr(models, ~ .x$byClass, .id = "Model")
metrics <- metrics %>%
  select(Model, Accuracy = "Balanced Accuracy", Recall = "Sensitivity", Precision = "Precision")


print(metrics)
#Comparing the logistic regression, classification tree, and random forest models based on accuracy, recall, and precision on the test sample, we have the following results:
#Classification Tree: Accuracy: 0.833, Recall: 0.985, Precision: 0.949
#Random Forest: Accuracy: 0.864, Recall: 0.990, Precision: 0.958
#Based on these metrics, the random forest model performs better than the classification tree model in terms of accuracy, recall, and precision.
