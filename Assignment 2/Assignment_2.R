# Load the caret package for data partitioning
library("caret")
library("ggplot2")

# Load the churn dataset from a URL and convert the churn variable to binary
churndat <- read.csv(url("https://statmath.wu.ac.at/~malsiner/datasets/churn.csv"))
churndat$churn <- ifelse(churndat$churn == "Yes", 1, 0)

# Convert the internationalplan and voicemailplan variables to binary
churndat$internationalplan <- ifelse(churndat$internationalplan == "yes", 1, 0)
churndat$voicemailplan <- ifelse(churndat$voicemailplan == "yes", 1, 0)

#structure of the data
str(churndat)

#Generate appropriate graphs for all variables differentiated into churn vs. no churn.
#accountlength
ggplot(churndat, aes(x = accountlength, fill = factor(churn))) + geom_histogram(bins = 20, position = "identity") + labs(title = "Account Length", x = "Account Length", y = "Frequency")
#internationalplan
ggplot(churndat, aes(x = internationalplan, fill = factor(churn))) + geom_bar(position = "identity") + labs(title = "International Plan", x = "International Plan", y = "Frequency")
#voicemailplan
ggplot(churndat, aes(x = voicemailplan, fill = factor(churn))) + geom_bar(position = "identity") + labs(title = "Voicemail Plan", x = "Voicemail Plan", y = "Frequency")
#numbervmailmessages
ggplot(churndat, aes(x = numbervmailmessages, fill = factor(churn))) + geom_histogram(bins = 20, position = "identity") + labs(title = "Number of Voicemail Messages", x = "Number of Voicemail Messages", y = "Frequency")
#totaldayminutes
ggplot(churndat, aes(x = totaldayminutes, fill = factor(churn))) + geom_histogram(bins = 20, position = "identity") + labs(title = "Total Day Minutes", x = "Total Day Minutes", y = "Frequency")
#totaleveminutes
ggplot(churndat, aes(x = totaleveminutes, fill = factor(churn))) + geom_histogram(bins = 20, position = "identity") + labs(title = "Total Eve Minutes", x = "Total Eve Minutes", y = "Frequency")
#totalnightminutes
ggplot(churndat, aes(x = totalnightminutes, fill = factor(churn))) + geom_histogram(bins = 20, position = "identity") + labs(title = "Total Night Minutes", x = "Total Night Minutes", y = "Frequency")
#totalintlcalls
ggplot(churndat, aes(x = totalintlcalls, fill = factor(churn))) + geom_histogram(bins = 20, position = "identity") + labs(title = "Total International Calls", x = "Total International Calls", y = "Frequency")
#totalintlcharge
ggplot(churndat, aes(x = totalintlcharge, fill = factor(churn))) + geom_histogram(bins = 20, position = "identity") + labs(title = "Total International Charge", x = "Total International Charge", y = "Frequency")
#numbercustomerservicecalls
ggplot(churndat, aes(x = numbercustomerservicecalls, fill = factor(churn))) + geom_histogram(bins = 20, position = "identity") + labs(title = "Number of Customer Service Calls", x = "Number of Customer Service Calls", y = "Frequency")
# There seems to be a relationship with churn and international plan. If the user has no International plan the churn rate is way lower compared to users with an international plan
# It is the opposite with the Voicemail Plan, if the user has one the churn rate is lower.
# Also with a rising number of customer service calls the churn rate seems to go up. 
# For all the other variables it is difficult to tell and there seems to be no real correlation. 


# 2 Splitting the data to training set and test set
# Split the data into training and testing sets, with 80% for training and 20% for testing
set.seed(123) # for reproducibility
train_index <- createDataPartition(churndat$churn, p = 0.8, list = FALSE)
train <- churndat[train_index, ]
test <- churndat[-train_index, ]

# 3 Full logistic regression model

# Fit a logistic regression model to the training data using all variables as predictors
mlm <- glm(churn ~ ., data = train, family = binomial())

# Print a summary of the logistic regression model
summary(mlm)

# The significant variables in this model are: internationalplan, voicemailplan, numbervmailmessages, totalintlcalls and numbercustomerservicecalls.


# 4 Logistic model stepwise analysis

# Perform stepwise variable selection on the logistic regression model using both forward and backward selection
# The 'trace' argument controls the amount of output produced during the stepwise procedure
mlm_step <- step(mlm, direction = "both", trace = FALSE)

# Print a summary of the stepwise logistic regression model
summary(mlm_step)

# Variables in the final model are : accountlength, internationalplan, voicemailplan, numbervmailmessages, totaldayminutes, 
# totaleveminutes, totalnightminutes, totalintlcalls, totalintlcharge and numbercustomerservicecalls.

# The coefficient of totaldayminutes suggests that there is a positive correlation between minutes talked and probability of leaving the company.
# Similarly, the coefficient of voicemailplan suggests a positive correlation between having a plan including a voicemail and the probability of leaving the company.


#5

# Calculate the predicted probabilities and predicted classes of the testing data using the stepwise logistic regression model
test$prob_churn_step <- predict(mlm_step, newdata = test, type = "response")
test$class_churn_step <- ifelse(test$prob_churn_step > 0.5, 1, 0)

# Calculate the accuracy, sensitivity, and specificity of the stepwise logistic regression model on the testing data
accuracy_step <- sum(test$class_churn_step == test$churn) / nrow(test)
sensitivity_step <- sum(test$class_churn_step[test$churn == 1] == 1) / sum(test$churn == 1)
specificity_step <- sum(test$class_churn_step[test$churn == 0] == 0) / sum(test$churn == 0)

# Print the accuracy, sensitivity, and specificity of the stepwise logistic regression model on the testing data
cat("Accuracy: ", round(accuracy_step, 4), "\n")
cat("Sensitivity: ", round(sensitivity_step, 4), "\n")
cat("Specificity: ", round(specificity_step, 4), "\n")

#6
library(e1071)
fit_nb = naiveBayes(churn ~ internationalplan + voicemailplan + numbercustomerservicecalls + totalintlcalls + numbervmailmessages, data = train)
