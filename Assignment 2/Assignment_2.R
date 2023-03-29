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
ggplot(churndat, aes(x = accountlength, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Account Length", y = "Count", title = "Account Length Distribution")
#internationalplan
ggplot(churndat, aes(x = internationalplan, fill = churn)) + geom_bar(position = "dodge") + labs(x = "International Plan", y = "Count", title = "International Plan Distribution")
#voicemailplan
ggplot(churndat, aes(x = voicemailplan, fill = churn)) + geom_bar(position = "dodge") + labs(x = "Voicemail Plan", y = "Count", title = "Voicemail Plan Distribution")
#numbercustomerservicecalls
ggplot(churndat, aes(x = numbercustomerservicecalls, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Number of Customer Service Calls", y = "Count", title = "Number of Customer Service Calls Distribution")
#totaldayminutes
ggplot(churndat, aes(x = totaldayminutes, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Day Minutes", y = "Count", title = "Total Day Minutes Distribution")
#totaldaycalls
ggplot(churndat, aes(x = totaldaycalls, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Day Calls", y = "Count", title = "Total Day Calls Distribution")
#totaldaycharge
ggplot(churndat, aes(x = totaldaycharge, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Day Charge", y = "Count", title = "Total Day Charge Distribution")
#totaleveminutes
ggplot(churndat, aes(x = totaleveminutes, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Eve Minutes", y = "Count", title = "Total Eve Minutes Distribution")
#totalevecalls
ggplot(churndat, aes(x = totalevecalls, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Eve Calls", y = "Count", title = "Total Eve Calls Distribution")
#totalevecharge
ggplot(churndat, aes(x = totalevecharge, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Eve Charge", y = "Count", title = "Total Eve Charge Distribution")
#totalnightminutes
ggplot(churndat, aes(x = totalnightminutes, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Night Minutes", y = "Count", title = "Total Night Minutes Distribution")
#totalnightcalls
ggplot(churndat, aes(x = totalnightcalls, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Night Calls", y = "Count", title = "Total Night Calls Distribution")
#totalnightcharge
ggplot(churndat, aes(x = totalnightcharge, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Night Charge", y = "Count", title = "Total Night Charge Distribution")
#totalintlcalls
ggplot(churndat, aes(x = totalintlcalls, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Intl Calls", y = "Count", title = "Total Intl Calls Distribution")
#totalintlcharge
ggplot(churndat, aes(x = totalintlcharge, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Total Intl Charge", y = "Count", title = "Total Intl Charge Distribution")
#numbervmailmessages
ggplot(churndat, aes(x = numbervmailmessages, fill = churn)) + geom_histogram(bins = 20, position = "identity") + labs(x = "Number of Vmail Messages", y = "Count", title = "Number of Vmail Messages Distribution")
# Which variables seem to be related to churn?
# The variables that seem to be related to churn are: internationalplan, voicemailplan, numbervmailmessages, totalintlcalls and numbercustomerservicecalls.

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
