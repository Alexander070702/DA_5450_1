# Load the caret package for data partitioning
library("caret")

# Load the churn dataset from a URL and convert the churn variable to binary
churndat <- read.csv(url("https://statmath.wu.ac.at/~malsiner/datasets/churn.csv"))
churndat$churn <- ifelse(churndat$churn == "Yes", 1, 0)

# Convert the internationalplan and voicemailplan variables to binary
churndat$internationalplan <- ifelse(churndat$internationalplan == "yes", 1, 0)
churndat$voicemailplan <- ifelse(churndat$voicemailplan == "yes", 1, 0)

# Split the data into training and testing sets, with 80% for training and 20% for testing
train_index <- createDataPartition(churndat$churn, p = 0.8, list = FALSE)
train <- churndat[train_index, ]
test <- churndat[-train_index, ]

# Fit a logistic regression model to the training data using all variables as predictors
mlm <- glm(churn ~ ., data = train, family = binomial())

# Print a summary of the logistic regression model
summary(mlm)

# Perform stepwise variable selection on the logistic regression model using both forward and backward selection
# The 'trace' argument controls the amount of output produced during the stepwise procedure
mlm_step <- step(mlm, direction = "both", trace = FALSE)

# Print a summary of the stepwise logistic regression model
summary(mlm_step)

