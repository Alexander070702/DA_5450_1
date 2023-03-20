# Load the data
if (!require("ISLR")) install.packages("ISLR")
library("ISLR")
data("College")

# Descriptive statistics and visualization
summary(College)
str(College)
cor(College[, -1])
pairs(College[, -1])

# Check for missing data
sum(is.na(College))

# Model 1: Fit a linear regression model using all variables
lm1 <- lm(Apps ~ ., data = College)
summary(lm1)

# Model 2: Fit a linear regression model using selected variables
lm2 <- lm(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend, data = College)
summary(lm2)

# Model 3: Perform stepwise variable selection
lm3 <- step(lm1)
summary(lm3)

# In-sample comparison of models
library("caret")
train.control <- trainControl(method = "cv", number = 5)
mse1 <- train(Apps ~ ., data = College, method = "lm", trControl = train.control)
mse2 <- train(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend, data = College, method = "lm", trControl = train.control)
mse3 <- train(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio, data = College, method = "lm", trControl = train.control)
mse1$results$RMSE
mse2$results$RMSE
mse3$results$RMSE

# Out-of-sample comparison of models
set.seed(123)
train.index <- createDataPartition(College$Apps, p = 0.8, list = FALSE)
train.data <- College[train.index, ]
test.data <- College[-train.index, ]
lm1.fit <- lm(Apps ~ ., data = train.data)
lm2.fit <- lm(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend, data = train.data)
lm3.fit <- lm(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio, data = train.data)
lm1.pred <- predict(lm1.fit, newdata = test.data)
lm2.pred <- predict(lm2.fit, newdata = test.data)
lm3.pred <- predict(lm3.fit, newdata = test.data)
caret::RMSE(lm1.pred, test.data$Apps)
caret::RMSE(lm2.pred, test.data$Apps)
caret::RMSE(lm3.pred, test.data$Apps)

