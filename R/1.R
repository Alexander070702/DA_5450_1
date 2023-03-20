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

# Model 3: Perform stepwise selection using AIC as the selection criterion
stepwise.model <- step(lm(Apps ~ ., data = College), direction = "both", trace = FALSE)

# Evaluate the final model's goodness of fit
par(mfrow=c(2,2))
plot(stepwise.model)

# Identify significant predictors
sig_preds <- names(which(summary(stepwise.model)$coefficients[,4] < 0.05))

# Interpret the coefficients
coeffs <- coef(stepwise.model)
cat("Top10perc coefficient:", coeffs["Top10perc"], "\n")
cat("Private coefficient:", coeffs["Private"], "\n")

# In-sample comparison of models
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

# Calculate training MSE
train.mse <- c(mean((predict(lm1.fit, newdata = train.data) - train.data$Apps)^2),
               mean((predict(lm2.fit, newdata = train.data) - train.data$Apps)^2),
               mean((predict(lm3.fit, newdata = train.data) - train.data$Apps)^2))
cat("Training MSE:", train.mse, "\n")

# Calculate test MSE
test.mse <- c(mean((lm1.pred - test.data$Apps)^2),
              mean((lm2.pred - test.data$Apps)^2),
              mean((lm3.pred - test.data$Apps)^2))
cat("Test MSE:", test.mse, "\n")

# Find index of the model with smallest test MSE
best.model.index <- which.min(test.mse)

# Output the best model
cat("Model", best.model.index, "is the best with a test MSE of", test.mse[best.model.index], "\n")
