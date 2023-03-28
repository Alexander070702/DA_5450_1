library ("caret")

churndat <- read.csv(url("https://statmath.wu.ac.at/~malsiner/datasets/churn.csv"))
churndat$churn <- ifelse(churndat$churn == "Yes", 1, 0)
churndat$internationalplan <- ifelse(churndat$internationalplan == "yes", 1, 0)
churndat$voicemailplan <- ifelse(churndat$voicemailplan == "yes", 1, 0)





#2
set.seed(123)
train_index <- createDataPartition(churndat$churn, p = 0.8, list = FALSE)
train <- churndat[train_index, ]
test <- churndat[-train_index, ]

#3
mlm = glm(churn ~ .,data = train, family = binomial())
summary(mlm)

#4
mlm_step = step(mlm, direction = "both", trace = FALSE)
summary(mlm_step)

