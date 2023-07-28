library(caret)
library(modeldata)


# Set seed
set.seed(123)

# Shuffle row indices: rows
rows <- sample(nrow(diamonds))

# Randomly order data
shuffled_diamonds <- diamonds[rows, ]

# Determine row to split on: split
split <- round(nrow(diamonds) * 0.80)

# Create train
train <- shuffled_diamonds[1:split, ]

# Create test
test <- shuffled_diamonds[(split + 1):nrow(diamonds), ]

# Fit lm model on train: model
model <- lm(price ~ ., train)

# Predict on test: p
p <- predict(model, test)

# Compute errors: error
error <- p - test$price

# Calculate RMSE
print(sqrt(mean(error^2)))

# Fit lm model using 10-fold CV: model
(model <- train(
  price ~ ., 
  diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
))

library(ISLR2)

(model <- train(
  medv ~ ., 
  Boston,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", 
    number = 5,
    repeats = 5, 
    verboseIter = TRUE
  )
))

######### Sonar
library(mlbench)
library(caTools)
data(Sonar)

# Get the number of observations
n_obs <- nrow(Sonar)

# Shuffle row indices: permuted_rows
permuted_rows <- sample(n_obs)

# Randomly order data: Sonar
Sonar_shuffled <- Sonar[permuted_rows, ]

# Identify row to split on: split
split <- round(n_obs * .6)

# Create train
train <- Sonar_shuffled[1:split,]

# Create test
test <- Sonar_shuffled[(split+1):n_obs,]

# Fit glm model: model
model <- glm(Class ~ ., family = "binomial", train)

# Predict on test: p
p <- predict(model, test, type="response")

# If p exceeds threshold of 0.5, M else R: m_or_r
m_or_r <- ifelse(p > 0.5, "M", "R")

# Convert to factor: p_class
p_class <- factor(m_or_r, levels = levels(test$Class))

# Create confusion matrix
confusionMatrix(p_class, test$Class)

colAUC(p, test$Class, plotROC = TRUE)


########################3

data(mlc_churn)
table(mlc_churn$churn) / nrow(mlc_churn)

set.seed(123)

myFolds <- createFolds(mlc_churn$churn, k = 5)

i <- myFolds$Fold1
table(mlc_churn$churn[i]) / length(i)

myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

churn_x = model.matrix(churn ~ ., mlc_churn)[,-1]
churn_y = mlc_churn$churn

model_glmnet <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl
)


# Fit random forest: model_rf
model_glm <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "glm",
  family = "binomial",
  trControl = myControl
)

# Create model_list
model_list <- list(item1 = model_glmnet, item2 = model_glm)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)

bwplot(resamples, metric = "ROC")
xyplot(resamples)

