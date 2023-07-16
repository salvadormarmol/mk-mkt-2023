# Helper packages
library(dplyr)     # for data wrangling
library(ggplot2)   # for awesome plotting
library(rsample)   # for data splitting
library(modeldata) # for attirtion dataset

# Modeling packages
library(caret)     # for logistic regression modeling

# Model interpretability packages
library(vip)       # variable importance

data("attrition", package = "modeldata")

# Job attrition data
churn <- attrition %>% 
  mutate_if(is.ordered, .funs = factor, ordered = FALSE)

df <- churn %>% mutate_if(is.ordered, factor, ordered = FALSE)

# Create training (70%) and test (30%) sets for the 
# rsample::attrition data.
set.seed(123)  # for reproducibility
churn_split <- df %>% initial_split(prop = .7, strata = "Attrition")
churn_train <- training(churn_split)
churn_test  <- testing(churn_split)

model1 <- glm(Attrition ~ MonthlyIncome, family = "binomial", data = churn_train)
model2 <- glm(Attrition ~ OverTime, family = "binomial", data = churn_train)

library(broom)
tidy(model1)
tidy(model2)

# x = MonthlyIncome
# ln(mu(x)/(1-mu(X))) = -0.886 - 0.000139 * x
# mu(x)/(1-mu(X)) = exp(-0.886 - 0.000139 * x)
# mu(x)/(1-mu(X)) = exp(-0.886) * exp(0.000139 * x)

# mu(x)/(1-mu(X)) = 0.4122 * 0.99


exp(coef(model1))
exp(coef(model2))

confint(model1)
confint(model2)

model3 <- glm(
  Attrition ~ MonthlyIncome + OverTime,
  family = "binomial", 
  data = churn_train
)

tidy(model3)
exp(coef(model3))

# Cambie el nombre del método para que no generara el warning y si realizara las repeticiones
set.seed(123)
cv_model1 <- train(
  Attrition ~ MonthlyIncome, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5) 
  # si solo se quiere cv sin repeats se debe cambiar el method a cv y quitar el parámetro repeats 
)

set.seed(123)
cv_model2 <- train(
  Attrition ~ MonthlyIncome + OverTime, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5)
)

set.seed(123)
cv_model3 <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5)
)

# se cambiaron todos los entrenamientos para que fueran comparables y el resample no se quejara

# extract out of sample performance measures
summary(
  resamples(
    list(
      model1 = cv_model1, 
      model2 = cv_model2, 
      model3 = cv_model3
    )
  )
)$statistics$Accuracy

vip(cv_model3)

summary(cv_model3)

# predict class
pred_class <- predict(cv_model3, churn_train)

# create confusion matrix
cm <- confusionMatrix(
  data = relevel(pred_class, ref = "Yes"), 
  reference = relevel(churn_train$Attrition, ref = "Yes")
)

cm$byClass["F1"]
cm$byClass["Precision"]
cm$byClass["Recall"]
cm$overall['Accuracy']

summary(churn_train$Attrition)

#Vemos como están balanceadas las bases
summary(churn_train$Attrition)/sum(summary(churn_train$Attrition))

library(ROCR)

# Compute predicted probabilities
m1_prob <- predict(cv_model1, churn_train, type = "prob")$Yes
m3_prob <- predict(cv_model3, churn_train, type = "prob")$Yes

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, churn_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")
perf2 <- prediction(m3_prob, churn_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = "black", lty = 2)
plot(perf2, add = TRUE, col = "blue")
legend(0.8, 0.8, legend = c("cv_model1", "cv_model3"),
       col = c("black", "blue"), lty = 2:1, cex = 0.6)


pred <- predict(model1, churn_test, type = "response")
pred_obj <- prediction(pred, churn_test$Attrition)
roc_obj <- performance(pred_obj, "tpr", "fpr")
auc <- performance(pred_obj, "auc")@y.values[[1]]
print(paste0("AUC modelo 1: ",auc))

pred <- predict(model3, churn_test, type = "response")
pred_obj <- prediction(pred, churn_test$Attrition)
roc_obj <- performance(pred_obj, "tpr", "fpr")
auc <- performance(pred_obj, "auc")@y.values[[1]]
print(paste0("AUC modelo 3: ",auc))

