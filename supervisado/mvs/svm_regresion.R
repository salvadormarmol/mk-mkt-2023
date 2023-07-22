library(e1071)
library(caret)
library(MLmetrics)

# Regression example
boston = MASS::Boston
set.seed(123)
indexes = createDataPartition(boston$medv, p = .8, list = F)
train = boston[indexes, ]
test = boston[-indexes, ]

set.seed(123)
#model_reg = svm(medv~., data=train,  kernel = 'radial')
model_reg = lm(medv~., data=train)
print(model_reg)

pred = predict(model_reg, test)

x=1:length(test$medv)
plot(x, test$medv, pch=18, col="red")
lines(x, pred, lwd="1", col="blue")

# accuracy check 
mse = MSE(test$medv, pred)
mae = MAE(test$medv, pred)
rmse = RMSE(test$medv, pred)
r2 = R2(test$medv, pred, form = "traditional")

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)

