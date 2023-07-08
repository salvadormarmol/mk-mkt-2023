library(ggplot2)

df <- read.csv('data.csv')

ggplot(data = df, mapping = aes(x = Spent, y = Total_Conversion)) +
  geom_point() +
  labs(x = "Inversión", y = "Conversiones", title = "Gráfica de dispersión de Conversines vs gasto") +
  geom_smooth(method = 'loess', formula = 'y ~ x')

ggplot(data = df) +
  geom_point(mapping = aes(x = Spent, y = Total_Conversion)) +
  labs(x = "Inversión", y = "Conversiones", title = "Gráfica de dispersión de Conversines vs gasto") +
  geom_smooth(mapping = aes(x = Spent, y = Total_Conversion), method = 'lm', formula = 'y ~ x')

reg <- lm(Total_Conversion ~ Spent, data=df)

summary(reg)

old <- par(mfrow = c(2, 2))
plot(reg)
par(old)

ggplot(data = df) +
  geom_point(mapping = aes(x = Spent, y = Total_Conversion)) +
  labs(x = "Inversión", y = "Conversiones", title = "Gráfica de dispersión de Conversines vs gasto") 

ggplot(data = df) +
  geom_point(mapping = aes(x = Spent, y = Approved_Conversion)) +
  labs(x = "Inversión", y = "Conversiones", title = "Gráfica de dispersión de Conversines vs gasto") 


reg <- lm(Approved_Conversion ~ Spent, data=df)

summary(reg)

old <- par(mfrow = c(2, 2))
plot(reg)
par(old)

df$interest <- factor(df$interest)
summary(df)

reg <- lm(Total_Conversion ~ Clicks, data=df)
summary(reg)

old <- par(mfrow = c(2, 2))
plot(reg)
par(old)

reg <- glm(Total_Conversion ~ age + gender + interest + Spent + Impressions, data=df, family = poisson)
summary(reg)

old <- par(mfrow = c(2, 2))
plot(reg)
par(old)

# Ajustar un modelo de regresión de Poisson
reg <- glm(Total_Conversion ~ Spent, data = df, family = poisson)

# Generar valores predichos del modelo
datos_predichos <- data.frame(
  Spent = seq(min(df$Spent), max(df$Spent), length.out = 100)
)

datos_predichos$y_pred <- predict(reg, newdata = datos_predichos, type = "response")

# Crear el gráfico
ggplot(df, aes(x = Spent, y = Total_Conversion)) +
  geom_point() +
  geom_line(data = datos_predichos, aes(x = Spent, y = y_pred), color = "red") +
  labs(x = "Inversión", y = "Conversiones")

old <- par(mfrow = c(2, 2))
plot(reg)
par(old)

library(nortest)
lillie.test(rnorm(100, mean = 5, sd = 3))
lillie.test(runif(100, min = 2, max = 4))
lillie.test(rnorm(100, mean = 0, sd = 1))
shapiro.test(rnorm(100, mean = 0, sd = 1))
shapiro.test(runif(100, min = 2, max = 4))

df_new =  subset(df, select = -c(ad_id, xyz_campaign_id, fb_campaign_id, Approved_Conversion))

modelo <- step(lm(Total_Conversion ~ ., data = df_new), direction = "backward")

# Imprimir los resultados del modelo
summary(modelo)

set.seed(123)    # seef for reproducibility
library(glmnet)  # for ridge regression
library(dplyr)   # for data cleaning

df <- read.csv('data.csv')

df <- df %>%  mutate(interest = factor(interest))

df_new <- df %>% select(-c(ad_id, xyz_campaign_id, fb_campaign_id, Total_Conversion, Approved_Conversion))
X <- model.matrix(~ . - 1, data = df_new)
y <- df %>% select(Total_Conversion) %>% as.matrix()

lambdas_to_try <- 10 ^ seq(-3, 5, length.out = 100)
ridge_cv <- cv.glmnet(
  X,
  y,
  alpha = 0,
  lambda = lambdas_to_try,
  standardize = TRUE,
  nfolds = 10
)
plot(ridge_cv)

lambda_cv <- ridge_cv$lambda.min

model_cv <-
  glmnet(X,
         y,
         alpha = 0,
         lambda = lambda_cv,
         standardize = TRUE)
y_hat_cv <- predict(model_cv, X)
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)
rsq_ridge_cv <- cor(y, y_hat_cv) ^ 2

res <-
  glmnet(X,
         y,
         alpha = 0,
         lambda = lambdas_to_try,
         standardize = FALSE)
plot(res, xvar = "lambda")
legend(
  "bottomright",
  lwd = 1,
  col = 1:6,
  legend = colnames(X),
  cex = .7
)

weights <- sapply(seq(ncol(X)), function(predictor) {
  uni_model <- lm(y ~ X[, predictor])
  coeff_variance <- summary(uni_model)$coefficients[2, 2] ^ 2
})

# Lasso

# Perform 10-fold cross-validation to select lambda
lambdas_to_try <- 10 ^ seq(-3, 5, length.out = 100)

lasso_cv <- cv.glmnet(
  X,
  y,
  alpha = 1,
  lambda = lambdas_to_try,
  standardize = TRUE,
  nfolds = 10
)
plot(lasso_cv)

lambda_cv <- lasso_cv$lambda.min

model_cv <-
  glmnet(X,
         y,
         alpha = 1,
         lambda = lambda_cv,
         standardize = TRUE)
y_hat_cv <- predict(model_cv, X)
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)
rsq_lasso_cv <- cor(y, y_hat_cv) ^ 2

res <-
  glmnet(X,
         y,
         alpha = 1,
         lambda = lambdas_to_try,
         standardize = FALSE)
plot(res, xvar = "lambda")
legend(
  "bottomright",
  lwd = 1,
  col = 1:6,
  legend = colnames(X),
  cex = .7
)
