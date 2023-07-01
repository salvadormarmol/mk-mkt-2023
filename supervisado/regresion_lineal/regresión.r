# Nos ubicamos en el directorio de trabajo donde tenemos los correos ejemplo

df <- read.csv('pesos_y_alturas_v1.csv')

# obtiene regesión lineal
lm.fit <- lm(Peso ~ Altura,
             data = df)

lm.fit$coefficients
lm.fit$residuals
lm.fit$fitted.values

# calcula error cuadrado medio y la raiz del error cuadrado medio
errors <- residuals(lm.fit) 
squared.errors <- errors ^ 2
mse <- mean(squared.errors)
rmse <- sqrt(mse)
rmse

summary(lm.fit)

# calcula r cuadrada
r2 <- 1-sum(squared.errors)/sum((df$Peso-mean(df$Peso))^2)
r2

df <- read.csv('pesos_y_alturas.csv')

# obtiene regesión lineal
lm.fit <- lm(Peso ~ Sexo + Altura,
             data = df)

lm.fit$coefficients

# calcula error cuadrado medio y la raiz del error cuadrado medio
errors <- residuals(lm.fit) 
squared.errors <- errors ^ 2
mse <- mean(squared.errors)
rmse <- sqrt(mse)
rmse

# calcula r cuadrada
r2 <- 1-sum(squared.errors)/sum((df$Peso-mean(df$Peso))^2)
r2

summary(lm.fit)

