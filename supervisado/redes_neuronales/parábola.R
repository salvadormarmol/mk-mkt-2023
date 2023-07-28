library(ggplot2)

# Generar datos x en el rango de -10 a 10
x <- seq(-10, 10, length.out = 100)

# Generar una parábola sin ruido
y_parabola <- x^2

# Generar ruido aleatorio
set.seed(1)  # Establecer una semilla para reproducibilidad
ruido <- rnorm(length(x), mean = 0, sd = 20)  # Ruido normalmente distribuido con media cero y desviación estándar de 20

# Agregar el ruido a la parábola
y <- y_parabola + ruido


# Crear un data frame con los valores de x y y
df <- data.frame(x = x, y = y)

# Graficar la parábola con ruido y un smooth de regresión lineal
ggplot(df, aes(x = x, y = y)) +
  geom_point() +  # Puntos de la parábola con ruido
  geom_smooth(method = "lm", se = FALSE) +  # Smooth de regresión lineal
  labs(x = "x", y = "y", title = "Parábola con ruido y smooth de regresión lineal")

reg <- lm(y~.,data=df)
old <- par(mfrow = c(2, 2))
plot(reg)
par(old)

df$x2 <- x*x

modelo <- lm(y ~ x + x2, data = df)
old <- par(mfrow = c(2, 2))
plot(modelo)
par(old)


reg <- lm(y~poly(x, degree = 2),data=df)
summary(reg)

old <- par(mfrow = c(2, 2))
plot(reg)
par(old)

# Graficar la parábola con ruido y un smooth de regresión lineal
ggplot(df, aes(x = x, y = y)) +
  geom_point() +  # Puntos de la parábola con ruido
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), se = FALSE) +  # Smooth de regresión lineal
  labs(x = "x", y = "y", title = "Parábola con ruido y smooth de regresión lineal")

