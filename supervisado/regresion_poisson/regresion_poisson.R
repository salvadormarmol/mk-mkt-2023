set.seed(42)  # Fijar semilla para reproducibilidad
n <- 10000  # Tamaño de la muestra
lambda <- 3  # Parámetro de la distribución de Poisson
datos_poisson <- rpois(n, lambda)

# Calcular la media y la varianza
media_poisson <- mean(datos_poisson)
varianza_poisson <- var(datos_poisson)

media_poisson
# Output: 3.0208

varianza_poisson
# Output: 3.026264

# Cargar el paquete "ggplot2" para graficar los datos
# install.packages("ggplot2")
library(ggplot2)

# Generar valores de x de manera lineal
x <- 1:100

# Generar valores de y con una relación log-lineal
y <- exp(0.5 + 0.02 * x)  # Coeficientes 0.5 y 0.02 para la relación log-lineal

# Agregar ruido aleatorio a los valores de y
set.seed(42)  # Fijar semilla para reproducibilidad
ruido <- rnorm(length(x), mean = 0, sd = 0.2)  # Ruido aleatorio con media cero y desviación estándar 0.2
y <- y * (1 + ruido)  # Agregar ruido a los valores de y

# Crear un dataframe con los valores de x e y
data <- data.frame(x = x, y = round(y))

# Graficar los datos
ggplot(data, aes(x, y)) +
  geom_point() +
  labs(x = "x", y = "y") +
  ggtitle("Relación log-lineal") +
  theme_minimal()

# Ajustar un modelo de regresión de Poisson
poisson_model <- glm(y ~ x, data = data, family = poisson)

# Imprimir los resultados del modelo
summary(poisson_model)

# Graficar los datos con el suavizado del modelo de regresión de Poisson
# Predecir los valores ajustados del modelo de regresión de Poisson
data$y_pred <- predict(poisson_model, newdata = data, type = "response")

# Graficar los datos con el suavizado del modelo de regresión de Poisson
ggplot(data, aes(x, y)) +
  geom_point() +
  geom_line(aes(y = y_pred), color = "red") +
  labs(x = "x", y = "y") +
  ggtitle("Relación log-lineal con suavizado de Poisson") +
  theme_minimal()
