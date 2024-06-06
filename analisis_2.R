# Cargar las bibliotecas necesarias
library(tidyverse)

# Leer los datos
datos <- read.csv("Datos_3.csv")

# Limpiar los datos y dividirlos
datos <- na.omit(datos)  # Eliminar filas con valores NA
set.seed(123)  # Para reproducibilidad
indices <- sample(1:nrow(datos), size = nrow(datos) * 0.8)
train_data <- datos[indices, ]
test_data <- datos[-indices, ]

# Modelo 1: Regresión Lineal Simple
modelo1 <- lm(num_arrestos ~ asistencia_miles, data = train_data)

# Modelo 2: Regresión Lineal Múltiple
modelo2 <- lm(num_arrestos ~ asistencia_miles + inv_social_millones, data = train_data)

# Modelo 3: Regresión con Interacción de Variables
modelo3 <- lm(num_arrestos ~ asistencia_miles * inv_social_millones, data = train_data)

# Evaluar los modelos en el conjunto de entrenamiento
summary(modelo1)
summary(modelo2)
summary(modelo3)

# Evaluar los modelos en el conjunto de prueba
pred1 <- predict(modelo1, newdata = test_data)
pred2 <- predict(modelo2, newdata = test_data)
pred3 <- predict(modelo3, newdata = test_data)

# Calcular métricas de error, por ejemplo, RMSE
rmse1 <- sqrt(mean((test_data$num_arrestos - pred1)^2))
rmse2 <- sqrt(mean((test_data$num_arrestos - pred2)^2))
rmse3 <- sqrt(mean((test_data$num_arrestos - pred3)^2))

# Comparar RMSE
print(rmse1)
print(rmse2)
print(rmse3)
