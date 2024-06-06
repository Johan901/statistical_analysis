# Cargar las bibliotecas necesarias
library(tidyverse)

# Cargar los datos
datos <- read.csv("Datos_3.csv")

# Ver las primeras filas del conjunto de datos
head(datos)

# Resumen estadístico de las variables
summary(datos)

# Chequear valores faltantes
sum(is.na(datos))

# Histogramas para variables numéricas
hist(datos$asistencia_miles, main = "Histograma de Asistencia en Miles", xlab = "Asistencia en Miles")
hist(datos$num_arrestos, main = "Histograma de Número de Arrestos", xlab = "Número de Arrestos")
hist(datos$inv_social_millones, main = "Histograma de Inversión Social en Millones", xlab = "Inversión Social en Millones")

# Diagramas de caja para las variables numéricas
boxplot(datos$asistencia_miles, main = "Diagrama de Caja de Asistencia en Miles")
boxplot(datos$num_arrestos, main = "Diagrama de Caja de Número de Arrestos")
boxplot(datos$inv_social_millones, main = "Diagrama de Caja de Inversión Social en Millones")
