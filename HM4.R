####################### PUNTO 1
library(forecast)


años <- 1:22
ventas <- c(0.39, 0.81, 0.93, 1.35, 1.48, 2.36, 2.45, 2.52, 2.81, 3.82, 5.5, 7.16, 1.93, 5.17, 7.72, 5.33, 8.12, 10.65, 10.06, 11.63, 5.85, 6.4)


modelo_holt <- holt(ts(ventas), h = 10, damped = TRUE)


plot(forecast(modelo_holt), main = "Suavizado de Holt para Triton Energy", xlab = "Años", ylab = "Ventas")


performance_metrics <- accuracy(forecast(modelo_holt))
rmse <- performance_metrics[1, "RMSE"]
mape <- performance_metrics[1, "MAPE"]

cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "%\n")



####################### PUNTO 2

library(forecast)

años <- 1980:1989
pesca <- c(6.482, 5.977, 6.367, 6.439, 6.438, 6.258, 6.031, 6.896, 7.192, 8.463)

modelo_holt_pesca <- holt(ts(pesca), h = 10, damped = TRUE)

plot(forecast(modelo_holt_pesca), main = "Suavizado de Holt para la Pesca en Colombia", xlab = "Año", ylab = "Millones de libras")

performance_metrics_pesca <- accuracy(forecast(modelo_holt_pesca))
rmse_pesca <- performance_metrics_pesca[1, "RMSE"]
mape_pesca <- performance_metrics_pesca[1, "MAPE"]

cat("RMSE:", rmse_pesca, "\n")
cat("MAPE:", mape_pesca, "%\n")



####################### PUNTO 3
library(ggplot2)

set.seed(123)
n <- 1000
phi <- 0.7
ar_proceso <- numeric(n)
ar_proceso[1] <- rnorm(1, mean = 0, sd = sqrt((1 - phi^2)))  # Varianza estacionaria
for (i in 2:n) {
  ar_proceso[i] <- phi * ar_proceso[i - 1] + rnorm(1, mean = 0, sd = 1)
}

data_ar <- data.frame(Time = 1:n, Values = ar_proceso)

ggplot(data_ar, aes(x = Time, y = Values)) + geom_line() +
  ggtitle("Proceso AR(1) Simulado") + xlab("Tiempo") + ylab("Valores")

acf(ar_proceso, main="Función de Autocorrelación del Proceso AR(1)")



####################### PUNTO 4
set.seed(123)
X_t <- rnorm(100) 

beta_0 <- 1
beta_1 <- 0.5
beta_2 <- 0.3
t <- 1:100

Z_t_const <- rep(beta_0, 100)
Z_t_linear <- beta_0 + beta_1 * X_t + beta_2 * t

BZ_t_const <- c(NA, Z_t_const[-100])
BZ_t_linear <- c(NA, Z_t_linear[-100])
BBZ_t_linear <- c(NA, NA, Z_t_linear[-(99:100)])

data_linear <- data.frame(Time = t, Z_t = Z_t_linear, BZ_t = BZ_t_linear, BBZ_t = BBZ_t_linear)
ggplot(data_linear, aes(x = Time)) +
  geom_line(aes(y = Z_t, colour = "Z_t")) +
  geom_line(aes(y = BZ_t, colour = "BZ_t")) +
  geom_line(aes(y = BBZ_t, colour = "BBZ_t")) +
  labs(colour = "Series") +
  ggtitle("Efecto del Operador de Rezago en una Serie Temporal Lineal") +
  xlab("Tiempo") + ylab("Valores")

cat("BZ_t constante: ", BZ_t_const[2], "\n")
cat("B^2Z_t constante: ", BZ_t_const[3], "\n")



