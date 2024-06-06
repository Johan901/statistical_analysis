# PUNTO 1

# Cargar las librerías necesarias
library(forecast)
library(tseries)
library(ggplot2)
library(readxl)  # Para leer archivos Excel

# Cargar los datos desde un archivo Excel
datos <- read_excel("Electricidad.xls", sheet = 1)  # Asegúrate de especificar la hoja correcta

# Convertir a serie temporal
datos_ts <- ts(datos$IPG2211A2N, start=c(1939, 1), frequency=12)

# Análisis Exploratorio de Datos (EDA)
plot(datos_ts, main="Monthly Electricity and Gas Production Index", xlab="Year", ylab="Index")
summary(datos_ts)
ggseasonplot(datos_ts, col=rainbow(50), main="Seasonal Plot: Electricity and Gas Production")
acf(datos_ts, main="ACF of Data")
pacf(datos_ts, main="PACF of Data")

# División de los datos en conjunto de entrenamiento y prueba
n <- length(datos_ts)
train_data <- window(datos_ts, end=c(2015, 12))
test_data <- window(datos_ts, start=2016)

# Asegurar que el número de periodos de predicción no excede la longitud de test_data
forecast_horizon <- min(length(test_data), 24)  # Ajusta a un máximo de 24 meses o la longitud de test_data

# Aplicación de modelos de series temporales
# Promedio Móvil
ma_model <- ma(train_data, order=12)
ma_forecast <- forecast(ma_model, h=forecast_horizon)

# Modelo SES
ses_model <- ses(train_data, h=forecast_horizon)  # Ajuste del modelo SES directamente con horizonte

# Modelo de Holt
holt_model <- holt(train_data, h=forecast_horizon)  # Ajuste del modelo Holt directamente con horizonte

# Modelo Holt-Winters
hw_model <- HoltWinters(train_data)
hw_forecast <- forecast(hw_model, h=forecast_horizon)

# Visualización de la predicción para cada modelo
par(mfrow=c(4, 1))
plot(ma_forecast, main="Moving Average Forecast")
lines(test_data, col='red')
plot(ses_model, main="SES Forecast")
lines(test_data, col='red')
plot(holt_model, main="Holt Forecast")
lines(test_data, col='red')
plot(hw_forecast, main="Holt-Winters Forecast")
lines(test_data, col='red')

# Evaluación de cada modelo
models <- list(MA = ma_forecast, SES = ses_model, Holt = holt_model, HW = hw_forecast)
results <- lapply(models, function(model) {
  data.frame(
    Model = deparse(substitute(model)),
    RMSE = accuracy(model, test_data)["Test set", "RMSE"],
    MAPE = accuracy(model, test_data)["Test set", "MAPE"]
  )
})

# Imprimir las métricas de precisión
print(results)

# Restaurar configuración gráfica
par(mfrow=c(1, 1))





# PUNTO 2 
# Instalar y cargar las librerías necesarias
install.packages(c("readxl", "forecast", "tseries", "Metrics", "ggplot2"))
library(readxl)
library(forecast)
library(tseries)
library(Metrics)
library(ggplot2)

# (a) Análisis Exploratorio de Datos (EDA)
# Leer los datos desde el archivo Excel
housing_data <- read_excel("Housing.xls")

# Visualizar las primeras filas del dataset
head(housing_data)

# Resumen estadístico de los datos
summary(housing_data)

# Convertir la columna de fecha a un objeto Date
housing_data$Fecha <- as.Date(housing_data$Fecha, format = "%d/%m/%Y")

# Graficar la serie temporal
ggplot(housing_data, aes(x = Fecha, y = Viviendas)) +
  geom_line() +
  labs(title = "Serie temporal de nuevas viviendas", x = "Fecha", y = "Viviendas")

# Convertir los datos a un objeto ts (time series)
housing_ts <- ts(housing_data$Viviendas, start = c(1959, 1), frequency = 12)

# Graficar la serie temporal
plot(housing_ts, main = "Serie temporal de nuevas viviendas", ylab = "Viviendas", xlab = "Año")

# (b) Identificar el mejor modelo usando train (70%) y test (30%) y las métricas RECM, MAPE, AIC, BIC
# Dividir los datos en conjunto de entrenamiento (70%) y prueba (30%)
train_size <- round(length(housing_ts) * 0.7)
train_ts <- window(housing_ts, end = c(1959 + (train_size-1) %/% 12, (train_size-1) %% 12 + 1))
test_ts <- window(housing_ts, start = c(1959 + train_size %/% 12, train_size %% 12 + 1))

# Ajustar varios modelos y evaluar sus métricas
# Modelo ARIMA
fit_arima <- auto.arima(train_ts)

# Predecir en el conjunto de prueba
forecasts_arima <- forecast(fit_arima, h = length(test_ts))

# Calcular las métricas
recm_arima <- rmse(test_ts, forecasts_arima$mean)
mape_arima <- mape(test_ts, forecasts_arima$mean)
aic_arima <- AIC(fit_arima)
bic_arima <- BIC(fit_arima)

# Mostrar las métricas
cat("ARIMA - RECM:", recm_arima, "MAPE:", mape_arima, "AIC:", aic_arima, "BIC:", bic_arima, "\n")

# (c) Supuestos del modelo
# Graficar los residuos del modelo ARIMA
checkresiduals(fit_arima)

# (d) Realizar un pronóstico para seis meses
forecast_six_months <- forecast(fit_arima, h = 6)

# Mostrar el pronóstico
print(forecast_six_months)

# Graficar el pronóstico
autoplot(forecast_six_months) +
  labs(title = "Pronóstico de seis meses", x = "Fecha", y = "Viviendas") +
  theme_minimal()





# PUNTO 3 
# Instalar y cargar las librerías necesarias
install.packages(c("readxl", "forecast", "tseries", "Metrics", "ggplot2"))
library(readxl)
library(forecast)
library(tseries)
library(Metrics)
library(ggplot2)

# (a) Análisis Exploratorio de Datos (EDA)
# Leer los datos desde el archivo Excel
consumption_data <- read_excel("Consumption.xls")

# Visualizar las primeras filas del dataset
head(consumption_data)

# Resumen estadístico de los datos
summary(consumption_data)

# Crear una columna de fecha basada en Año y Trimestre
consumption_data$Fecha <- as.Date(paste0(consumption_data$Año, "-", 
                                         gsub("Trimestre_", "", consumption_data$Trimestre), "-01"), 
                                  format="%Y-%m-%d")

# Graficar la serie temporal
ggplot(consumption_data, aes(x = Fecha, y = Consumo)) +
  geom_line() +
  labs(title = "Serie temporal del consumo personal", x = "Fecha", y = "Consumo")

# Convertir los datos a un objeto ts (time series)
consumption_ts <- ts(consumption_data$Consumo, start = c(1970, 1), frequency = 4)

# Graficar la serie temporal
plot(consumption_ts, main = "Serie temporal del consumo personal", ylab = "Consumo", xlab = "Año")

# (b) Identificar el mejor modelo usando train (70%) y test (30%) y las métricas RECM, MAPE, AIC, BIC
# Dividir los datos en conjunto de entrenamiento (70%) y prueba (30%)
train_size <- round(length(consumption_ts) * 0.7)
train_ts <- window(consumption_ts, end = c(1970 + (train_size-1) %/% 4, (train_size-1) %% 4 + 1))
test_ts <- window(consumption_ts, start = c(1970 + train_size %/% 4, train_size %% 4 + 1))

# Ajustar varios modelos y evaluar sus métricas
# Modelo ARIMA
fit_arima <- auto.arima(train_ts)

# Predecir en el conjunto de prueba
forecasts_arima <- forecast(fit_arima, h = length(test_ts))

# Calcular las métricas
recm_arima <- rmse(test_ts, forecasts_arima$mean)
mape_arima <- mape(test_ts, forecasts_arima$mean)
aic_arima <- AIC(fit_arima)
bic_arima <- BIC(fit_arima)

# Mostrar las métricas
cat("ARIMA - RECM:", recm_arima, "MAPE:", mape_arima, "AIC:", aic_arima, "BIC:", bic_arima, "\n")

# (c) Supuestos del modelo
# Graficar los residuos del modelo ARIMA
checkresiduals(fit_arima)

# (d) Realizar un pronóstico para tres meses
forecast_three_months <- forecast(fit_arima, h = 3)

# Mostrar el pronóstico
print(forecast_three_months)

# Graficar el pronóstico
autoplot(forecast_three_months) +
  labs(title = "Pronóstico de tres meses", x = "Fecha", y = "Consumo") +
  theme_minimal()
