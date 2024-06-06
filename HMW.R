# Paso 1: Cargar los datos
ruta_archivo <- "C:/Users/USER/Desktop/stadistic2/homework2/datos_restaurante.xlsx"
datos <- read_excel(ruta_archivo)

# Paso 2: Calcular la matriz de varianza y covarianza
MVC <- cov(datos[, -1])  # Excluimos la primera columna (nombres de clientes)

# Paso 3: Estandarizar los datos y calcular la matriz de varianza y covarianza
datos_estandarizados <- scale(datos[, -1])  # Estandarizamos excluyendo la primera columna
MVC_estandarizada <- cov(datos_estandarizados)

# Paso 4: Calcular los valores y vectores propios
valores_propios <- eigen(MVC)$values
valores_propios_estandarizado <- eigen(MVC_estandarizada)$values
vectores_propios <- eigen(MVC)$vectors
vectores_propios_estandarizado <- eigen(MVC_estandarizada)$vectors

# Paso 5: Calcular Y1, Y2 y Cor(Y1, Y2)

# (a) Las expectativas de las componentes principales son cero para datos estandarizados
E_Y1 <- 0
E_Y2 <- 0

# (b) Las varianzas de las componentes principales son los valores propios correspondientes
V_Y1 <- valores_propios[1]
V_Y2 <- valores_propios[2]

# (c) Las componentes principales son ortogonales; por lo tanto, su correlación es cero
Cor_Y1_Y2 <- 0

# Imprimir resultados
cat("E(Y1):", E_Y1, "\n")
cat("E(Y2):", E_Y2, "\n")
cat("V(Y1):", V_Y1, "\n")
cat("V(Y2):", V_Y2, "\n")
cat("Cor(Y1, Y2):", Cor_Y1_Y2, "\n")
