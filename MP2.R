library(tidyverse)
library(corrplot)
library(factoextra)

# Cargar los datos desde un archivo TSV
data <- read_tsv('C:/Users/USER/Desktop/stadistic2/miniproyecto2/participants_iowa.tsv')

# Convertir variables a factores o numéricos según sea necesario
data <- data %>%
  mutate(
    group = as.factor(group),
    gender = as.factor(gender),
    age = as.numeric(age),
    moca_total = as.numeric(moca_total), 
    monthssincepddiagnosis = as.numeric(monthssincepddiagnosis),
    updrsiii = as.numeric(updrsiii)
  )

# Resumen estadístico de la edad por grupo
age_summary <- data %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE),
    min = min(age, na.rm = TRUE),
    q1 = quantile(age, 0.25, na.rm = TRUE),
    median = median(age, na.rm = TRUE),
    q3 = quantile(age, 0.75, na.rm = TRUE),
    max = max(age, na.rm = TRUE)
  )

# Imprimir el resumen estadístico
print(age_summary)

# Gráfico de distribución de edad por grupo
ggplot(data, aes(x = group, y = age, fill = group)) +
  geom_boxplot() +
  labs(title = "Distribución de la edad por grupo", x = "Grupo", y = "Edad")

# Gráfico de distribución de género por grupo
ggplot(data, aes(x = group, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de género por grupo", x = "Grupo", y = "Cantidad")

# Comparación de puntuaciones MoCA por grupo
ggplot(data, aes(x = group, y = moca_total, fill = group)) +
  geom_boxplot() +
  labs(title = "Puntuaciones MoCA por grupo", x = "Grupo", y = "Puntuación MoCA")

# Análisis de la duración de la enfermedad en el grupo PD, filtrando NA
valid_pd_data <- data %>%
  filter(group == "pd", !is.na(monthssincepddiagnosis))

ggplot(valid_pd_data, aes(x = monthssincepddiagnosis)) +
  geom_histogram(fill = 'blue', bins = 30) +
  labs(title = "Distribución de la duración de la enfermedad en el grupo PD", x = "Meses desde el diagnóstico", y = "Frecuencia")

# Análisis de severidad de la enfermedad (UPDRS-III) en el grupo PD, filtrando NA
valid_updrs_data <- data %>%
  filter(group == "pd", !is.na(updrsiii))

ggplot(valid_updrs_data, aes(x = updrsiii)) +
  geom_histogram(fill = 'red', bins = 30) +
  labs(title = "Distribución de la puntuación UPDRS-III en el grupo PD", x = "Puntuación UPDRS-III", y = "Frecuencia")

# Crear y visualizar la matriz de correlaciones
data_for_corr <- data %>%
  select(age, moca_total, monthssincepddiagnosis, updrsiii) %>%
  drop_na()

cor_matrix <- cor(data_for_corr)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")



#PCA
# Nos asefuramos de que las variables sean numéricas y filtrar los NA
data_for_pca <- data %>%
  select(age, moca_total, monthssincepddiagnosis, updrsiii) %>% 
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

# Realizar PCA
pca_result <- prcomp(data_for_pca, scale. = TRUE) # scale. = TRUE para estandarizar los datos

# Nombres de los componentes principales
pca_names <- paste("PC", 1:ncol(data_for_pca), sep = "")

# Asignar nombres a los componentes
names(pca_result$rotation) <- pca_names

# Resumen de resultados
summary(pca_result)

# Visualización de PCA
fviz_pca_ind(pca_result, 
             geom.ind = "point",
             col.ind = 'black', 
             addEllipses = TRUE, 
             ellipse.level = 0.95)


# Incluimos las puntuaciones de MoCA y UPDRS-III, y el tiempo desde el diagnóstico para el índice

data_for_index <- data %>%
  filter(group == "pd") %>% 
  mutate_at(vars(moca_total, monthssincepddiagnosis, updrsiii), scale) %>%
  drop_na() 

# Crea el índice como la suma de las variables estandarizadas
data_for_index <- data_for_index %>%
  mutate(index = rowSums(select(., moca_total, monthssincepddiagnosis, updrsiii))) 