library(tidyverse)
library(factoextra)

# Cargar los datos
data <- read_tsv('C:/Users/USER/Desktop/stadistic2/miniproyecto2/participants_iowa.tsv')

# Asegurarse de que las variables sean numéricas y filtrar los NA
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
             geom.ind = "point", # para usar solo puntos sin texto
             col.ind = 'black', # Color de los puntos
             addEllipses = TRUE, # Añadir elipses de confianza
             ellipse.level = 0.95) # Nivel de confianza para las elipses
