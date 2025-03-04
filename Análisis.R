# Puede averiguar más sobre el marco de datos ejecutando la siguiente línea:

?titanic_train

#Previamente debe tener las librerías instaladas

# Cargar librerías
library(titanic)
library(ggplot2)
library(dplyr)
library(psych)

# Cargar el dataset
data <- titanic::titanic_train
attach(data)

# Análisis de variables cuantitativas (Edad - Age)

# Estadísticas descriptivas
summary(data$Age)
quantile(Age, c(0.2, 0.4, 0.6, 0.8),na.rm = TRUE)

# Medidas adicionales
edad_stats <- data %>%
  summarise(
    Media = mean(Age, na.rm = TRUE),
    Mediana = median(Age, na.rm = TRUE),
    Moda = as.numeric(names(sort(table(Age), decreasing = TRUE)[1])),
    Varianza = var(Age, na.rm = TRUE),
    DesviacionEstandar = sd(Age, na.rm = TRUE),
    Rango = max(Age, na.rm = TRUE) - min(Age, na.rm = TRUE),
    Asimetria = skew(Age, na.rm = TRUE),
    )

print(edad_stats)

# Gráfico de distribución de edad
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de la Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()


# Análisis de variables cualitativas (Sexo - Sex, Embarque - Embarked, Sobrevive - Survived)

# Tabla de frecuencias
sexo_freq <- table(data$Sex)
sexo_freq

embarque_freq <- table(data$Embarked)
embarque_freq 

sobrevive_freq <- table(data$Survived)
sobrevive_freq

# Convertir a data frame para ggplot
sexo_df <- as.data.frame(sexo_freq)
embarque_df <- as.data.frame(embarque_freq)
sobrevive_df <- as.data.frame(sobrevive_freq)

# Gráfico circular para Sexo
ggplot(sexo_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  labs(title = "Frecuencia de Sexo") +
  theme_void() + # Elimina los ejes para mejorar la visualización
  scale_fill_manual(values = c( "pink", "blue")) # Colores opcionales


# Gráfico de barras para Embarque
ggplot(embarque_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencia de Embarque", x = "Puerto de Embarque", y = "Frecuencia") +
  theme_minimal()

# Gráfico de barras para Supervivencia
ggplot(sobrevive_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Supervivencia en el Titanic", x = "Sobrevive (0 = No, 1 = Sí)", y = "Frecuencia") +
  theme_minimal()

##  Análisis comparativo
### Distribución de Edad por Supervivencia


ggplot(data, aes(x = factor(Survived), y = Age, fill = factor(Survived))) +
  geom_boxplot() +
  labs(title = "Distribución de Edad por Supervivencia", x = "Sobrevive (0 = No, 1 = Sí)", y = "Edad") +
  theme_minimal()

### Proporción de Supervivencia por Sexo

ggplot(data, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de Supervivencia por Sexo", x = "Sexo", y = "Proporción") +
  scale_fill_manual(values = c("red", "green"), labels = c("No", "Sí")) +
  theme_minimal()

### Supervivencia por Puerto de Embarque

ggplot(data, aes(x = Embarked, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de Supervivencia por Puerto de Embarque", x = "Puerto de Embarque", y = "Proporción") +
  scale_fill_manual(values = c("red", "green"), labels = c("No", "Sí")) +
  theme_minimal()

