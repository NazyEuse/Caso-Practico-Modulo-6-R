
# Cargamos Tidyverse
library(tidyverse)

# Importamos la base de datos Titanicv2.csv
titanic <- read.csv("Titanicv2.csv")

# Exploramos los datos
glimpse(titanic)
summary(titanic)

# Seleccionamos las columnas más importantes para el análisis
titanic_clean <- titanic %>%
  select(Survived, Pclass, Sex, Age, Fare, Embarked) %>%
  mutate(Survived = factor(Survived, levels = c(0, 1), labels = c("No", "Yes")),
         Pclass = factor(Pclass, levels = c(1, 2, 3), labels = c("Upper Class", "Middle Class", "Lower Class")),
         Sex = as.factor(Sex))

# Comprobamos los datos seleccionados
glimpse(titanic_clean)

# Resumen por clase y género
summary_table <- titanic_clean %>%
  group_by(Survived, Pclass, Sex) %>%
  summarise(Age_Mean = mean(Age, na.rm = TRUE),
            Fare_Median = median(Fare, na.rm = TRUE),
            Count = n())

print(summary_table)

# Histograma de edad
ggplot(titanic_clean, aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribución de la Edad de los Pasajeros", x = "Edad", y = "Frecuencia")

# Gráfico de barras para supervivencia por clase y género
ggplot(titanic_clean, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  facet_wrap(~Sex) +
  labs(title = "Supervivencia por Clase Socioeconómica y Género", x = "Clase Socioeconómica", y = "Proporción")

# Diagrama de caja para Fare (Costo de Ticket) vs Supervivencia
ggplot(titanic_clean, aes(x = Survived, y = Fare, fill = Survived)) +
  geom_boxplot() +
  labs(title = "Relación entre Costo de Ticket y Supervivencia", x = "Supervivencia", y = "Costo de Ticket (Fare)")

