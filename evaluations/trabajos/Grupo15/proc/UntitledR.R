# Cargar paquetes necesarios
pacman::p_load(
  tidyverse, ggplot2, sjPlot, ggpubr, ggmosaic, haven, dplyr,
  psych, Hmisc, broom, rempsyc, kableExtra, summarytools,
  stargazer, janitor, crosstable, table1, sjlabelled, rcompanion
)



# Desactivar notación científica y limpiar el entorno
options(scipen = 999) # para desactivar notación científica
rm(list = ls()) # para limpiar el entorno de trabajo

# Cargar base de datos
Base_de_datos_Casen_2022_SPSS_18_marzo_2024 <- read_sav("Base de datos Casen 2022 SPSS_18 marzo 2024.sav")

# Verificar nombres de columnas
names(Base_de_datos_Casen_2022_SPSS_18_marzo_2024)

# Seleccionar las variables de interés
data <- Base_de_datos_Casen_2022_SPSS_18_marzo_2024 %>% 
  dplyr::select(participacion_sindicato = o26a,
                edad,
                sexo)

# Reemplazar -88 por NA en todo el dataframe
data[data == -88] <- NA
print(data)

# Verificar los NA en el dataframe
sum(is.na(data))
colSums(is.na(data))

# Omitir las filas con NA
data <- na.omit(data)
dim(data)


#Análisis Univariado

# Resumen de las variables
summarytools::dfSummary(data) %>% 
  summarytools::view(method = "render")

# Descripción estadística
psych::describe(data) %>%
  kable()

# IC para la media de una variable
Publish::ci.mean(data$edad, alpha = 0.05)
Publish::ci.mean(data$sexo, alpha = 0.05)
Publish::ci.mean(data$participacion_sindicato, alpha = 0.05)

# Mostrar estadísticas descriptivas usando sjmisc
data %>% 
  sjmisc::descr(show = c("label", "range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")


#Análisis Bivariado

# Correlación de punto biserial
cor.test(data$edad, as.numeric(data$participacion_sindicato), method = "pearson")

# Crear la tabla de contingencia con variables transformadas temporalmente a factor
tabla_contingencia <- table(factor(data$sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
                            factor(data$participacion_sindicato, levels = c(1, 2), labels = c("Sí", "No")))

# Imprimir la tabla de contingencia
print(tabla_contingencia)

# Calcular Phi
phi_result <- rcompanion::phi(tabla_contingencia)
print(phi_result)



# Tabla cruzada con la participación en sindicato y sexo
tab1 <- data %>%
  group_by(sexo, participacion_sindicato) %>% # Agrupamos por sexo y participacion_sindicato
  summarise(n = n()) %>% # Contamos las observaciones por combinación
  mutate(prop = round((n / sum(n)) * 100, 2)) %>% # Calculamos el porcentaje
  pivot_wider(id_cols = sexo, names_from = participacion_sindicato, values_from = prop) # Convertimos a formato ancho

tab1

# Crear la tabla usando kableExtra
tabla1 <- tab1 %>% 
  kable(format = "html",
        align = "c",
        col.names = c("Sexo", "Sí", "No"),
        caption = "Tabla 1. Participación en sindicato según sexo") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a CASEN 2022.")

tabla1

# Generar tabla cruzada con sjPlot
sjPlot::tab_xtab(data$sexo, data$participacion_sindicato, show.row.prc = TRUE)

# Generar tabla cruzada con summarytools
summarytools::ctable(as.factor(data$sexo), as.factor(data$participacion_sindicato)) %>%
  summarytools::view(method = "render")

# Crear gráfico de barras apiladas
grafico_barras <- ggplot(data, aes(x = sexo, fill = participacion_sindicato)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  labs(title = "Proporción de Participación en Sindicato por Sexo",
       x = "Sexo",
       y = "Proporción",
       fill = "Participación en Sindicato") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")

# Mostrar gráfico
grafico_barras

# Asegurarse de que las columnas sean factores
data$participacion_sindicato <- factor(data$participacion_sindicato, 
                                       levels = c(1, 2), 
                                       labels = c("Sí", "No"))

# Crear el gráfico de caja
grafico_participacion_edad <- ggplot(data, aes(x = participacion_sindicato, y = edad, fill = participacion_sindicato)) +
  geom_boxplot(outlier.shape = 16,  # Personaliza la forma de los valores atípicos
               outlier.colour = "red",  # Cambia el color de los valores atípicos
               outlier.size = 3) +  # Ajusta el tamaño de los valores atípicos
  scale_fill_manual(values = c("lightblue", "lightgreen")) +  # Colores personalizados
  labs(title = "Distribución de Edad según Participación en Sindicato",
       x = "Participación en Sindicato",
       y = "Edad",
       fill = "Participación") +
  theme_minimal(base_size = 15) +  # Usa un tema minimalista y ajusta el tamaño base
  theme(legend.position = "top")  # Coloca la leyenda en la parte superior

# Mostrar el gráfico
grafico_participacion_edad

# Gráfico de dispersión
ggplot(data, aes(x = edad, y = factor(participacion_sindicato), color = factor(participacion_sindicato))) +
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.5) +
  scale_color_manual(values = c("#FF6666", "#66CC66"), labels = c("Sí", "No")) +
  labs(title = "Relación entre Edad y Participación en Sindicato", 
       x = "Edad", 
       y = "Participación en Sindicato", 
       color = "Participación en Sindicato") +
  theme_minimal()

# Convertir la variable sexo en factor
data$sexo <- factor(data$sexo, levels = c(1, 2), labels = c("Hombre", "Mujer"))

# Gráfico de dispersión por sexo y participación en sindicato
ggplot(data, aes(x = edad, y = participacion_sindicato, color = sexo)) +
  geom_point(alpha = 0.6, size = 2) +  # Añadir puntos
  scale_color_manual(values = c("blue", "pink")) +  # Colores personalizados para sexo
  labs(title = "Relación entre Edad y Participación en Sindicato por Sexo",
       x = "Edad",
       y = "Participación en Sindicato",
       color = "Sexo") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")


# Gráfico de densidad para edad, segmentado por sexo y participación en sindicato
grafico_densidad <- ggplot(data, aes(x = edad, fill = interaction(sexo, participacion_sindicato))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral", "lightyellow")) +
  labs(title = "Distribución de Edad por Sexo y Participación en Sindicato",
       x = "Edad",
       y = "Densidad",
       fill = "Sexo y Participación") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")

# Mostrar gráfico
grafico_densidad

# Calcular edad promedio por sexo y participación en sindicato
edad_promedio <- data %>%
  group_by(sexo, participacion_sindicato) %>%
  summarise(edad_promedio = mean(edad, na.rm = TRUE))

# Gráfico de barras para edad promedio
grafico_barras_promedio <- ggplot(edad_promedio, aes(x = interaction(sexo, participacion_sindicato), y = edad_promedio, fill = participacion_sindicato)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  labs(title = "Edad Promedio por Sexo y Participación en Sindicato",
       x = "Sexo y Participación en Sindicato",
       y = "Edad Promedio",
       fill = "Participación en Sindicato") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")

# Mostrar gráfico
grafico_barras_promedio