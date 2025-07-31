pacman::p_load(tidyverse, # Manipulacion datos
               sjPlot, # Graficos y tablas
               sjmisc, # Descriptivos
               corrplot, # Correlaciones
               psych,
               dyplr, # Test estadísticos
               kableExtra,
               haven,
               broom) # Tablas
options(scipen = 999) # para desactivar notacion cientifica

rm(list = ls()) # para limpiar el entorno de trabajo

load("~/Library/Mobile Documents/com~apple~CloudDocs/estadisticafinal/Base de datos EBS 2023.RData")

# Cargar y seleccionar variables de interés
ebs <- EBS_2023_vp %>%
  dplyr::select(
    sg01,          # Sexo
    glosa_area,    # Tipo de residencia (urbano/rural)
    ss7_c,         # Ansiedad
    oo7_a1,        # Riesgo de perder el trabajo
    yy2,           # Dificultad para llegar a fin de mes
    tdomestico     # Tiempo dedicado al trabajo doméstico (minutos)
  )

# Reemplazar valores no válidos (-99, -88, -89) con NA
ebs <- ebs %>%
  dplyr::mutate(across(everything(), ~ ifelse(.x %in% c(-99, -88, -89), NA, .x)))

# Recodificar 'glosa_area' (Tipo de residencia) a valores numéricos
ebs <- ebs %>%
  dplyr::mutate(glosa_area = case_when(
    glosa_area == "Urbano" ~ "1",  # Urbano como 1
    glosa_area == "Rural" ~ "2",   # Rural como 2
    TRUE ~ NA_character_           # Otros valores como NA
  ))

# Convertir todas las columnas a formato numérico
ebs <- ebs %>%
  dplyr::mutate(across(everything(), ~ as.numeric(as.character(.x))))

# Crear una nueva variable: tdomestico_horas (Trabajo doméstico en horas)
ebs <- ebs %>%
  dplyr::mutate(tdomestico_horas = tdomestico / 60)

# Recodificar 'yy2' (Dificultad para llegar a fin de mes) invirtiendo los valores
ebs <- ebs %>%
  dplyr::mutate(yy2 = dplyr::case_when(
    yy2 == 1 ~ 5,    # Con mucha dificultad -> 5
    yy2 == 2 ~ 4,    # Con dificultad -> 4
    yy2 == 3 ~ 3,    # Ni con facilidad ni con dificultad -> 3
    yy2 == 4 ~ 2,    # Con facilidad -> 2
    yy2 == 5 ~ 1,    # Con mucha facilidad -> 1
    TRUE ~ NA_real_  # Otros valores como NA
  ))

# Eliminar casos donde tdomestico_horas sea mayor a 19
ebs <- ebs %>%
  filter(tdomestico_horas <= 19)

frq(ebs$tdomestico_horas)

# Verificar las transformaciones aplicadas
str(ebs)
summary(ebs)

# Crear tabla descriptiva
tabla_descriptivos <- ebs %>%
  select(sg01, glosa_area, ss7_c, oo7_a1, yy2, tdomestico) %>%
  sjmisc::descr(show = c("label", "range", "mean", "sd", "NA.prc", "n")) %>%
  as.data.frame()

# Crear tabla con ggtexttable
tabla_plot <- ggtexttable(
  tabla_descriptivos,
  rows = NULL, # Sin nombres de filas
  theme = ttheme("light")
)

# Mostrar tabla como un gráfico
print(tabla_plot)

# Matriz de correlaciones 
M <- cor(ebs, use = "complete.obs") 
M

sjPlot::tab_corr(ebs, 
                 triangle = "lower")

#correlación entre ordinales
cor.test(ebs$ss7_c, ebs$yy2, method = "spearman", use = "pairwise.complete.obs")
cor.test(ebs$ss7_c, ebs$oo7_a1, method = "spearman", use = "pairwise.complete.obs")

#correlación punto biserial 
cor.test(ebs$ss7_c, ebs$sg01, method = "pearson", use = "pairwise.complete.obs")
cor.test(ebs$ss7_c, ebs$tdomestico_horas, method = "pearson", use = "pairwise.complete.obs")
cor.test(ebs$ss7_c, ebs$glosa_area, method = "pearson", use = "pairwise.complete.obs")


#Se realiza decodificación para hacer una correlación tetratorica y profundizar el análisis de la relación entre sexo y nivel de ansiedad.

# Recodificación de la variable ss7_c a una nueva variable ss7_c_recode
# Recodificación de la variable ss7_c a una nueva variable ss7_c_recode
ebs <- ebs %>%
  mutate(
    ss7_c_recode = case_when(
      ss7_c == 1 ~ 1,  # Baja ansiedad: Nunca
      ss7_c == 2 ~ 1,  # Baja ansiedad: Algunos días
      ss7_c == 3 ~ 2,  # Alta ansiedad: Más de la mitad de los días
      ss7_c == 4 ~ 2,  # Alta ansiedad: Casi todos los días
      TRUE ~ NA_real_  # Asignar NA en caso de valores faltantes
    ))

# cálculo de correlación
tetratorica_ansiedad_sexo <- ebs %>%
  dplyr::select(ss7_c_recode, sg01)

psych::tetrachoric(tetratorica_ansiedad_sexo, na.rm = T)

chi_results1 <- chisq.test(ebs$ss7_c_recode, ebs$sg01)

chi_results1


chi_results1 <- chisq.test(ebs$ss7_c_recode, ebs$sg01)
n <- na.omit(ebs %>% select(ss7_c_recode, ebs$sg01)) %>% nrow()
# Cálculo de Phi
phi <- sqrt(chi_results1$statistic / n)
phi

sjPlot::tab_xtab(ebs$ss7_c_recode, ebs$sg01, show.row.prc = TRUE)

# ansiedad y capacidad de llegar a fin de mes
ggplot(subset(ebs, !is.na(ss7_c) & !is.na(yy2)), aes(x = factor(ss7_c), fill = factor(yy2))) +
  geom_bar(position = "dodge", color = "white") +
  scale_fill_manual(
    values = c("1" = "#FADADD", "2" = "#F5C6E0", "3" = "#D8BFD8", "4" = "#C3B1E1", "5" = "#B39EB5"),
    labels = c(
      "1" = "Con mucha facilidad",
      "2" = "Con facilidad",
      "3" = "Ni con dificultad ni con facilidad",
      "4" = "Con dificultad",
      "5" = "Con mucha dificultad"
    )
  ) +
  labs(
    title = "Relación entre la frecuencia de ansiedad y la capacidad para llegar a fin de mes",
    x = "Frecuencia con la que se experimenta ansiedad",
    y = "Número de Personas",
    fill = "Facilidad para llegar a fin de mes",
    caption = "Fuente: EBS (2023), de elaboración propia"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Nunca",
    "2" = "Algunos días",
    "3" = "Más de la mitad de los días",
    "4" = "Casi todos los días"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#5A3E6B"),
    axis.text.x = element_text(size = 10, color = "#5A3E6B"),
    axis.text.y = element_text(size = 10, color = "#5A3E6B"),
    axis.title = element_text(face = "bold", color = "#5A3E6B"),
    legend.title = element_text(face = "bold", color = "#5A3E6B"),
    legend.text = element_text(color = "#5A3E6B"),
    plot.caption = element_text(hjust = 1, size = 8, color = "#5A3E6B")
  )


# Gráfico de barras para ansiedad y capacidad de llegar a fin de mes
ggplot(subset(ebs, !is.na(ss7_c) & !is.na(oo7_a1)), aes(x = factor(ss7_c), fill = factor(oo7_a1))) +
  geom_bar(position = "dodge", color = "white") +  # Eliminar NA solo en el gráfico
  scale_fill_manual(
    values = c("1" = "#FADADD", "2" = "#F5C6E0", "3" = "#D8BFD8", "4" = "#C3B1E1", "5" = "#B39EB5"),
    labels = c(
      "1" = "Muy en desacuerdo",
      "2" = "En desacuerdo",
      "3" = "Ni de acuerdo ni en desacuerdo",
      "4" = "De acuerdo",
      "5" = "Muy de acuerdo"
    )
  ) +
  labs(
    title = "Relación entre la frecuencia de ansiedad y la percepción de inseguridad laboral",
    x = "Frecuencia con la que se experimenta ansiedad",
    y = "Número de Personas",
    fill = "Percepción de inseguridad laboral",
    caption = "Fuente: EBS (2023), de elaboración propia"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Nunca",
    "2" = "Algunos días",
    "3" = "Más de la mitad de los días",
    "4" = "Casi todos los días"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#5A3E6B"),
    axis.text.x = element_text(size = 10, color = "#5A3E6B"),
    axis.text.y = element_text(size = 10, color = "#5A3E6B"),
    axis.title = element_text(face = "bold", color = "#5A3E6B"),
    legend.title = element_text(face = "bold", color = "#5A3E6B"),
    legend.text = element_text(color = "#5A3E6B"),
    plot.caption = element_text(hjust = 1, size = 8, color = "#5A3E6B")
  )



# ansiedad por sexo
ggplot(subset(ebs, !is.na(ss7_c) & !is.na(sg01)), aes(x = factor(ss7_c), fill = factor(sg01))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("1" = "#440154", "2" = "#C3B1E1"), # Colores morado y rosado
    labels = c("1" = "Masculino", "2" = "Femenino")
  ) +
  labs(
    title = "Distribución de la frecuencia de ansiedad por sexo",
    x = "Frecuencia de Ansiedad (ss7_c)",
    y = "Frecuencia",
    fill = "Sexo",
    caption = "Fuente: EBS (2023), de elaboración propia"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Nunca",
    "2" = "Algunos días",
    "3" = "Más de la mitad de los días",
    "4" = "Casi todos los días"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#440154"),
    axis.text.x = element_text(size = 10, color = "#440154"),
    axis.text.y = element_text(size = 10, color = "#440154"),
    axis.title = element_text(face = "bold", color = "#440154"),
    legend.title = element_text(face = "bold", color = "#440154"),
    legend.text = element_text(color = "#440154"),
    plot.caption = element_text(hjust = 1, size = 8, color = "#440154")
  )


#tdomestico histograma

ggplot(ebs %>% filter(!is.na(sg01) & !is.na(tdomestico_horas)), 
       aes(x = tdomestico_horas, fill = factor(sg01))) +
  geom_histogram(position = "dodge", bins = 30, color = "white") + 
  scale_fill_manual(
    values = c("1" = "#5A3E6B", "2" = "#D8BFD8"),
    labels = c("1" = "Masculino", "2" = "Femenino")
  ) +
  labs(
    title = "Histograma del tiempo dedicado al trabajo doméstico por sexo",
    x = "Tiempo dedicado al trabajo doméstico (horas)",
    y = "Número de Personas",
    fill = "Sexo",
    caption = "Fuente: EBS (2023), de elaboración propia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#4B0082"),
    axis.text.x = element_text(size = 10, color = "#4B0082"),
    axis.text.y = element_text(size = 10, color = "#4B0082"),
    axis.title = element_text(face = "bold", color = "#4B0082"),
    legend.title = element_text(face = "bold", color = "#4B0082"),
    legend.text = element_text(color = "#4B0082"),
    plot.caption = element_text(hjust = 1, size = 8, color = "#4B0082")
  )

