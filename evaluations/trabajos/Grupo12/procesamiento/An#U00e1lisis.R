#--------------------#
#     Análisis       #
#--------------------#

# Cargar librerías ####
pacman::p_load(tidyverse, # manipulación de datos
               sjPlot, # hacer gráficos
               sjmisc, # descriptivos
               rstatix, # test estadísticos
               broom, # tablas
               corrplot, # correlaciones
               psych, # test estadísticos
               kableExtra,
               haven,
               sjlabelled,
               here,
               ggmosaic,
               ggplot2) # tablas

options(scipen = 999) # para desactivar notacion cientifica

# Base de datos ####
dataCEP <- readRDS("output/dataCEP.rds")

# Cargar el objeto dataCEP desde el archivo RDS
dataCEP <- readRDS("output/dataCEP.rds")


# Gráfico univariado ####
g1 <- ggplot(dataCEP, aes(x=conf_gob))+
  geom_bar(position="dodge", fill = "steelblue")+                     # conf_gob
  labs(title = "Confianza en Instituciones políticas: El Gobierno",
       x = "Confianza en el Gobierno",
       y = "Frecuencia",
       fill = "Confianza en el Gobierno")+
  theme_minimal()
g1
ggsave(here("input", "img", "grafico1.png"), width = 9.5 , height = 5.5 )

g2 <- ggplot(dataCEP, aes(x=situacion_politica))+
  geom_bar(position="dodge", fill = "hotpink")+                     # situacion_politica
  labs(title = "Situación política",
       x = "Situación política",
       y = "Frecuencia",
       fill = "Situación política")+
  theme_minimal()
g2
ggsave(here("input", "img", "grafico2.png"), width = 9.5 , height = 5.5 )

g3 <- ggplot(dataCEP, aes(x=situacion_econ))+
  geom_bar(position="dodge", fill = "tomato")+                     # situacion_politica
  labs(title = "Situación económica",
       x = "Situación económica",
       y = "Frecuencia",
       fill = "Situación económica")+
  theme_minimal()
g3
ggsave(here("input", "img", "grafico3.png"), width = 9.5 , height = 5.5 )

g4 <- ggplot(dataCEP, aes(x=area))+
  geom_bar(position="dodge", fill = "slateblue")+                     # situacion_politica
  labs(title = "Área urbano/rural",
       x = "Área urbano/rural",
       y = "Frecuencia",
       fill = "Área urbano/rural")+
  theme_minimal()
g4
ggsave(here("input", "img", "grafico4.png"), width = 9.5 , height = 5.5 )

g5 <- ggplot(dataCEP, aes(x=nivel_educ))+
  geom_bar(position="dodge", fill = "plum1")+                     # situacion_politica
  labs(title = "Nivel educacional",
       x = "Nivel educacional",
       y = "Frecuencia",
       fill = "Nivel educacional")+
  theme_minimal()
g5
ggsave(here("input", "img", "grafico5.png"), width = 9.5 , height = 5.5 )

# Graficos bivariados ####

# H1. 
g6 <- ggplot(dataCEP, aes(x = factor(situacion_econ), y = conf_gob)) +
  geom_jitter(color = "purple") +
  labs(x = "Percepción de la situación económica", y = "Confianza en el gobierno") +
  theme_minimal()
g6
ggsave(here("input", "img", "grafico6.png"), width = 9.5 , height = 5.5 )

# H2.
g7 <- ggplot(dataCEP, aes(x = factor(situacion_politica), y = conf_gob)) +
  geom_jitter(color = "violetred") +
  labs(x = "Percepción de la situación política", y = "Confianza en el gobierno") +
  theme_minimal()
g7
ggsave(here("input", "img", "grafico7.png"), width = 9.5 , height = 5.5 )

# H3.
g8 <- ggplot(dataCEP, aes(x = factor(area), fill = factor(conf_gob))) +
  geom_bar(position = "fill") +
  labs(x = "Zona (0=Urbana, 1=Rural)", y = "Proporción", fill = "Confianza en el gobierno") +
  theme_minimal()
g8
ggsave(here("input", "img", "grafico8.png"), width = 9.5 , height = 5.5 )

# H4.
g9 <- ggplot(dataCEP, aes(x = factor(conf_gob), y = nivel_educ, fill = factor(conf_gob))) +
  geom_boxplot() +
  scale_fill_manual(values = c("plum1", "olivedrab2", "steelblue3", "khaki")) + # Personaliza colores
  labs(x = "Confianza en el gobierno", y = "Nivel educativo", fill = "Confianza") +
  theme_minimal()
g9
ggsave(here("input", "img", "grafico9.png"), width = 9.5 , height = 5.5 )


g10 <-ggplot(dataCEP, aes(x = conf_gob, y = nivel_educ)) +
  geom_point(alpha = 0.6, color = "plum1") + # Puntos del scatterplot
  geom_smooth(method = "lm", se = TRUE, color = "violetred", linetype = "dashed") + # Línea de tendencia
  labs(
    title = "Relación entre Confianza en el Gobierno y Nivel Educativo",
    x = "Confianza en el Gobierno",
    y = "Nivel Educativo"
  ) +
  theme_minimal()
g10
ggsave(here("input", "img", "grafico10.png"), width = 9.5 , height = 5.5 )

