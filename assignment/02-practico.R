# Código: Práctico 2 Estadística Correlacional 2024
# Fecha: 03-09-2024
# Autor: Equipo Docente

# 1. Cargar librerías -----------------------------------------------------

# install.packages("pacman")

pacman::p_load(dplyr, # Manipulacion datos
               gginference, # Visualizacion
               rempsyc, # Reporte
               kableExtra, # Tablas
               broom) # Varios

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

# 2. Cargar datos ---------------------------------------------------------

load(url("https://github.com/cursos-metodos-facso/datos-ejemplos/raw/main/proc_casen.RData")) #Cargar base de datos

# 3. Explorar datos -------------------------------------------------------

names(proc_casen) # Nombre de columnas
dim(proc_casen) # Dimensiones

# Ejercicio 1 ---------------------------------------------------------

# a) Procesamiento

casen_subset <- proc_casen %>%
  select(sexo, ytrabajocor) %>% # seleccionamos
  sample_n(1500) # extraemos una muestra de 1500 casos

casen_subset <- na.omit(casen_subset) # eliminamos casos perdidos (listwise)

# b) Tabla descriptiva

casen_subset %>%
  dplyr::group_by(sexo) %>% # se agrupan por la variable categórica
  dplyr::summarise(Obs. = n(),
                   Promedio = mean(ytrabajocor, na.rm=TRUE),
                   SD = sd(ytrabajocor, na.rm=TRUE)) %>% # se agregan las operaciones a presentar en la tabla
  kable(format = "markdown") # se genera la tabla

# c) Resolver con 5 pasos inferencia

# 1 Formulación hipótesis

## HA: X hombres - X mujeres > 0
## H0: X hombres - X mujeres ≤ 0

# 2, 3 y 4 en R

test_ej1 <- t.test(casen_subset$ytrabajocor ~ casen_subset$sexo,
                   alternative = "greater",
                   conf.level = 0.95)

test_ej1

stats.table <- tidy(test_ej1, conf_int = T)
nice_table(stats.table, broom = "t.test")

gginference::ggttest(test_ej1)

# 5 Interpretacion

## Efecto, significancia y decisión de rechazo (o no)

# Ejercicio 2 ----------------------------------------------

# a) Procesamiento

goldin_data <- proc_casen %>%
  dplyr::select(ocupado, sexo, ytrabajocor, hijo) %>%
  dplyr::filter(ocupado == 1 & sexo == 2) %>%
  sample_n(1500) %>%
  na.omit()# creamos subset con solo mujeres ocupadas

# b) Tabla descriptiva

goldin_data %>%
  dplyr::group_by(hijo) %>%
  dplyr::summarise(Obs. = n(),
                   Media = mean(ytrabajocor, na.rm = T),
                   DS = sd(ytrabajocor, na.rm = T)) %>%
  kable(format = "markdown") # hacemos la tabla

# c) Resolver con 5 pasos inferencia

# 1 Formulación hipótesis

## HA: X sin hijos - X con hijos < 0
## H0: X sin hijos - X con hijos ≥ 0

# 2, 3 y 4 en R

test_ej2 <- t.test(goldin_data$ytrabajocor ~ goldin_data$hijo,
                   alternative = "less",
                   conf.level = 0.95)

test_ej2

stats.table <- tidy(test_ej2, conf_int = T)
nice_table(stats.table, broom = "t.test")

gginference::ggttest(test_ej2)

# 5 Interpretacion

## Efecto, significancia y decisión de rechazo (o no)

# Ejercicio 3 ----------------------------------------------

# a) Procesamiento

# b) Tabla descriptiva

# c) Resolver con 5 pasos inferencia

# 1 Formulación hipótesis

## HA:
## H0:

# 2, 3 y 4 en R

# 5 Interpretacion

## Efecto, significancia y decisión de rechazo (o no)

