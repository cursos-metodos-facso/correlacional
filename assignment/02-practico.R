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

set.seed(03092024) # Fijar una semilla para reproducibilidad

proc_casen <- proc_casen %>%
  select(universitaria, ytrabajocor) %>%
  # Solo para que la tabla se vea mejor, vamos a generar una variable categórica
  mutate(
    universitaria_cat = case_when(
      universitaria == 1 ~ "Tiene educación universitaria",
      universitaria == 0 ~ "No tiene educación universitaria",
      TRUE ~ NA
    )
  ) %>%
  filter(!is.na(ytrabajocor)) %>% # Filtramos por los casos que no tengan NA en la variable de ingresos
  sample_n(1500) # Generamos una muestra aleatoria de 1500 casos

# b) Tabla descriptiva

proc_casen %>%
  group_by(universitaria_cat) %>%
  summarise(
    Obs. = n(),
    Media = mean(ytrabajocor),
    SD = sd(ytrabajocor)
  ) %>%
  kable(format = "markdown")

# c) Resolver con 5 pasos inferencia

# 1 Formulación hipótesis

# Para este caso, ambos tipos de hipótesis (direccional y no direccional) podrían ser argumentadas.
# Para mantener la simplicidad, vamos a contrastar una hipótesis no direccional.
# Esto quiere decir que queremos comprobar si en la población existen diferencias en los
# ingresos medios entre quienes tienen educación universitaria y quienes no.
# En lenguaje de contraste de hipótesis, HA y H0 serían así:

## HA: X universitaria - X nouniversitaria ≠ 0
## H0: X universitaria - X nouniversitaria = 0

# 2, 3 y 4 en R

# Prueba t no direccional al 95% se confianza
test95_ej3 <- t.test(proc_casen$ytrabajocor ~ proc_casen$universitaria,
                 alternative = "two.sided",
                 conf.level = 0.95,
                 var.equal = T
                 )
test95_ej3

stats.table <- tidy(test95_ej3, conf_int = T)
nice_table(stats.table, broom = "t.test")

gginference::ggttest(test95_ej3)

# Prueba t no direccional al 99% se confianza
test99_ej3 <- t.test(proc_casen$ytrabajocor ~ proc_casen$universitaria,
                     alternative = "two.sided",
                     conf.level = 0.99,
                     var.equal = T
                     )
test99_ej3

stats.table <- tidy(test99_ej3, conf_int = T)
nice_table(stats.table, broom = "t.test")

gginference::ggttest(test99_ej3)

# 5 Interpretacion

media_univ <- round(mean(proc_casen %>% filter(universitaria == 1) %>% pull(ytrabajocor)))
sd_univ <- round(sd(proc_casen %>% filter(universitaria == 1) %>% pull(ytrabajocor)))

media_nouniv <- round(mean(proc_casen %>% filter(universitaria == 0) %>% pull(ytrabajocor)))
sd_nouniv <- round(sd(proc_casen %>% filter(universitaria == 0) %>% pull(ytrabajocor)))

df95 <- test95_ej3$parameter[[1]]
t95 <- test95_ej3$statistic[[1]]
p95 <- test95_ej3$p.value[[1]]

## Efecto, significancia y decisión de rechazo (o no)
interpretacion_ej3 <- paste0(
  "Primero, para determinar si existen diferencias en los ingresos medios entre quienes tienen educación universitaria",
  "(X = ", media_univ, "; SD = ", sd_univ, ") y quienes no (X = ", media_nouniv, "; SD = ", sd_nouniv, ")",
  " aplicamos una prueba t no direccional para dos muestras independientes al 95% de confianza.",
  " Los resultados de la prueba entregan evidencia para rechazar la hipótesis nula H0 sobre no diferencias en las medias de ingresos, t(", df95, ") =", t95, "; p =", p95
  )

print(interpretacion_ej3)
