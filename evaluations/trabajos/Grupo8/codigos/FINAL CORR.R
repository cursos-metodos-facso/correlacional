
# ---- INFORME FINAL ESTADÍSTICA CORRELACIONAL ----

# Cargar librerías

library(haven)
library(pacman)
library(httr)
library(vcd)
pacman::p_load(tidyverse, # para sintaxis
               ggplot2,   # para gráficos
               car,       # para recodificar
               psych,     # para analizar datos
               sjmisc,    # para analizar datos
               srvyr,     # para estimación de IC y ponderadores
               Publish,  # para IC)
               dplyr, # Manipulacion datos
               gginference, #Visualización
               rempsyc, # Reporte
               kableExtra, # Tablas
               broom,   # Varios
               sjPlot, # Graficos
               rstatix, # Test estadísticos
               corrplot, # Correlaciones
               knitr, # Render y tablas
               summarytools, # Tablas
               stargazer, # Tablas
               janitor, # Tablas y formateo
               crosstable, # Tablas
               table1,  # Tablas
               ggpubr, # Gráficos
               ggmosaic) # Gráficos

options(scipen = 999) # para desactivar notacion cientifica


# Cargar BBDD

EFH2021 <- read_dta("datos/EFH2021.dta")
View(EFH2021)


# ANÁLISIS DESCRIPTIVO UNIVARIADO ----

# Variable dependiente: Uso de tarjetas con criptomonedas (u_tcripto)

# Variable independiente 1: Valor total de los activos financieros (act_fin)

# Variable independiente 2: Estrato de ingreso efectivo del hogar (estrato)

# Variable independiente 3: Carga financiera total de la deuda no hipotecaria



# USO DE TARJETAS CON CRIPTOMONEDAS

# Frecuencias y Porcentajes
frecuencias_cripto <- table(EFH2021$u_tcripto)
frecuencias_cripto

porcentajes_cripto <- prop.table(frecuencias_cripto) * 100
porcentajes_cripto


# Tabla con frecuencias y porcentajes
tabla_resultados_cripto <- data.frame(
  Categoria = c("No usa", "Usa"),
  Frecuencia = frecuencias_cripto,
  Porcentaje = porcentajes_cripto
)

tabla_resultados_cripto

kable(tabla_resultados_cripto, caption = "Distribución del Uso de Criptomonedas en los Hogares")


# VALOR TOTAL DE LOS ACTIVOS FINANCIEROS

# Resumen básico
summary(EFH2021$act_fin)

# Descripción completa
describe(EFH2021$act_fin)

# Estadísticos básicos
media_actfin <- mean(EFH2021$act_fin, na.rm = TRUE)
desviacion_estandar_actfin <- sd(EFH2021$act_fin, na.rm = TRUE)
mediana_actfin <- median(EFH2021$act_fin, na.rm = TRUE)
minimo_actfin <- min(EFH2021$act_fin, na.rm = TRUE)
maximo_actfin <- max(EFH2021$act_fin, na.rm = TRUE)
cuartil_25_actfin <- quantile(EFH2021$act_fin, 0.25, na.rm = TRUE)
cuartil_75_actfin <- quantile(EFH2021$act_fin, 0.75, na.rm = TRUE)

# Tabla con los estadísticos
tabla_estadisticos <- data.frame(
  Estadistico = c("Media", "Desviación Estándar", "Mediana", "Mínimo", "Máximo", "Cuartil 25", "Cuartil 75"),
  Valor = c(media_actfin, desviacion_estandar_actfin, mediana_actfin, minimo_actfin, maximo_actfin, cuartil_25_actfin, cuartil_75_actfin)
)

# Mostrar la tabla:
tabla_estadisticos


# ESTRATO DE INGREGO EFECTIVO DEL HOGAR

# Frecuencias y porcentajes
frecuencias_estrat <- table(EFH2021$estrato)
porcentaje_estrat <- prop.table(frecuencias_estrat) * 100

# Tabla con frecuencias y porcentajes
tabla_resultados_estrat <- data.frame(
  Estrato = names(frecuencias_estrat),
  Frecuencia = frecuencias_estrat,
  Porcentaje = round(porcentaje_estrat, 2)  # Redondeamos los porcentajes a 2 decimales
)

# Mostrar la tabla:
kable(tabla_resultados_estrat, caption = "Distribución del Estrato de Ingreso Efectivo del Hogar")

# Moda
moda <- names(sort(table(EFH2021$estrato), decreasing = TRUE))[1]

# Mediana (como una categoría representativa)
mediana <- median(as.numeric(EFH2021$estrato), na.rm = TRUE)

# Crear una tabla con los estadísticos descriptivos
tabla_estadisticos_estrat <- data.frame(
  Estadistico = c("Moda", "Mediana"),
  Valor = c(moda, names(frecuencias_estrat)[mediana])
)

# Mostrar la tabla:
kable(tabla_estadisticos_estrat, caption = "Estadísticos Descriptivos del Estrato de Ingreso Efectivo del Hogar")


# Carga financiera total de la deuda no hipotecaria

summary(EFH2021$cf_dnhip)


# ANÁLISIS BIVARIADO ----


# Chi cuadrado

# Correlación entre Uso de criptomonedas y Estrato de nivel de ingreso

EFH2021 %>%
  sjPlot::sjtab(estrato,
                u_tcripto,
                show.row.prc = TRUE, # porcentaje fila
                show.col.prc = TRUE # porcentaje columna
  )

chi_results <- chisq.test(EFH2021$estrato, EFH2021$u_tcripto)

chi_results

# Tabla de contingencia:

tabla_contingencia <- table(EFH2021$estrato, EFH2021$u_tcripto)

tabla_contingencia

# V de Cramer

v_cramer <- assocstats(tabla_contingencia)$cramer
print(v_cramer)


# Punto biserial

# Correlación entre Uso de criptomonedas y Valor total de activos financieros

cor_result <- cor.test(EFH2021$u_tcripto, EFH2021$act_fin)
cor_result

# Tabla de proporciones
prop_table <- EFH2021 %>%
  group_by(estrato, u_tcripto) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

prop_table <- EFH2021 %>%
  group_by(estrato, u_tcripto) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Correlación de Spearman 

# Correlación entre Valor total de activos financieros y Estrato de nivel de ingreso

EFH2021$estrato <- as.numeric(as.character(EFH2021$estrato))

spearman_result <- cor.test(EFH2021$estrato, EFH2021$act_fin, method = "spearman", use = "complete.obs")
print(spearman_result)





