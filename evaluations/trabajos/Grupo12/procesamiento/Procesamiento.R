---------------------
  #    Trabajo final    #
  ---------------------

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
               knitr,
               webshot2,
               rio,
               here) # tablas
webshot::install_phantomjs()
options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

# Base de datos ####
data <- "input/base_91.Rds"
base_91 <-readRDS(data)
str(base_91)

base_91 <- base_91 %>%
  mutate(across(where(is.labelled), as.numeric))


# Procesamiento ####

dataCEP <- base_91 %>% # seleccionamos
  dplyr::select(situacion_econ=percepcion_2,
                situacion_politica=percepcion_38,
                conf_gob=confianza_6_i,
                nivel_educ=esc_nivel_1_c,
                area=zona_u_r)



# Recodificar valores perdidos ####

dataCEP$situacion_econ <- car::recode(dataCEP$situacion_econ, "c(-9,-8)=NA")
dataCEP$situacion_politica <- car::recode(dataCEP$situacion_politica, "c(-9,-8)=NA")
dataCEP$conf_gob <- car::recode(dataCEP$conf_gob, "c(-9,-8)=NA")
dataCEP$nivel_educ <- car::recode(dataCEP$nivel_educ, "c(-9,-8)=NA")


# Eliminar valores perdidos ####

sum(is.na(dataCEP))
colSums(is.na(dataCEP))

dataCEP <- na.omit(dataCEP)
dim(dataCEP)



# Recodificación de valores conf_gob ####
dataCEP <- dataCEP %>% 
  mutate(conf_gob = case_when(
    conf_gob == 1 ~ 4,
    conf_gob == 2 ~ 3,
    conf_gob == 3 ~ 2,
    conf_gob == 4 ~ 1
  ))

# Etiquetas de valores conf_gob ####
dataCEP$conf_gob <- set_labels(dataCEP$conf_gob, labels = c(
  "Nada de confianza" = 1,
  "Poca confianza" = 2,
  "Bastante confianza" = 3,
  "Mucha confianza" = 4
))
# Tabla de frecuencia conf_gob ####
sjmisc::frq(dataCEP$conf_gob)
tab_df(sjmisc::frq(dataCEP$conf_gob), title = "Tabla de frecuencia de Confianza en el Gobierno")


# Recodificación de valores situacion_politica ####
dataCEP <- dataCEP %>% 
  mutate(situacion_politica = case_when(
  situacion_politica == 1 ~ 5,
  situacion_politica == 2 ~ 4,
  situacion_politica == 3 ~ 3,
  situacion_politica == 4 ~ 2,
  situacion_politica == 5 ~ 1))

# Etiquetación de valores situacion_politica #### 
dataCEP$situacion_politica <- set_labels(dataCEP$situacion_politica,
                                         labels=c( "Muy mala"=1,
                                                   "Mala"=2,
                                                   "Ni buena, ni mala"=3,
                                                   "Buena"=4,
                                                   "Muy buena"=5))

# Tabla de frecuencias situacion_politica ####
sjmisc::frq(dataCEP$situacion_politica)
tab_df(sjmisc::frq(dataCEP$situacion_politica), title = "Tabla de frecuencia de Situación Política")

# Recodificación de valores area ####
dataCEP <- dataCEP %>% 
  mutate(area = ifelse(area == 2, 1, 0))

# Etiquetación de valores area ####
dataCEP$area <- set_labels(dataCEP$area,
                           labels=c("Urbano"=0,
                                    "Rural"=1))

# Tabla de frecuencias area ####
sjmisc::frq(dataCEP$area)
tab_df(sjmisc::frq(dataCEP$area), title = "Tabla de frecuencia de Área urbano/rural")
           

# Etiquetación valores situacion_econ ####        
dataCEP$situacion_econ <- set_labels(dataCEP$situacion_econ,
                                     labels=c( "Muy mala"=1,
                                               "Mala"=2,
                                               "Ni buena, ni mala"=3,
                                               "Buena"=4,
                                               "Muy buena"=5))

# Tabla de frecuencias situacion_econ ####
sjmisc::frq(dataCEP$situacion_econ)
tab_df(sjmisc::frq(dataCEP$situacion_econ), title = "Tabla de frecuencia de Situación Económica")


# Etiquetación de valores nivel_educ ####
dataCEP$nivel_educ <- set_labels(dataCEP$nivel_educ,
                                 labels=c( "Sin estudios formales"=0,
                                           "Básica incompleta / Primaria o preparatoria incompleta"=1,
                                           "Básica completa / Primara o preparatoria completa"=2,
                                           "Media científico humanista o media técnico profesional incompleta / Humanidades incompleta"=3,
                                           "Media científico humanista o media técnico profesional completa / Humanidades completa"=4,
                                           "Instituto técnico (CFT) o instituto profesional incompleto (carreras 1 a 3 años)"=5,
                                           "Instituto técnico (CFT) o instituto profesional completo (carreras 1 a 3 años) / Hasta suboficial de FFAA/Carabineros"=6,
                                           "Universitaria incompleta (carreras 4 o más años)"=7,
                                           "Universitaria completa (carreras 4 o más años) / oficial de FFAA/Carabineros"=8,
                                           "Postgrado (postítulo, Máster, Magíster, Doctorado) incompleto"=9,
                                           "Postgrado (postítulo, Máster, Magíster, Doctorado) completo"=10))

# Tabla de frecuancias nivel_educ ####
sjmisc::frq(dataCEP$nivel_educ)
tab_df(sjmisc::frq(dataCEP$nivel_educ), title = "Tabla de frecuencia de Nivel Educacional")


# Correlacion entre cada variable ####

dataCEP$conf_gob <- as.numeric(as.character(dataCEP$conf_gob))
dataCEP$situacion_econ <- as.numeric(as.character(dataCEP$situacion_econ))
dataCEP$situacion_politica <- as.numeric(as.character(dataCEP$situacion_politica))

# Realiza los tests de correlación
cor1 <- cor.test(dataCEP$conf_gob, dataCEP$situacion_econ, method = "spearman")
cor2 <- cor.test(dataCEP$conf_gob, dataCEP$situacion_politica, method = "spearman")
cor3 <- cor.test(dataCEP$conf_gob, dataCEP$nivel_educ)
cor4 <- cor.test(dataCEP$conf_gob, dataCEP$area)

# Crea la tabla con los resultados
resultados <- data.frame(
  Variables = c("Confianza en el Gobierno ~ Situación Económica", 
                "Confianza en el Gobierno ~ Situación Política", 
                "Confianza en el Gobierno ~ Nivel Educacional", 
                "Confianza en el Gobierno ~ Área Urbana/Rural"),
  Coeficiente = c(cor1$estimate, cor2$estimate, cor3$estimate, cor4$estimate),
  p_valor = c(cor1$p.value, cor2$p.value, cor3$p.value, cor4$p.value)
)

# Aplica formato a la tabla
resultados %>%
  knitr::kable(format = "html", caption = "Tabla de Correlaciones") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, 
                position = "center") %>%
  row_spec(0, bold = TRUE, color = "mistyrose", background = "mediumorchid3")
# Guarda la tabla en un archivo CSV
write.csv(resultados, file = "output/tabla_correlaciones.csv", row.names = FALSE)


# Realiza los tests de correlación para la creación de tablas individuales
cor1 <- cor.test(dataCEP$conf_gob, dataCEP$situacion_econ, method = "spearman")
cor2 <- cor.test(dataCEP$conf_gob, dataCEP$situacion_politica, method = "spearman")
cor3 <- cor.test(dataCEP$conf_gob, dataCEP$nivel_educ)
cor4 <- cor.test(dataCEP$conf_gob, dataCEP$area)

# Función para crear tablas individuales
crear_tabla <- function(titulo, coef, p_valor) {
  data.frame(
    Métrica = c("Coeficiente", "p-valor"),
    Valor = c(coef, p_valor)
  ) %>%
    knitr::kable(format = "html", caption = titulo) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE,
                  position = "center") %>%
    row_spec(0, bold = TRUE, color = "mistyrose", background = "mediumorchid3")
}

# Tablas individuales
tabla1 <- crear_tabla("Confianza en el Gobierno ~ Situación Económica", cor1$estimate, cor1$p.value)
write.csv(resultados, file = "output/tabla1.csv", row.names = FALSE)
tabla2 <- crear_tabla("Confianza en el Gobierno ~ Situación Política", cor2$estimate, cor2$p.value)
write.csv(resultados, file = "output/tabla2.csv", row.names = FALSE)
tabla3 <- crear_tabla("Confianza en el Gobierno ~ Nivel Educacional", cor3$estimate, cor3$p.value)
write.csv(resultados, file = "output/tabla3.csv", row.names = FALSE)
tabla4 <- crear_tabla("Confianza en el Gobierno ~ Área Urbana/Rural", cor4$estimate, cor4$p.value)
write.csv(resultados, file = "output/tabla4.csv", row.names = FALSE)

# Muestra las tablas
tabla1
tabla2
tabla3
tabla4




summary(dataCEP$situacion_econ)
summary(dataCEP$conf_gob)
summary(dataCEP$situacion_politica)
summary(dataCEP$nivel_educ)
summary(dataCEP$area)

# Tabla de estadísticos descriptivos ####
dataCEP %>%
  sjmisc::descr(show = c("range", "mean", "median", "sd", "n")) %>%
  knitr::kable(format = "html", caption = "Tabla de Estadísticos Descriptivos") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, 
                position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#0073C2") # Encabezado en azul

# Definir función para calcular la moda
Mode <- function(x) {
  ux <- unique(na.omit(x)) # Elimina valores NA
  ux[which.max(tabulate(match(x, ux)))]
}

# Función para calcular tendencia central
tendencia_central <- function(dataCEP, columna) {
  tabla <- dataCEP %>%
    summarise(
      mediana = median({{columna}}, na.rm = TRUE),
      media = mean({{columna}}, na.rm = TRUE),
      moda = Mode({{columna}})
    )
  return(tabla)
}

# Llamar a la función
tabla_confgob <- tendencia_central(dataCEP, conf_gob)
tabla_situacion_politica <- tendencia_central(dataCEP, situacion_politica)
tabla_situacion_econ <- tendencia_central(dataCEP, situacion_econ)
tabla_nivel_educ <- tendencia_central(dataCEP, nivel_educ)
tabla_area <- tendencia_central(dataCEP, area)
# Mostrar resultados
print(tabla_confgob)
print(tabla_situacion_politica)
print(tabla_situacion_econ)
print(tabla_nivel_educ)
print(tabla_area)

tablas <- list(tabla_confgob, tabla_situacion_politica, tabla_situacion_econ, tabla_nivel_educ, tabla_area)
nombres_hojas <- c("Confianza en el Gobierno", "Situación política", "Situación económica", "Nivel educacional", "Área urbana/rural")

# Medidas de dispersión
dispersion <- function(dataCEP, columna){
  tabla <- dataCEP %>%
    summarise(
      rango = max({{columna}})-min({{columna}}),
      de = sd({{columna}}, na.rm = TRUE),
      cv = sd({{columna}}, na.rm = TRUE)/mean({{columna}}, na.rm = TRUE))
  return(tabla)
}

tablaconfgob <- dispersion(dataCEP, conf_gob)
tablasitpol <- dispersion(dataCEP, situacion_politica)
tablasitecon <- dispersion(dataCEP, situacion_econ)
tablaniveleduc <- dispersion(dataCEP, nivel_educ)
tablaarea <- dispersion(dataCEP, area)

# Mostrar resultados
print(tablaconfgob)
print(tablasitpol)
print(tablasitecon)
print(tablaniveleduc)
print(tablaarea)

tablas <- list(tablaconfgob, tablasitpol, tablasitecon, tablaniveleduc, tablaarea)
nombres_hojas <- c("Confianza en el Gobierno", "Situación política", "Situación económica", "Nivel educacional", "Área urbana/rural")

# Definir función para calcular la moda
Mode <- function(x) {
  ux <- unique(na.omit(x)) # Elimina valores NA
  ux[which.max(tabulate(match(x, ux)))] # Encuentra el valor con mayor frecuencia
}

# Función para calcular medidas de tendencia central y dispersión
resumen_completo <- function(data, columna) {
  central <- data %>%
    summarise(
      media = mean({{columna}}, na.rm = TRUE),
      mediana = median({{columna}}, na.rm = TRUE),
      moda = Mode({{columna}})
    )
  
  dispersion <- data %>%
    summarise(
      rango = max({{columna}}, na.rm = TRUE) - min({{columna}}, na.rm = TRUE),
      desviacion_estandar = sd({{columna}}, na.rm = TRUE),
      coeficiente_variacion = sd({{columna}}, na.rm = TRUE) / mean({{columna}}, na.rm = TRUE)
    )
  
  # Combinar resultados
  tabla <- bind_cols(central, dispersion)
  return(tabla)
}

# Crear tabla resumen para cada variable
tabla_confgob <- resumen_completo(dataCEP, conf_gob) %>% mutate(variable = "Confianza en el Gobierno")
tabla_sitpol <- resumen_completo(dataCEP, situacion_politica) %>% mutate(variable = "Situación política")
tabla_sitecon <- resumen_completo(dataCEP, situacion_econ) %>% mutate(variable = "Situación económica")
tabla_niveleduc <- resumen_completo(dataCEP, nivel_educ) %>% mutate(variable = "Nivel educacional")
tabla_area <- resumen_completo(dataCEP, area) %>% mutate(variable = "Área urbana/rural")

# Unir todas las tablas en una sola
tabla_final <- bind_rows(tabla_confgob, tabla_sitpol, tabla_sitecon, tabla_niveleduc, tabla_area)

# Reordenar columnas para mejor visualización
tabla_final <- tabla_final %>%
  select(variable, media, mediana, moda, rango, desviacion_estandar, coeficiente_variacion)

# Mostrar tabla con kable
tabla_final %>%
  knitr::kable(format = "html", caption = "Tabla de Estadísticos Descriptivos") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, color = "lightpink", background = "maroon3") # Encabezado azul

# Guardar la tabla como un archivo CSV en la carpeta "output"
write.csv(tabla_final, file = "output/tabla_final.csv", row.names = FALSE)

# Guardamos base de datos ####
saveRDS(dataCEP, "output/dataCEP.rds")
