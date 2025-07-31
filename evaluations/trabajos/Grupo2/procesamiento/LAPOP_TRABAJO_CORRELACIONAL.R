# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                    Estadística Correlacional 2023
#
#
# ******************************************************************************
# Carga Librerías --------------------------------------------------------------

pacman::p_load(tidyverse,
               sjmisc,
               knitr,
               kableExtra,
               sjPlot,
               stargazer,
               janitor,
               crosstable,
               table1,
               haven,
               car,
               psych,
               summarytools,
               ggplot2,
               dplyr,
               broom
) 

options(scipen = 999)
rm(list = ls())

# Datos y procesamiento de datos -----------------------------------------------


# Carga datos ------------------------------------------------------------------

SLV_2023_LAPOP_AmericasBarometer_v1.0_w.sav <- read_sav ("http://datasets.americasbarometer.org/database/files/SLV_2023_LAPOP_AmericasBarometer_v1.0_w.sav") 

# Filtrar y seleccionar -------------------------------------------------------

data <- SLV_2023_LAPOP_AmericasBarometer_v1.0_w.sav %>%
  dplyr::select(aprob_gob = m1, #1-5
                gob_correct = anestg, #1-4
                conf_pres = b21a, #1-7
                sensa_seg = aoj11, #1-4
                seg_pais = soe1, #1-3
                seg_com = soe2, #1-3
                seg_fam = soe3) #1-3

## Remover NA's ----------------------------------------------------------------
data$aprob_gob <- car::recode(data$aprob_gob, "c(888888,988888)=NA")
data$gob_correct <- car::recode(data$gob_correct, "c(888888,988888)=NA")
data$conf_pres <- car::recode(data$conf_pres, "c(888888,988888)=NA")
data$sensa_seg <- car::recode(data$sensa_seg, "c(888888,988888)=NA")
data$seg_pais <- car::recode(data$seg_pais, "c(888888,988888)=NA")
data$seg_com <- car::recode(data$seg_com, "c(888888,988888)=NA")
data$seg_fam <- car::recode(data$seg_fam, "c(888888,988888)=NA")

colSums(is.na(data))
sum(is.na(data))

data<- na.omit(data)

## Recodificar en función del sentido ------------------------------------------
data <- data %>%
  dplyr::mutate(aprob_gob = case_when(aprob_gob == 1  ~ 5, 
                                      aprob_gob == 2 ~ 4,
                                      aprob_gob == 3 ~ 3,
                                      aprob_gob == 4 ~ 2,
                                      aprob_gob == 5 ~ 1))

data <- data %>%
  dplyr::mutate(gob_correct = case_when(gob_correct == 1  ~ 4, 
                                        gob_correct == 2 ~ 3,
                                        gob_correct == 3 ~ 2,
                                        gob_correct == 4 ~ 1))
data <- data %>%
  dplyr::mutate(sensa_seg = case_when(sensa_seg == 1  ~ 4, 
                                      sensa_seg == 2 ~ 3,
                                      sensa_seg == 3 ~ 2,
                                      sensa_seg == 4 ~ 1))
data <- data %>%
  dplyr::mutate(seg_pais = case_when(seg_pais == 1  ~ 3, 
                                     seg_pais == 2 ~ 2,
                                     seg_pais == 3 ~ 1))
data <- data %>%
  dplyr::mutate(seg_com = case_when(seg_com == 1  ~ 3, 
                                    seg_com == 2 ~ 2,
                                    seg_com == 3 ~ 1))
data <- data %>%
  dplyr::mutate(seg_fam = case_when(seg_fam == 1  ~ 3, 
                                    seg_fam == 2 ~ 2,
                                    seg_fam == 3 ~ 1))

#Para todas las variables, 1 es "menos" o "menor" y el número más grande es,
#"más" o "mayor" o "mejor"...


# Crear índice: aprobación del gobierno ----------------------------------------
##Recodificación de variables

data <- data %>%
  mutate(r_aprob_gob = case_when(aprob_gob %in% c(1, 2) ~ 1,
                                 aprob_gob == 3 ~ 2,
                                 aprob_gob %in% c(4, 5) ~ 3))
data <- data %>%
  mutate(r_gob_correct = case_when(gob_correct %in% c(1, 2) ~ 1,  
                                   gob_correct == 3 ~ 2,          
                                   gob_correct == 4 ~ 3))
data <- data %>%
  mutate(r_conf_pres = case_when(conf_pres %in% c(1, 2) ~ 1,  
                                 conf_pres %in% c(3, 4, 5) ~ 2,          
                                 conf_pres %in% c(6, 7) ~ 3))
#indice gobierno

### Consistencia de la bateria de datos-----------------------------------------

gobierno <- data %>%
  dplyr::select(r_aprob_gob, r_gob_correct, r_conf_pres)

sjPlot::tab_corr(gobierno, 
                 triangle = "lower")

psych::alpha(gobierno)

#### Creación del indice -------------------------------------------------------

data = data %>% 
  rowwise() %>%
  mutate(indice_gob = mean(c(r_aprob_gob, r_gob_correct, r_conf_pres))) %>% 
  ungroup()

summary(data$indice_gob)

# Crear escala: de  seguridad --------------------------------------------------
#Consistencia de la batería de variables y porqué no se eligió sensa_seg
#Seguridad: todas las variables (no tiene buena correlación)

seguridad <- data %>%
  dplyr::select(sensa_seg, seg_pais, seg_com, seg_fam)

sjPlot::tab_corr(seguridad,
                 triangle = "lower")

psych::alpha(seguridad)

## Escala de Seguridad: varibles de cambio -------------------------------------
#(estas sí tienen toda la correlación)

cambio_seguridad <- data %>%
  dplyr::select(seg_pais, seg_com, seg_fam)

sjPlot::tab_corr(cambio_seguridad,
                 triangle = "lower")
### consistencia de la bateria de datos-----------------------------------------

psych::alpha(cambio_seguridad)

# Escala seguridad -------------------------------------------------------------

data = data %>% 
  rowwise() %>%
  mutate(escala_seg= mean(c(seg_pais, seg_com, seg_fam))) %>% 
  ungroup()

summary(data$escala_seg)

# Descriptivos -----------------------------------------------------------------
## Descriptivos generales ------------------------------------------------------
#Descriptivos de todas las variables a usar

descriptivos <- data %>%
  dplyr::select(aprob_gob, gob_correct, conf_pres, sensa_seg, seg_pais, seg_com,
                seg_fam, indice_gob, escala_seg)

psych::describe(descriptivos) %>%
  kable() %>%
  kable_styling(full_width = TRUE)

### Tabla ----------------------------------------------------------------------

## Descriptivos: Indice gobierno -----------------------------------------------

psych::describe(data$indice_gob) %>%
  kable() %>%
  kable_styling(full_width = TRUE)

describe(data$escala_seg)

### Tabla ----------------------------------------------------------------------

### Diagrama -------------------------------------------------------------------

## Descriptivos: Escala de seguridad -------------------------------------------

psych::describe(data$escala_seg) %>%
  kable() %>%
  kable_styling(full_width = TRUE)

### Tabla ----------------------------------------------------------------------

### Grafico  -------------------------------------------------------------------

## Descriptivos: Sensación de seguridad ----------------------------------------

psych::describe(data$sensa_seg) %>%
  kable() %>%
  kable_styling(full_width = TRUE)

### Tabla ----------------------------------------------------------------------

### Grafico --------------------------------------------------------------------

# Análisis estadistico Bivariado -----------------------------------------------
## Hipótesis 1:-----------------------------------------------------------------
### Prueba de hipótesis --------------------------------------------------------
###A mayor percepción de seguridad actual,
###mayor aprobación del gobierno de Bukele

gob_sensa_seg <- data %>% 
  dplyr::select(indice_gob, sensa_seg)

sjPlot::tab_corr(gob_sensa_seg, 
                 na.deletion = "listwise",
                 triangle = "lower")

cor.test(data$indice_gob, data$sensa_seg)

# Visualización bivariada ------------------------------------------------------

## Hipótesis 2:-----------------------------------------------------------------
### Prueba de hipótesis --------------------------------------------------------
###A mayor noción de que mejoró la seguridad,
###mayor aprobación de Bukele

gob_escala_seg <- data %>% 
  dplyr::select(indice_gob, escala_seg)

sjPlot::tab_corr(gob_escala_seg, 
                 na.deletion = "listwise",
                 triangle = "lower")

cor.test(data$indice_gob, data$escala_seg)

# Visualización bivariada ------------------------------------------------------

## Hipótesis 3:-----------------------------------------------------------------
### Prueba de hipótesis --------------------------------------------------------
### A mayor sensación de seguridad actual,
###mayor confianza en que el gobiernonacional está haciendo lo correcto 

sjPlot::sjt.xtab(data$sensa_seg, data$gob_correct,
                 show.row.prc = TRUE,
                 show.col.prc = TRUE)

chisq.test(data$sensa_seg, data$gob_correct)

# Visualización bivariada ------------------------------------------------------


# Guardar datos ----------------------------------------------------------------
save(data,file="output/LAPOP_TRABAJO_CORRELACIONAL.RData")

