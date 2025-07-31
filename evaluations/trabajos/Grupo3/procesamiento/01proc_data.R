# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                    Estadística Correlacional 2023
#
#             Plantilla procesamiento trabajo final curso
#
# ******************************************************************************


# Carga Librerías --------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse,   # manipulacion datos
               sjPlot,      # tablas
               confintr,    # IC
               gginference, # visualizacion 
               rempsyc,     # reporte
               broom,       # varios
               sjmisc,      # para descriptivos
               knitr)       # para       

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo


# Carga datos ------------------------------------------------------------------

#Instalar paquete haven 
install.packages("haven")

# Cargar el paquete haven
library(haven)

# Cargar la base de datos .dta
data <- read_dta("C:/Users/victo/Desktop/Estadística Final/input/CHL_2023_LAPOP_AmericasBarometer_v1.0_w.dta")


## Filtrar y seleccionar -------------------------------------------------------
library(dplyr)
data1 <- data %>%  
  select(tendecia_politica=l1n,
         nivel_educativo=edre,
         ingresos_familiares=q10inc,
         edad=q2) 


## Recodificamos y Removemos NA's ----------------------------------------------------------------

data1$tendecia_politica <- car::recode(data1$tendecia_politica, "c(999999,888888,988888)=NA")
data1$nivel_educativo <- car::recode(data1$nivel_educativo, "c(888888,988888)=NA")
data1$ingresos_familiares <- car::recode(data1$ingresos_familiares, "c(888888,988888)=NA")
data1$edad <- car::recode(data1$edad, "c(888888,988888)=NA")

install.packages("tidyr")
library(tidyr)
data_final <- drop_na(data1)


#Convertimos las variables en numéricas ----------------------------------------------------------------

install.packages("labelled")
library(labelled)

library(dplyr)

data_final <- data_1 %>%
  mutate(across(everything(), ~ {
    if (is.labelled(.)) {
      as.numeric(as.character(.))  # Convertir de haven_labelled a numérico
    } else {
      as.numeric(.)  # Convertir a numérico si ya es numérico
    }
  }))

## Recodificamos valores de variable tendencia política para que sean más comprensibles-----------------------------------------
# Restar 1300 a la variable ingresos_familiares
data_final$ingresos_familiares <- data_final$ingresos_familiares - 1300


# Guardar datos ----------------------------------------------------------------
save(data,file="output/data.RData")
