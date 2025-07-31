# ******************************************************************************
# 
#                         Universidad de Chile
#                     Facultad de Ciencias Sociales
#                    Estadística Correlacional 2023
#
# ******************************************************************************


# Carga librerías --------------------------------------------------------------

library(pacman)

pacman::p_load(tidyverse, # Manipulacion datos
               sjmisc, # Descriptivos
               knitr, # Render y tablas
               kableExtra, # Formateo tablas
               summarytools, # Tablas
               sjPlot, # Tablas y gráficos
               stargazer, # Tablas
               janitor, # Tablas y formateo
               crosstable, # Tablas
               table1, # Tablas
               psych, # Estadísticos
               broom,
               psych, # Test estadísticos
               haven,
               ggplot2,
               ggmosaic,
               webshot,
               sjlabelled)

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

# Carga datos ------------------------------------------------------------------

library(haven)
CEP <- read_dta("input/data/Base de datos_ECEP_89/base_89.dta")

# Convertimos todas las variables a numerica -----------------------------------

CEP.treatment <- dplyr::select(CEP,
                               iden_pol_2,
                               esc_nivel_1,
                               democracia_20) %>%
  mutate_all(~(as.numeric(.)))

# Tratamiento de los casos perdidos --------------------------------------------

CEP.treatment$pos.politica <- car::recode(CEP$iden_pol_2, "c(99,88)=NA")
CEP.treatment$educ <- car::recode(CEP$esc_nivel_1, "c(99,88)=NA")
CEP.treatment$dem.opi <- car::recode(CEP$democracia_20, "c(9,8)=NA")

#Recodificación ----------------------------------------------------------------

# Inversión de variable
CEP.treatment <- CEP.treatment %>%
  dplyr::mutate(
    dem.opi = 6 - dem.opi,
  )

#Confirmamos que fue correctamente invertida en sentido
CEP.treatment$dem.opi

#Creamos una nueva variable de extremismo en base a posición política
CEP.treatment <- CEP.treatment %>%
  mutate(ext.politica = case_when(
    pos.politica %in% c(1, 2, 9, 10) ~ 1,
    pos.politica %in% c(3, 4, 5, 6, 7, 8) ~ 0,
  ))

#Confirmamos que la variable fue correctamente creada
CEP.treatment$ext.politica

#Contamos los valores perdidos (NA) en una variable específica
sum(is.na(CEP.treatment$pos.politica))

#Seleccionamos variables tratadas en dataframe final

CEP.selected <- dplyr::select(CEP.treatment,
                              dem.opi,
                              pos.politica,
                              ext.politica,
                              educ)

# Analisis descriptivo ---------------------------------------------------------

#Mutamos variable para presentar la tabla
CEP.selected$pos.politica <- as.numeric(CEP.selected$pos.politica)

CEP.selected$educ <- as.numeric(CEP.selected$educ)

#Tabla univariada descriptiva

dfSummary(CEP.selected) %>% 
  summarytools::view(method = "render")


#Ajustamos Labels

CEP.selected$dem.opi <- set_labels(
  CEP.selected$dem.opi, 
  labels = c("Muy mal" = 1, "Mal" = 2, "Regular" = 3, "Bien" = 4, "Muy bien" = 5)
)

CEP.selected$ext.politica <- set_labels(
  CEP.selected$ext.politica, 
  labels = c("Extremista" = 1, "No extremista" = 0)
)


#Generamos tablas bivariadas descriptivas

sjPlot::tab_xtab(CEP.selected$educ, CEP.selected$dem.opi, show.row.prc = TRUE)

sjPlot::tab_xtab(CEP.selected$pos.politica, CEP.selected$dem.opi, show.row.prc = TRUE)

sjPlot::tab_xtab(CEP.selected$ext.politica, CEP.selected$dem.opi, show.row.prc = TRUE)


# Analisis Bivariado------------------------------------------------------------


#Revisamos las correlaciones

cor.test(CEP.selected$dem.opi,
         CEP.selected$educ,
         method = "kendall")

cor.test(CEP.selected$dem.opi,
         CEP.selected$pos.politica,
         method = "pearson")

cor.test(CEP.selected$dem.opi,
         CEP.selected$ext.politica,
         method = "pearson")


#Tabla de asociación
#Creamos objeto para el análisis
Tablas <- CEP.selected %>%
  dplyr::select(pos.politica, dem.opi, educ, ext.politica) %>%
  na.omit()


#Generamiento de la tabla
ggplot(data = Tablas) +
  geom_mosaic(aes(product(dem.opi, ext.politica))) +
  geom_label(data = layer_data(last_plot(), 1),
             aes(x = (xmin + xmax)/ 2,
                 y = (ymin + ymax)/ 2,
                 label = paste0(round((.wt/sum(.wt))*100,1),"%"))) + theme_bw()

