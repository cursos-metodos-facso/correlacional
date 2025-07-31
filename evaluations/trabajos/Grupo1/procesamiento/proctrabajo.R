#r librerias y base de datos ----
pacman::p_load(tidyverse,
               ggplot2, 
               sjPlot, 
               ggpubr, 
               ggmosaic,
               dplyr, 
               gginference, 
               rempsyc, 
               kableExtra, 
               broom,
               labelled,
               gt,
               DT)

options(scipen = 999) 
rm(list = ls()) 
library(haven)
Simce4b2022_rbd_final <- read_dta("input/data/Simce4b2022_rbd_final.dta")
save(Simce4b2022_rbd_final,file = "output/Simce4b2022_rbd_final.Rdata")

#Procesamiento y recodificacion de variables-----

# procesamiento de variables
proc_simce4b2022 <- dplyr::select(Simce4b2022_rbd_final, 
                                  region = nom_reg_rbd,
                                  rural_urbano = cod_rural_rbd,
                                  prom_lec = prom_lect4b_rbd,
                                  prom_mat = prom_mate4b_rbd)


# recodificacion de variables
proc_simce4b2022 <- dplyr::mutate(
  proc_simce4b2022,
  prom_mat = car::recode(prom_mat, recodes = "0 = NA"),
  prom_lec = car::recode(prom_lec, recodes = "0 = NA")
)

#Creacion variables a utilizar ----

library(dplyr)

# nueva variable zona según condiciones de 'region'

proc_simce4b2022 <- proc_simce4b2022 %>%
  mutate(zona = case_when(
    region %in% c("DE ARICA Y PARINACOTA", "DE TARAPACÁ", "DE ANTOFAGASTA", "DE ATACAMA") ~ "Zona norte",
    region %in% c("DE LA ARAUCANÍA", "	DE LOS RÍOS", "DE LOS LAGOS") ~ "Zona Sur",
    region == "METROPOLITANA DE SANTIAGO" ~ "Zona Metropolitana",
    TRUE ~ "Otra Zona"
  ))
#nueva variable promedio general
proc_simce4b2022 <- proc_simce4b2022 %>%
  mutate(
    prom_general = rowMeans(cbind(prom_lec, prom_mat), na.rm = TRUE)
  )

#tablas variables a utilizar ----

#creacion tabla variable rural/urbano}

tabla_1 <- proc_simce4b2022 %>%
  group_by(rural_urbano) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round((n / sum(n)) * 100, 2)) 

tabla_1 <- tabla_1 %>% 
  kableExtra::kable(format = "html",
                    align = "c",
                    col.names = c("Pertenece a zona rural o urbana", "n", "Proporción"),
                    caption = "Tabla 1. Distribución de ruralidad/urbanidad") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a resultados Simce 2022.")

#creacion tabla de frecuencias region

tabla_2 <- proc_simce4b2022 %>%
  group_by(region) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round((n / sum(n)) * 100, 2)) 

tabla_2 <- tabla_2 %>% 
  kableExtra::kable(format = "html",
                    align = "c",
                    col.names = c("Region", "n", "Proporción"),
                    caption = "Tabla 2. Distribución de datos por región") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a resultados Simce 2022.")

#creacion tabla descriptiva de zona
tabla_3 <- proc_simce4b2022 %>%
  group_by(zona) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round((n / sum(n)) * 100, 2)) 

tabla_3 <- tabla_3 %>% 
  kableExtra::kable(format = "html",
                    align = "c",
                    col.names = c("Zona", "n", "Proporción"),
                    caption = "Tabla 3. Distribución de datos por Zona") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a resultados Simce 2022.")

#r estadisticos descriptivos promedio lectura

tabla_4 <- proc_simce4b2022 %>%
  summarise(
    Media = mean(prom_lec, na.rm = TRUE),
    Mediana = median(prom_lec, na.rm = TRUE),
    `Cuartil 1` = quantile(prom_lec, probs = 0.25, na.rm = TRUE),
    `Cuartil 3` = quantile(prom_lec, probs = 0.75, na.rm = TRUE),
    `Desviacion estándar` = sd(prom_lec, na.rm = TRUE),
    Varianza = var(prom_lec, na.rm = TRUE),
    `Coeficiente de variación` = sd(prom_lec, na.rm = TRUE) / mean(prom_lec, na.rm = TRUE)
  )


tabla_4 <- kableExtra::kbl(tabla_4, escape=F, full_width = F, caption = "Tabla 4: Estadísticos descriptivos promedio de lectura")  %>%
  kable_paper("hover") %>%
  kableExtra::kable_classic(full_width = F, font_size = 14) %>% kable_minimal() %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a resultados Simce 2022.")

#estadisticos descrptivos promedio matematicas}
tabla_5 <- proc_simce4b2022 %>%
  summarise(
    Media = mean(prom_mat, na.rm = TRUE),
    Mediana = median(prom_mat, na.rm = TRUE),
    `Cuartil 1` = quantile(prom_mat, probs = 0.25, na.rm = TRUE),
    `Cuartil 3` = quantile(prom_mat, probs = 0.75, na.rm = TRUE),
    `Desviacion estándar` = sd(prom_mat, na.rm = TRUE),
    Varianza = var(prom_mat, na.rm = TRUE),
    `Coeficiente de variación` = sd(prom_mat, na.rm = TRUE) / mean(prom_mat, na.rm = TRUE)
  )


tabla_5 <- kableExtra::kbl(tabla_5, escape=F, full_width = F, caption = "Tabla 5: Estadísticos descriptivos promedio de matematicas")  %>%
  kable_paper("hover") %>%
  kableExtra::kable_classic(full_width = F, font_size = 14) %>% kable_minimal() %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a resultados Simce 2022.")

#estadisticos descriptivos promedio general
tabla_6 <- proc_simce4b2022 %>%
  summarise(
    Media = mean(prom_general, na.rm = TRUE),
    Mediana = median(prom_general, na.rm = TRUE),
    `Cuartil 1` = quantile(prom_general, probs = 0.25, na.rm = TRUE),
    `Cuartil 3` = quantile(prom_general, probs = 0.75, na.rm = TRUE),
    `Desviacion estándar` = sd(prom_general, na.rm = TRUE),
    Varianza = var(prom_general, na.rm = TRUE),
    `Coeficiente de variación` = sd(prom_general, na.rm = TRUE) / mean(prom_general, na.rm = TRUE)
  )


tabla_6 <- kableExtra::kbl(tabla_6, escape=F, full_width = F, caption = "Tabla 6: Estadísticos descriptivos promedio general")  %>%
  kable_paper("hover") %>%
  kableExtra::kable_classic(full_width = F, font_size = 14) %>% kable_minimal() %>%
  kableExtra::add_footnote(label = "Fuente: 
                         Elaboración propia en base a resultados Simce 2022.")

#


#analisis bivariado 1----
# hipotesis alternativa 1 diferencia ruralidades norte y sur

#borrar na de base de datos para poder hacer las hipotesis

proc_simce4b2022 <- na.omit(proc_simce4b2022)

# Filtrar datos para Zona Norte Rural y Zona Sur Rural
datos_h1 <- proc_simce4b2022 %>%
  filter(
    zona %in% c("Zona norte", "Zona Sur") & 
      rural_urbano == "2"
  )

# Realizar la prueba t para la variable prom_general
prueba_t_h1 <- t.test(
  prom_general ~ zona,
  data = datos_h1,
  var.equal = TRUE 
)


grafico_1 <- gginference::ggttest(prueba_t_h1) +
  ggtitle("Grafico Prueba t Hipotesis 1")

#| label: fig-1
#| fig-cap: "Grafico prueba T Hipótesis 1"
#|fig-cap-location: top
#| warning: false
grafico_1

tabla_h1 <- tidy(prueba_t_h1, conf_int = T)
nice_table(tabla_h1, broom = "t.test")


#analisis bivariado 2----
#r prueba t hipotesis 2

prueba_t_h2 <- t.test(
  prom_general ~ rural_urbano,
  data = proc_simce4b2022,
  var.equal = TRUE # Cambia a FALSE si no asumes varianzas iguales
)


library(flextable)

grafico_2 <- gginference::ggttest(prueba_t_h2)+
  ggtitle("Grafico prueba t hipotesis 2")

#| label: fig-2
#| fig-cap: "Grafico prueba T Hipótesis 2"
#|fig-cap-location: top
#| warning: false
grafico_2

library(rempsyc)
tabla_h2 <- tidy(prueba_t_h2, conf_int = T)
nice_table(tabla_h2, broom = "t.test")

#analisis bivariado 3 ----
#r etiquetar tramos promedio y pertenencia a zona metropolitana
promedio <- mean(proc_simce4b2022$prom_general, na.rm = TRUE)
proc_simce4b2022 <- mutate(proc_simce4b2022,
                           tramo_promedio = case_when
                           (prom_general < 251.8169 ~ "Bajo el promedio",
                             prom_general >= 251.8169 ~ "Igual o mayor al promedio"))

proc_simce4b2022$zona_metropolitana <- ifelse(proc_simce4b2022$zona == "Zona Metropolitana", "Si", "No")


library(sjPlot)
library(sjlabelled)

proc_simce4b2022$tramo_promedio <- set_label(proc_simce4b2022$tramo_promedio, 
                                             label = "Distancia con el promedio")

proc_simce4b2022$zona_metropolitana <- set_label(proc_simce4b2022$zona_metropolitana, 
                                                 label = "Pertenece a Zona Metropolitana")


tabla_hipotesis_3 <- proc_simce4b2022 %>%
  sjPlot::sjtab(tramo_promedio,
                zona_metropolitana,
                encoding= "UFT-8",
                show.row.prc = TRUE,
                show.col.prc = TRUE)
#| label: fig-3
#| fig-cap: "Prueba T Hipótesis 1"
#|fig-cap-location: top
#| warning: false
tabla_hipotesis_3


