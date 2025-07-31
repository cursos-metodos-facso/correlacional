library(pacman)
p_load(tidyverse, #entorno de paquetes
       dplyr,# Manipulacion datos
       sjPlot, # Graficos y tablas
       sjmisc, # Descriptivos
       corrplot, # Correlaciones
       psych, # Test estadísticos
       kableExtra,
       sjlabelled,
       labelled,
       summarytools) # Tablas
options(scipen = 999)
rm(list = ls()) 
library(haven)

ELSOC_Long_2016_2022_v1_00 <- read_dta("input/ELSOC_Long_2016_2022_v1.00.dta")

data <- ELSOC_Long_2016_2022_v1_00 %>% filter(ola==3) %>% select(t06_01, 
                                                                      t06_02,
                                                                      t06_03,
                                                                      t06_04,
                                                                      t06_05,
                                                                      t06_06,
                                                                      t06_07,
                                                                      t06_08,
                                                                      t07_01,
                                                                      t07_02,
                                                                      m29,
                                                                      m0_sexo)




data <- data %>%
  mutate(across(everything()    , ~ na_if(., -666))) %>%
  mutate(across(everything(), ~ na_if(., -777))) %>%
  mutate(across(everything(), ~ na_if(., -999))) %>%
  mutate(across(everything(), ~ na_if(., -888)))



data <- data %>% select(t06_01, t06_02, t06_03, t06_04, t06_05, t06_06, t06_07, t06_08, t07_01, t07_02, m29, m0_sexo) %>%
  mutate(across(c(t06_01, t06_02, t06_03, t06_04, t06_05, t06_06, t06_07, t06_08, t07_01, t07_02), 
                ~ . - 1, .names = "{.col}"))

data <- data %>%
  mutate(across(matches("^t06_|^t07_"), ~ set_labels(., 
                                                     labels = c("Totalmente en desacuerdo" = 0,
                                                                "En desacuerdo" = 1,
                                                                "Ni de acuerdo ni en desacuerdo" = 2,
                                                                "De acuerdo" = 3,
                                                                "Totalmente de acuerdo" = 4))))


data <- data %>%
  select(t06_01, t06_02, t06_03, t06_04, t06_05, t06_06, t06_07, t06_08, t07_01, t07_02, m29, m0_sexo) %>%
  rename_with(~ c(
    "seguridad",
    "conectividad",
    "areasverdes",
    "limpieza",
    "ptrabajo",
    "pescuelas",
    "pcomercio",
    "pamigos",
    "tamanovivienda",
    "calidadhogar",
    "ingreso",
    "sexo"))

data <- data %>% mutate(isr = rowSums(select(., seguridad,
                                             conectividad,
                                             areasverdes,
                                             limpieza,
                                             ptrabajo,
                                             pescuelas,
                                             pcomercio,
                                             pamigos,
                                             tamanovivienda,
                                             calidadhogar), na.rm = TRUE))


data <- data %>%
  mutate(isr_rec = cut(isr,
                                breaks = c(-Inf, 9, 19, 29, 39, 40),
                                labels = c("1", "2", "3", "4", "5"),
                                right = TRUE, 
                                include.lowest = TRUE))

data <- data %>%
  mutate(isr_rec = factor(isr_rec, 
                          levels = c(1, 2, 3, 4, 5),
                          labels = c("Satisfacción muy baja", 
                                     "Satisfacción baja",
                                     "Satisfacción moderada", 
                                     "satisfaccion alta",
                                     "satisfaccion muy alta")))





data <-data %>% mutate(isr = set_variable_labels(isr, "Escala de satisfaccion residencial"),
                       isr_rec = set_variable_labels(isr_rec, "Escala de satisfaccion residencial recodificada"),
                       sexo = set_variable_labels(sexo, "Sexo del entrevistado"))



tab_corr(data %>% select(seguridad,
                         conectividad,
                         areasverdes,
                         limpieza,
                         ptrabajo,
                         pescuelas,
                         pcomercio,
                         pamigos,
                         tamanovivienda,
                         calidadhogar), triangle = "lower")

alpha(data %>% select(seguridad,
                      conectividad,
                      areasverdes,
                      limpieza,
                      ptrabajo,
                      pescuelas,
                      pcomercio,
                      pamigos,
                      tamanovivienda,
                      calidadhogar))


#analisis de confiailidad y valides para pa escala
matriz <- tab_corr(data %>% select(seguridad,
                         conectividad,
                         areasverdes,
                         limpieza,
                         ptrabajo,
                         pescuelas,
                         pcomercio,
                         pamigos,
                         tamanovivienda,
                         calidadhogar), triangle = "lower")

alfa <- alpha(data %>% select(seguridad,
                      conectividad,
                      areasverdes,
                      limpieza,
                      ptrabajo,
                      pescuelas,
                      pcomercio,
                      pamigos,
                      tamanovivienda,
                      calidadhogar))

#descriptivo variables principales.
sumario1a <- dfSummary(data %>% select(isr)) %>% (view)
sumario1b <- dfSummary(data %>% select (isr_rec)) %>% (view)
sumario1c <- dfSummary(data %>% select (sexo)) %>% (view)
sumario1d <- dfSummary(data %>% select (ingreso)) %>% (view)

tab_xtab(data$sexo, data$isr_rec, show.row.prc = TRUE)
  
cor.test(data$isr, data$ingreso)



  
  
 tabla1 <- dfSummary(ELSOC_Long_2016_2022_v1_00 %>% filter(ola==3) %>%
              select(t06_01, t06_02, t06_03, t06_04, t06_05, 
                     t06_06, t06_07, t06_08, t07_01, t07_02)) %>%
    view()


