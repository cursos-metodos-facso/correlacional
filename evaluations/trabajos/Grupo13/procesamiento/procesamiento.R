pacman::p_load(tidyverse, 
               car,
               haven,
               broom)

options(scipen = 999)
rm(list = ls())

proc_elsoc<-elsoc %>%
  dplyr::select(
    c16, d01_01, m01,
  )

proc_elsoc <- proc_elsoc %>% rename(Identificación_Partidaria = c16, Estatus_Social =d01_01,
                                    Nivel_Educacional = m01)

proc_elsoc <- proc_elsoc %>%
  mutate(Identificación_Partidaria = car::recode(Identificación_Partidaria,
                                                 "-999:-666 = NA",
                                                 as.factor = TRUE))
proc_elsoc <- proc_elsoc %>%
  mutate(Nivel_Educacional = car::recode(Nivel_Educacional,
                                         "-999:-666 = NA",
                                         as.factor = TRUE))
proc_elsoc <- proc_elsoc %>%
  mutate(Estatus_Social = car::recode(Estatus_Social,
                                      "-999:-666 = NA",
                                      as.factor = TRUE))
proc_elsoc <- na.omit(proc_elsoc)

proc_elsoc <- proc_elsoc %>%
  mutate(Identificación_Partidaria = ifelse(Identificación_Partidaria %in% c(15), "No se identifica", "Si se identifica"))

proc_elsoc$Identificación_Partidaria_num <- ifelse(proc_elsoc$Identificación_Partidaria == "No se identifica", 0, 
                                                   ifelse(proc_elsoc$Identificación_Partidaria == "Si se identifica", 1, NA))

proc_elsoc <- proc_elsoc %>%
  mutate(Nivel_Educacional = car::recode(Nivel_Educacional,
                                         "1 = 'Sin estudios'; 
                                     2 = 'Educación Básica o Preparatoria incompleta'; 
                                     3 = 'Educación Básica o Preparatoria completa'; 
                                     4 = 'Educación Media o Humanidades incompleta'; 
                                     5 = 'Educación Media o Humanidades completa'; 
                                     6 = 'Técnica Superior incompleta'; 
                                     7 = 'Técnica Superior completa'; 
                                     8 = 'Universitaria incompleta'; 
                                     9 = 'Universitaria completa'; 
                                     10 = 'Estudios de posgrado (magíster o doctorado)'", 
                                         as.factor = TRUE))


save(proc_elsoc, file = "proc_elsoc.RData")
