# Trabajo final correlacional (arreglo de base de datos)
# Cargar librerías -----

pacman::p_load(tidyverse, # colección de paquetes para manipulación de datos
               dplyr,# para manipular datos
               car,       # para recodificar
               psych,     # para Alfa de Chronbach
               sjmisc,    # para descriptivos
               sjPlot,     # para gráficos
               sjlabelled,
               haven, # para importar datos
               car,# para recodificar datos
               knitr) # tablas

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

# Carga de base de datos -----

setwd("~/Estadisticacorrelacional/TrabajoFinal/input/data")

lapopcl23 <- read_dta("lapop2023.dta")

# Limpiar base de datos -----

proc_lapop <- dplyr::select(lapopcl23, c(aprob_pino = arn1,
                                      rrss = smedia11,
                                      educ = edre,
                                      id_pol = vb10)) # Seleccionar variables

colSums(is.na(proc_lapop))
sum(is.na(proc_lapop))


proc_lapop <- mutate(proc_lapop,
                     aprob_pino = case_when(aprob_pino == 1 ~ "5",
                                            aprob_pino == 2 ~ "4",
                                            aprob_pino == 3 ~ "3",
                                            aprob_pino == 4 ~ "2",
                                            aprob_pino == 5 ~ "1"))

proc_lapop <- mutate(proc_lapop,
                     rrss  = case_when(rrss == 1 ~ "1",
                       rrss == 2 ~ "0"))

proc_lapop <- mutate(proc_lapop,
                     id_pol  = case_when(id_pol == 1 ~ "1",
                                       id_pol == 2 ~ "0"))

proc_lapop$rrss <- as.numeric(proc_lapop$rrss)

proc_lapop$id_pol <- as.numeric(proc_lapop$id_pol)

proc_lapop$aprob_pino <- as.numeric(proc_lapop$aprob_pino)

proc_lapop <- mutate(proc_lapop,
                     aprob_pino_num = case_when(aprob_pino >= 1  & aprob_pino <= 3 ~ "0",
                                                aprob_pino >= 4 & aprob_pino <= 5 ~ "1"))

proc_lapop$aprob_pino_num <- as.numeric(proc_lapop$aprob_pino_num)

proc_lapop <- mutate(proc_lapop,
                     aprob_pino_dum = case_when(aprob_pino >= 1  & aprob_pino <= 3 ~ "Baja aprobación",
                                                aprob_pino >= 4 & aprob_pino <= 5 ~ "Alta aprobación"))

proc_lapop <- na.omit(proc_lapop) # Utilizamos listwise


proc_lapop$aprob_pino <- set_labels(proc_lapop$aprob_pino,
                                      labels=c( "Muy malo"=1,
                                                "Malo"=2,
                                                "Ni bueno, ni malo"=3,
                                                "Bueno"=4,
                                                "Muy bueno"=5))

proc_lapop$educ <- set_labels(proc_lapop$educ,
                                    labels=c( "Bajo nivel educacional"=0,1,2,3,
                                              "Alto nivel educacional"=4,5,6))

proc_lapop$aprob_pino_num <- set_labels(proc_lapop$aprob_pino_num,
                              labels=c( "Baja aprobación"=0,
                                        "Alta aprobación"=1))

proc_lapop$rrss <- set_labels(proc_lapop$rrss,
                                    labels=c( "Si"=1,
                                              "No"=0))
proc_lapop$id_pol <- set_labels(proc_lapop$id_pol,
                              labels=c( "Si"=1,
                                        "No"=0))

# Guardar nueva base de datos ----

setwd("~/Estadisticacorrelacional/TrabajoFinal/output")

save(proc_lapop,file = "proc_lapop.rdata")

