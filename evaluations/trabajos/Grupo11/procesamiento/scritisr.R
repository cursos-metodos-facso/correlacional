library(pacman)
p_load(tidyverse, #entorno de paquetes
       dplyr,# Manipulacion datos
       sjPlot, # Graficos y tablas
       sjmisc, # Descriptivos
       corrplot, # Correlaciones
       psych, # Test estadísticos
       kableExtra,
       labelled,
       sjlabelled,
       summarytools,
       ggplot2) # Tablas
options(scipen = 999)
rm(list = ls()) 
library(haven)
#IMPORTAR DAOTS
ELSOC_Long_2016_2022_v1_00 <- read_dta("input/ELSOC_Long_2016_2022_v1.00.dta")
#FILTRAR OLA 3
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



#MARCAR NA
data <- data %>%
  mutate(across(everything()    , ~ na_if(., -666))) %>%
  mutate(across(everything(), ~ na_if(., -777))) %>%
  mutate(across(everything(), ~ na_if(., -999))) %>%
  mutate(across(everything(), ~ na_if(., -888)))




#RESTAR 1 PARA EL CALCULO DE LA ESCALA
data <- data %>% select(t06_01, t06_02, t06_03, t06_04, t06_05, t06_06, t06_07, t06_08, t07_01, t07_02, m29, m0_sexo) %>%
  mutate(across(c(t06_01, t06_02, t06_03, t06_04, t06_05, t06_06, t06_07, t06_08, t07_01, t07_02), 
                ~ . - 1, .names = "{.col}"))
#AJUSTAR LABELS
data <- data %>%
  mutate(across(matches("^t06_|^t07_"), ~ set_labels(., 
                                                     labels = c("Totalmente en desacuerdo" = 0,
                                                                "En desacuerdo" = 1,
                                                                "Ni de acuerdo ni en desacuerdo" = 2,
                                                                "De acuerdo" = 3,
                                                                "Totalmente de acuerdo" = 4))))

#RENOMBRE, NO LO USAMOS AL FINAL.
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
#CALCULO DE LA ESCALA
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

#CORTES PARA LA RECODIFICACION
data <- data %>%
  mutate(isr_rec = cut(isr,
                       breaks = c(-Inf, 19, 29, 40),
                       labels = c("1", "2", "3"),
                       right = TRUE, 
                       include.lowest = TRUE))
#LABEL PARA LOS CORTES
data <- data %>%
  mutate(isr_rec = factor(isr_rec, 
                          levels = c(1, 2, 3),
                          labels = c(
                                     "Satisfacción baja",
                                     "Satisfacción moderada", 
                                     "Satisfaccion alta"
                                   )))
#LABELS PARA LOS VALORES DE LA VCARIABLE SEXO
data <- data %>% 
  mutate(sexo = factor(sexo,
                       levels = c(1,2),
                       labels = c("Hombre",
                                  "Mujer")))

#LABEL PARA ISR, ISR REC, Y SEXO
data <-data %>% mutate(isr = set_variable_labels(isr, "Escala de satisfaccion residencial"),
                       isr_rec = set_variable_labels(isr_rec, "Escala de satisfaccion residencial recodificada"),
                       sexo = set_variable_labels(sexo, "Sexo del entrevistado"))
#LABEL PARA SEXO DE NUEVO PQ NO HABIA RESULTADO
data <-data %>% mutate(sexo = set_variable_labels(sexo, "Sexo del entrevistado"))
#MATRIZ
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
#CRONBAJ
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
#BORRADORES DE LOS GRAFICOS Y CORRELACIONES
objeto <- data %>% select(isr, isr_rec)
sumario1 <- dfSummary(objeto,
                      title = "Variables dependientes")
  view(sumario1)

  sumario1 <- dfSummary(data %>% select(isr, isr_rec),
                        title = "Variables dependientes")
  
tab_xtab(data$sexo, data$isr_rec, show.row.prc = TRUE)

cor.test(data$isr, data$ingreso)


ggplot(data= data,
       mapping = aes(x = sexo)) + geom_bar(color= "black", fill= "lightblue")+
  labs(title ="Proporcion sexo", 
       x = "Sexo", 
       y = "Frecuencia",
       caption = "Fuente: Elaboración propia en base a ELSOC.") 


ggplot(data= data,
       mapping = aes(x =isr)) + geom_histogram(color= "black", fill= "lightblue")+
  labs(title ="Satisfaccion residencial", 
       x = "Satisfaccion", 
       y = "Frecuencia",
       caption = "Fuente: Elaboración propia en base a ELSOC.")


ggplot(data= data,
       mapping = aes(x =isr_rec)) + geom_bar(color= "black", fill= "lightblue")+
  labs(title ="Satisfaccion residencial en tramos", 
       x = "Satisfacción", 
       y = "Frecuencia",
       caption = "Fuente: Elaboración propia en base a ELSOC.")


ggplot(data= data,
       mapping = aes(x =isr_rec)) + geom_bar(color= "black", fill= "lightblue")+
  labs(title ="Satisfaccion residencial en tramos", 
       x = "Satisfacción", 
       y = "Frecuencia",
       caption = "Fuente: Elaboración propia en base a ELSOC.") 

ingreso_clean <- data[is.finite(data$ingreso), ]

# Crear el histograma
ggplot(data = ingreso_clean, mapping = aes(x = ingreso)) + 
  geom_histogram(color = "black", fill = "lightblue") +
  labs(
    title = "Distribución ingreso", 
    x = "Ingreso en pesos", 
    y = "Frecuencia",
    caption = "Fuente: Elaboración propia en base a ELSOC.")
    
    
  dfSummary(data %>% select(isr, isr_rec)) %>% view()

  scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))

#----- sumario sexo ingreso
library(ggplot2)
  options(scipen = 999)
  dfSummary(data %>% select (sexo, ingreso))%>% view
  
  
  #---- suamrio escala
  dfSummary(ELSOC_Long_2016_2022_v1_00 %>% select(t06_01, 
                                                                     t06_02,
                                                                     t06_03,
                                                                     t06_04,
                                                                     t06_05,
                                                                     t06_06,
                                                                     t06_07,
                                                                     t06_08,
                                                                     t07_01,
                                                                     t07_02)) %>% view()


  
  tab_corr(data %>% select(t06_01,
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
                           m0_sexo))
  
  tab_xtab(data$sexo, data$isr_rec, show.row.prc = TRUE)
  #-------------------------------------------
  plot_scatter(
    data = data, 
    x = isr,
    y = ingreso, 
    fit.line = "lm",      # Mostrar línea de regresión
    show.ci = TRUE,       # Mostrar intervalo de confianza
    dot.size = 3)
    
  cor.test(data$ingreso, data$isr)
  
#-----------------------------------
  
 t.test(data$sexo ~ data$isr, alterative = "greater", conf.lever = 0.95)
  
  
  dfSummary(data$isr,
            headings = FALSE,
            varnumbers = FALSE,
            labels.col = TRUE,
            na.col = TRUE,
            graph.col = TRUE,
            round.digits = 3) %>% view()
  
  grafico2
  ggplot(data= data,
         mapping = aes(x =isr_rec)) + geom_bar(color= "black", fill= "lightblue")+
    labs(title ="Satisfaccion residencial en tramos", 
         x = "Satisfacción", 
         y = "Frecuencia",
         caption = "Fuente: Elaboración propia en base a ELSOC.")
  
  
  
  
  dfSummary(data$isr,  headings = FALSE,         
            display.labels = FALSE,    
            display.type = FALSE,     
            varnumbers = FALSE,       
            valid.col = FALSE,        
            na.col = FALSE,           
            graph.col = FALSE         
  )%>% view(Method= "render")
  
  
  dfSummary(data$isr_rec,  headings = FALSE,         
            display.labels = FALSE,    
            display.type = FALSE,     
            varnumbers = FALSE,       
            valid.col = FALSE,        
            na.col = FALSE,           
            graph.col = FALSE         
  )%>% view(Method= "render")
  
  dfSummary(data$sexo,  headings = FALSE,         
            display.labels = FALSE,    
            display.type = FALSE,     
            varnumbers = FALSE,       
            valid.col = FALSE,        
            na.col = FALSE,           
            graph.col = FALSE         
  )%>% view(Method= "render")
  
  
  dfSummary(data$ingreso,  headings = FALSE,         
            display.labels = FALSE,    
            display.type = FALSE,     
            varnumbers = FALSE,       
            valid.col = FALSE,        
            na.col = FALSE,           
            graph.col = FALSE         
  )%>% view(Method= "render")
  
  
  
  suppressWarnings(
    ggplot(data, aes(x = isr, y = ingreso)) +
      geom_point(size = 3, color = "blue") +
      geom_smooth(method = "lm", se = TRUE, color = "red", na.rm = TRUE) +
      scale_y_continuous(
        breaks = c(500000, 1000000, 2000000, 5000000, seq(10000000, max(data$ingreso, na.rm = TRUE), by = 10000000)),
        labels = scales::label_number(scale = 1e-6, suffix = "M", big.mark = "")  # Formato con separadores de miles y "M" para millones
      ) +
      labs(
        title = "Gráfico de dispersión con línea de regresión",
        x = "ISR (Índice de satisfacción residencial)",
        y = "Ingreso(millones)",
        caption = "Fuente: Elaboración propia"
      ) +
      theme_minimal()
  )
  
  
datacorplot <- data %>% select(ingreso, isr)
datacorplot <- na.omit(datacorplot)


t.test(isr ~ sexo, data = data, alternative = "greater")


ggplot(data, aes(x = sexo, y = isr, fill = factor(sexo))) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Boxplot con outliers en rojo
  geom_point(
    data = aggregate(isr ~ sexo, data = data, FUN = mean),  # Calcular las medias por grupo
    aes(x = sexo, y = isr),
    color = "blue", size = 4, shape = 18  # Puntos azules para las medias
  ) +
  labs(
    title = "Diferencia de ISR entre Hombres y Mujeres",
    x = "Sexo",
    y = "Índice de Satisfacción Residencial (ISR)",
    caption = "Fuente: Elaboración propia"
  ) +
  scale_x_discrete(labels = c("2" = "Mujeres", "1" = "Hombres")) +  # Etiquetas para los grupos
  theme_minimal()


pearson1$estimate
pearson1

tab_xtab(data$sexo, data$isr_rec, show.row.prc = TRUE)

pearson1p
shi2raw

dfSummary(subsetsummary,  headings = FALSE,         
          display.labels = FALSE,    
          display.type = FALSE,     
          varnumbers = FALSE,       
          valid.col = FALSE,        
          na.col = FALSE,           
          graph.col = FALSE) %>% view()


subsetsummary <- ELSOC_Long_2016_2022_v1_00 %>% filter(ola==3) %>% select(t06_01, 
                                                                                t06_02,
                                                                                t06_03,
                                                                                t06_04,
                                                                                t06_05,
                                                                                t06_06,
                                                                                t06_07,
                                                                                t06_08,
                                                                                t0sub7_01,
                                                                                t07_02)

subsetsummary <- subsetsummary %>%   mutate(across(everything()    , ~ na_if(., -666))) %>%
  mutate(across(everything(), ~ na_if(., -777))) %>%
  mutate(across(everything(), ~ na_if(., -999))) %>%
  mutate(across(everything(), ~ na_if(., -888)))


t.test(data$isr ~ data$sexo, alternative = "two.sided")
