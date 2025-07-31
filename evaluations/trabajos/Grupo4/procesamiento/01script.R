pacman::p_load(
  tidyverse, # Manipulacion datos
  sjPlot, # Graficos y tablas
  sjmisc, # Descriptivos
  kableExtra, # Tablas
  psych, # Estadísticos
  broom,
  haven,
  readxl,
  rempsyc,
  flextable,
  ggrepel,
  sjlabelled,
  summarytools
  
)

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

###------------------------------PROCESAMIENTO DE DATOS------------------------------###
simcerbd <- read_dta("input/data/Simce2m2022_rbd_final.dta")
pobrezamultidimensional <- read_excel("input/data/Estimaciones_Indice_Pobreza_Multidimensional_Comunas_2022.xlsx")

#Elegir variables a utilizar de la base de datos SIMCE
simcerbd_proc <- dplyr::select(simcerbd,
                               cod_com="cod_com_rbd",
                               depe="cod_depe2",
                               prom_lect="prom_lect2m_rbd",
                               prom_mat="prom_mate2m_rbd",
                               insuficiente_lect="palu_eda_ins_lect2m_rbd",
                               insuficiente_mat="palu_eda_ins_mate2m_rbd"
                               
)

#Promedio SIMCE por establecimiento
simcerbd_proc = simcerbd_proc %>% 
  rowwise() %>%
  mutate(prom_simce = mean(c(prom_lect, prom_mat))) %>% 
  ungroup()

#Promedio de estudiantes con puntajes insuficientes por establecimiento
simcerbd_proc = simcerbd_proc %>% 
  rowwise() %>%
  mutate(insuficiente_simce = mean(c(insuficiente_lect, insuficiente_mat))) %>% 
  ungroup()


#Filtrar solo establecimientos educacionales municipales
simcerbd_proc <- dplyr::filter(simcerbd_proc, depe == 1)

#Promedio puntaje SIMCE por comuna
simcerbd_proc = simcerbd_proc %>%
  group_by(cod_com) %>%
  summarise(
    prom_simce = mean(prom_simce),
    insuficiente_simce = mean (insuficiente_simce)) 

#Elegir variables de base de datos de pobreza comunal multidimensional
pobreza_proc <- dplyr::select(pobrezamultidimensional,
                              cod_com,
                              com= "Nombre comuna",
                              reg="Región",
                              pob_multi="Porcentaje de personas en situación de pobreza multidimensional 2022"
)


#Juntar ambas bases de datos a partir del código de comuna
simce <- merge(simcerbd_proc, pobreza_proc, by="cod_com")

#Filtrar solo región Metropolitana
simce <- dplyr::filter(simce, reg == "Metropolitana")

#Pasar de valor relativo a %
simce$pob_multi <- simce$pob_multi * 100

#Recodificar variable pobreza multidimensional a dicotómica
simce <- simce %>%
  mutate(pob_multi_dic = ifelse(pob_multi >= 19, 1, 0))


#Nombrar variable pobreza multidimensional
simce$pob_multi_dic <- car::recode(simce$pob_multi_dic,
                                   recodes = c("0 = 'Pobreza comunal bajo promedio nacional';
                                                       1 = 'Pobreza comunal sobre promedio nacional'"))
#Recodificar variable promedio simce a dicotómica                                                      
simce <- simce %>%
  mutate(prom_simce_dic= ifelse(prom_simce >= 248.84, 1, 0))

#Nombrar variable promedio simce dicotómica  
simce$prom_simce_dic <- car::recode(simce$prom_simce_dic,
                                    recodes = c("0 = 'Puntaje Simce bajo promedio nacional';
                                                       1 = 'Puntaje Simce sobre promedio nacional'"))

#Recodificar variable porcentaje de estudiantes con puntajes insuficiente a ordinal  
simce <- mutate(simce,
                nivel_insuficiencia_or = case_when(insuficiente_simce >= 0 & insuficiente_simce <= 33 ~ "0",
                                                   insuficiente_simce > 33 & insuficiente_simce <= 67 ~ "1",
                                                   insuficiente_simce > 67 & insuficiente_simce <= 100 ~ "2"
                ))  

#Nombrar variable nivel de puntajes insuficientes a ordinal
simce$nivel_insuficiencia_or <- car::recode(simce$nivel_insuficiencia_or,
                                            recodes = c("0 = 'Bajo';
                                                       1 = 'Medio';
                                                2 = 'Alto'")) 

#Transformar a variable ordinal
simce$nivel_insuficiencia_or <- factor(simce$nivel_insuficiencia_or, levels = c("Bajo", "Medio", "Alto"), ordered = TRUE)

#Base final a utilizar
simce <- dplyr::select(simce,
                       cod_com,
                       com,
                       prom_simce,
                       pob_multi,
                       insuficiente_simce,
                       pob_multi_dic,
                       prom_simce_dic,
                       nivel_insuficiencia_or
)

#Poner labels 
set_label(simce$nivel_insuficiencia_or) <- "Nivel de alumnos con puntajes Simce insuficientes"
set_label(simce$pob_multi_dic) <- "Nivel de pobreza multidimensional comunal"

###------------------------------Análisis descriptivos------------------------------###
#Tabla 1
tab1 <- simce %>% 
  summarise(
    n = n(), # Tamaño muestral
    min = min(pob_multi, na.rm = T), # Mínimo
    max = max(pob_multi, na.rm = T), # Máximo
    media = round(mean(pob_multi, na.rm = T), 2), # Media redondeada a 2 digitos
    sd = round(sd(pob_multi, na.rm = T), 2),
    Q1 = quantile(pob_multi, 0.25, 2), 
    mediana = median(pob_multi, na.rm = T, 2),
    Q3 = quantile(pob_multi, 0.75, 2)
  )

tabla1 <- tab1 %>% 
  kable(format = "html",
        align = "c",
        col.names = c("Tamaño muestral", "Mínimo", "Máximo", "Media", "Desv. estándar", "Cuartil 1", "Mediana (Cuartil 2)", "Cuartil 3"),
        caption = "Pobreza multidimensional comunal en RM") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a MIDESOF 2022.")

tabla1

#Tabla 2
tabla2 <- simce %>%
  group_by(pob_multi_dic) %>% # agrupamos por pais y variable
  summarise(n = n()) %>% # contamos por categ de respuesta
  mutate(prop = round((n / sum(n)) * 100, 2)) %>% # porcentaje
  na.omit() 

tabla2 %>% kable(format = "html",
                 align = "c",
                 col.names = c("Pobreza multidimensional Comunal", "Frecuencia", "Frecuencia porcentual"),
                 caption = "Nivel de pobreza multidimensional comunal con respecto al promedio nacional") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a MIDESOF 2022.")

#Tabla 3
tab3 <- simce %>% 
  summarise(
    n = n(), # Tamaño muestral
    min = min(prom_simce, na.rm = T), # Mínimo
    max = max(prom_simce, na.rm = T), # Máximo
    media = round(mean(prom_simce, na.rm = T), 2), # Media redondeada a 2 digitos
    sd = round(sd(prom_simce, na.rm = T), 2),
    Q1 = quantile(prom_simce, 0.25, 2), 
    mediana = median(prom_simce, na.rm = T, 2),
    Q3 = quantile(prom_simce, 0.75, 2)
  )

tabla3 <- tab3 %>% 
  kable(format = "html",
        align = "c",
        col.names = c("Tamaño muestral", "Mínimo", "Máximo", "Media", "Desv. estándar", "Cuartil 1", "Mediana (Cuartil 2)", "Cuartil 3"),
        caption = "Puntaje SIMCE promedio comunal en la Región Metropolitana") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a la Agencia de Calidad de la Educación 2022.")

tabla3

#Tabla 4
tabla4 <- simce %>%
  group_by(prom_simce_dic) %>% # agrupamos por pais y variable
  summarise(n = n()) %>% # contamos por categ de respuesta
  mutate(prop = round((n / sum(n)) * 100, 2)) %>% # porcentaje
  na.omit() 

tabla4 %>% kable(format = "html",
                 align = "c",
                 col.names = c("Nivel de puntaje Simce promedio comunal", "Frecuencia", "Frecuencia porcentual"),
                 caption = "Nivel de puntaje Simce promedio comunal") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a la Agencia de Calidad de la Educación 2022.")  

#Tabla 5
tab5 <- simce %>% 
  summarise(
    n = n(), # Tamaño muestral
    min = min(insuficiente_simce, na.rm = T), # Mínimo
    max = max(insuficiente_simce, na.rm = T), # Máximo
    media = round(mean(insuficiente_simce, na.rm = T), 2), # Media redondeada a 2 digitos
    sd = round(sd(insuficiente_simce, na.rm = T), 2),
    Q1 = quantile(insuficiente_simce, 0.25, 2), 
    mediana = median(insuficiente_simce, na.rm = T, 2),
    Q3 = quantile(insuficiente_simce, 0.75, 2)
  )

tabla5 <- tab5 %>% 
  kable(format = "html",
        align = "c",
        col.names = c("Tamaño muestral", "Mínimo", "Máximo", "Media", "Desv. estándar", "Cuartil 1", "Mediana (Cuartil 2)", "Cuartil 3"),
        caption = "Tabla 5. Porcentaje promedio de alumnos con puntajes SIMCE insuficientes a nivel comunal") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a la Agencia de Calidad de la Educación 2022.")

tabla5

#Tabla 6
tabla6 <- simce %>%
  group_by(nivel_insuficiencia_or) %>% # agrupamos por pais y variable
  summarise(n = n()) %>% # contamos por categ de respuesta
  mutate(prop = round((n / sum(n)) * 100, 2)) %>% # porcentaje
  na.omit() 

tabla6 %>% kable(format = "html",
                 align = "c",
                 col.names = c("Nivel promedio de puntajes insuficientes", "Frecuencia", "Frecuencia porcentual"),
                 caption = "Nivel promedio de alumnos con puntajes insuficientes Simce a nivel comunal") %>% 
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>% 
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a la Agencia de Calidad de la Educación 2022.")

###------------------------------Análisis bivariados------------------------------###
#Correlación de Pearson: promedio SIMCE Y pobreza multidimensional (H1)
cor_results <- cor.test(x = simce$pob_multi, 
                        y = simce$prom_simce,
                        method = "pearson",
                        use = "complete.obs") 

cor_results

stats.table <- tidy(cor_results)

tablaH1 <- stats.table %>%
  dplyr::mutate(
    estimate = round(estimate, 2),
    statistic = round(statistic, 2),
    ic_95 = paste0("[", round(conf.low, 2), ",", round(conf.high, 2), "]"),
    stars = gtools::stars.pval(p.value),
    p_value = case_when(
      p.value < 0.05 & p.value > 0.01 ~ "< 0.05",
      p.value < 0.01 & p.value > 0.001 ~ "< 0.01",
      p.value < 0.001 ~ "< 0.001",
      TRUE ~ ""
    ),
    p_value = paste0(p_value, stars)
  ) %>%
  dplyr::select(estimate, statistic, p_value, parameter, method, alternative, ic_95) %>%
  kableExtra::kable(
    col.names = c("Estimación", "t", "p-value", "df", "Método", "Alternativa", "95% IC"),
    booktabs = T
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = T,
    latex_options = "hold_position",
    position = "center"
  )

tablaH1

graficoH1 <- ggplot(data = simce,
       mapping = aes(x = pob_multi, y = prom_simce, color = com)) + # especificamos datos y mapping  
  geom_point(size = 4) + # agregamos geometria y color 
  geom_smooth(stat = "smooth", position = "identity", method = "lm", 
              colour = "blue", size = 0.7) +
  scale_y_continuous(limits = c(190,310), breaks = seq(190,310, by = 15)) + 
geom_text(aes(label = com), size = 3, color = "black") +
  scale_x_continuous(limits = c(2,32), breaks = seq(2,32, by = 6)) +
  labs(title ="Correlación de Pearson entre Promedio Simce comunal y Pobreza multidimensional comunal", 
       y = "Promedio Simce comunal", 
       x = "Pobreza multidimensional comunal (%)",
       caption = "Fuente: Elaboración propia en base a la Agencia de Calidad de la Educación y MIDESOF 2022 .",
color = "Comuna") +  
  theme_bw() 

graficoH1

# Prueba t: promedio SIMCE y pobreza multidimensional dicotómica (H2)
test <- t.test(simce$prom_simce ~ simce$pob_multi_dic, 
               alternative = "greater",
               conf.level = 0.95)
test

stats.table <- tidy(test, conf_int = T)
tablaH2 <- nice_table(stats.table, broom = "t.test")

tablaH2

simce_2 = simce %>% 
  group_by(pob_multi_dic) %>%
  summarise(prom_simce = mean(prom_simce))

graficoH2 <- ggplot(data = simce_2,
                    mapping = aes(x = pob_multi_dic, y = prom_simce, fill = pob_multi_dic)) + # especificamos datos y mapping 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(prom_simce,2)), vjust = 1.5, colour = "white") +
  labs(title ="Promedio de puntaje Simce según nivel de pobreza multidimensional comunal", 
       x = "nivel de Pobreza multidimensional comunal con respecto al promedio nacional", 
       y = "Promedio de puntaje Simce comunal",
       fill = "nivel de pobreza comunal",
       caption = "Fuente: Elaboración propia en base a la Agencia de Calidad de la Educación y MIDESOF 2022.") + # agregamos titulo, nombres a los ejes y fuente 
  theme_bw()# agregamos geometria 

graficoH2

# Chi cuadrado: pobreza multidimensional dicotómica y nivel de insuficiencia ordinal (H3)  
contingenciaH3 <- simce %>%
  sjPlot::sjtab(pob_multi_dic,
                nivel_insuficiencia_or,
                show.row.prc = TRUE, 
                show.col.prc = TRUE
  )

contingenciaH3

###------------------------------Guardar base de datos------------------------------###
save(simce, file="output/simce.Rdata")


