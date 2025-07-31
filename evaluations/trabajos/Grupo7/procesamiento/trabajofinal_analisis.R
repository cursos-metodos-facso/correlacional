# Análisis de los datos procesados trabajo final de correlacional 
# Cargar librerías ----

pacman::p_load(tidyverse, 
               sjPlot, 
               sjmisc, 
               rstatix, 
               broom, 
               corrplot, 
               psych, 
               kableExtra,
               haven,
               sjlabelled,
               ltm) 

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

# Cargar base de datos ----

setwd("~/Estadisticacorrelacional/TrabajoFinal/output")

load("~/Estadisticacorrelacional/TrabajoFinal/output/proc_lapop.rdata")

# Descriptivos de las variables ----
# Evaluación del gobierno de Pinochet - frq
tabla1 <- proc_lapop %>%
  group_by(aprob_pino_dum) %>%
  summarise(n= n()) %>%
  mutate(prop = round((n/sum(n))* 100, 2)) %>%
  na.omit

tabla1 %>% kable(format = "html",
                 align = "c",
                 col.names = c("Aprobación del  gobierno de Pinochet", "Frecuencia", "Frecuencia porcentual"),
                 caption = "Frecuencia de aprobación del gobierno de Pinochet") %>%
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a LAPOP CHILE 2023")

# Descriptivos aprob_pino
tab5 <- proc_lapop %>%
  summarise(
    n = n(),
    min = min(aprob_pino_num, na.rm = T),
    max = max(aprob_pino_num, na.rm = T),
    media = round(mean(aprob_pino_num, na.rm = T), 2),
    sd = round(sd(aprob_pino_num, na.rm = T), 2),
    Q1 = quantile(aprob_pino_num, 0.25, 2),
    mediana = median(aprob_pino_num, na.rm = T, 2),
    Q3 = quantile(aprob_pino_num, 0.75, 2))

tabla5 <- tab5 %>%
  kable(format = "html",
        align = "c",
        col.names = c("Tamaño muestral", "Mínimo", "Máximo", "Media", "Desviación estándar", "Cuantil 1", "Mediana", "Cuantil 3"),
        caption = "Tabla 5: Estadísticos descriptivos aprobación del gobierno de Pinochet") %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a LAPOP CHILE 2023")



# Nivel educacional - frq
tabla2 <- proc_lapop %>%
  group_by(educ) %>%
  summarise(n= n()) %>%
  mutate(prop = round((n/sum(n))* 100, 2)) %>%
  na.omit

tabla2 %>% kable(format = "html",
                 align = "c",
                 col.names = c("Nivel educacional", "Frecuencia", "Frecuencia porcentual"),
                 caption = "Frecuencia de nivel educacional") %>%
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a LAPOP CHILE 2023")

# Descriptivos educ

tab6 <- proc_lapop %>%
  summarise(
    n = n(),
    min = min(educ, na.rm = T),
    max = max(educ, na.rm = T),
    media = round(mean(educ, na.rm = T), 2),
    sd = round(sd(educ, na.rm = T), 2),
    Q1 = quantile(educ, 0.25, 2),
    mediana = median(educ, na.rm = T, 2),
    Q3 = quantile(educ, 0.75, 2))

tabla6 <- tab6 %>%
  kable(format = "html",
        align = "c",
        col.names = c("Tamaño muestral", "Mínimo", "Máximo", "Media", "Desviación estándar", "Cuantil 1", "Mediana", "Cuantil 3"),
        caption = "Tabla 6: Estadísticos descriptivos nivel educacional") %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a LAPOP CHILE 2023")

# Identificar fake news en redes sociales - frq
tabla3 <- proc_lapop %>%
  group_by(rrss) %>%
  summarise(n= n()) %>%
  mutate(prop = round((n/sum(n))* 100, 2)) %>%
  na.omit

tabla3 %>% kable(format = "html",
                 align = "c",
                 col.names = c("Consumo de información falsa en redes sociales", "Frecuencia", "Frecuencia porcentual"),
                 caption = "Frecuencia del consumo de información falsa en redes sociales") %>%
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a LAPOP CHILE 2023")

# Descriptivos rrss

tab7 <- proc_lapop %>%
  summarise(
    n = n(),
    min = min(rrss, na.rm = T),
    max = max(rrss, na.rm = T),
    media = round(mean(rrss, na.rm = T), 2),
    sd = round(sd(rrss, na.rm = T), 2),
    Q1 = quantile(rrss, 0.25, 2),
    mediana = median(rrss, na.rm = T, 2),
    Q3 = quantile(rrss, 0.75, 2))

tabla7 <- tab7 %>%
  kable(format = "html",
        align = "c",
        col.names = c("Tamaño muestral", "Mínimo", "Máximo", "Media", "Desviación estándar", "Cuantil 1", "Mediana", "Cuantil 3"),
        caption = "Tabla 7: Estadísticos descriptivos consumo de información falsa en redes sociales") %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a LAPOP CHILE 2023")

# Identificación con partido político
tabla4 <- proc_lapop %>%
  group_by(id_pol) %>%
  summarise(n= n()) %>%
  mutate(prop = round((n/sum(n))* 100, 2)) %>%
  na.omit

tabla4 %>% kable(format = "html",
                 align = "c",
                 col.names = c("Simpatía con algún partido político", "Frecuencia", "Frecuencia porcentual"),
                 caption = "Frecuencia de la simpatía con algún partido político") %>%
  kableExtra::kable_classic(full_width = FALSE, position = "center", font_size = 14) %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a LAPOP CHILE 2023")

# Descriptivos id_pol

tab8 <- proc_lapop %>%
  summarise(
    n = n(),
    min = min(id_pol, na.rm = T),
    max = max(id_pol, na.rm = T),
    media = round(mean(id_pol, na.rm = T), 2),
    sd = round(sd(id_pol, na.rm = T), 2),
    Q1 = quantile(id_pol, 0.25, 2),
    mediana = median(id_pol, na.rm = T, 2),
    Q3 = quantile(id_pol, 0.75, 2))

tabla8 <- tab8 %>%
  kable(format = "html",
        align = "c",
        col.names = c("Tamaño muestral", "Mínimo", "Máximo", "Media", "Desviación estándar", "Cuantil 1", "Mediana", "Cuantil 3"),
        caption = "Tabla 7: Estadísticos descriptivos simpatía con algún partido político") %>%
  kableExtra::add_footnote(label = "Fuente: Elaboración propia en base a LAPOP CHILE 2023")



# Análisis bivariado ----

test_ej1 <- t.test(proc_lapop$educ ~ proc_lapop$aprob_pino_num, 
                   alternative = "greater",
                   conf.level = 0.95)

test_ej1

punbis1 <- cor.test(proc_lapop$aprob_pino, proc_lapop$rrss) # Aprobación a Pinochet y fake news en redes sociales.

punbis2 <- cor.test(proc_lapop$aprob_pino, proc_lapop$id_pol) # Aprobación a Pinochet y si se identifica con partido político.



# Gráficos ----

db_g1 <- proc_lapop %>%
  group_by(aprob_pino_dum) %>%
  summarise(
    media_educ = mean(educ, na.rm = TRUE),
    error_std = sd(educ, na.rm = TRUE) / sqrt(n()),
    ci_inf = media_educ - qt(0.975, df = n() - 1) * error_std,
    ci_sup = media_educ + qt(0.975, df = n() - 1) * error_std
  ) %>%
  na.omit()

ggplot(data = db_g1,
       mapping = aes(x = aprob_pino_dum, y = media_educ, group = aprob_pino_dum)) + # especificamos datos y mapping 
  geom_bar(stat = "identity", color = "black", fill = "steelblue") + # agregamos geometria
  geom_errorbar(aes(ymin = ci_inf, ymax = ci_sup), width = 0.2, color = "red") + # agregamos intervalo de confianza calculado previamente
  scale_y_continuous(limits = c(0,6), n.breaks = 6) + # agregamos escala en eje Y y ponemos limites minimos y maximos
  geom_text(aes(label = round(media_educ,2)), vjust = 1.5, colour = "white") + # agregamos los valores de la media para cada barra/grupo
  labs(title ="Media de aprobación del gobierno de Pinochet según nivel educacional", 
       x = "Aprobación del gobierno de Pinochet", 
       y = "Nivel educacional",
       caption = "Fuente: Elaboración propia en base a LAPOP CHILE 2023.") + # agregamos titulo, nombres a los ejes y fuente 
  theme_bw() # especificamos tema

ggplot(proc_lapop, aes(x = aprob_pino, fill = as.factor(id_pol))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("skyblue", "orange"), 
                    labels = c("No simpatiza", "Simpatiza")) +
  labs(x = "Evaluación al gobierno de Pinochet",
       y = "Densidad",
       fill = "Simpatía",
       title = "Distribución de evaluación por grupo") +
  theme_minimal()


ggplot(proc_lapop, aes(x = aprob_pino, fill = as.factor(rrss))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("purple", "green"), 
                    labels = c("No reconoce", "reconoce")) +
  labs(x = "Evaluación al gobierno de Pinochet",
       y = "Densidad",
       fill = "Reconocimiento fake news",
       title = "Distribución de evaluación por grupo") +
  theme_minimal()
