library(pacman)
pacman::p_load(tidyverse, # Manipulacion datos
               sjPlot, # Graficos y tablas
               sjmisc, # Descriptivos
               corrplot, # Correlaciones
               psych, # Test estadísticos
               kableExtra,
               psych,
               sjlabelled,
               sjplot,
               knitr,
               summarytools,
               ggplot2)

#cargar datos
load("C:/Users/baldo/Desktop/estadisticafinal/ELSOC_Long_2016_2022_v1.00.RData")
data <- elsoc_long_2016_2022.2 %>% filter(ola ==3) %>% select(s30_01, 
                                                              s30_02, 
                                                              s30_03,
                                                              s30_04,
                                                              s30_05,
                                                              s30_06,
                                                              s30_07,
                                                              s30_08,
                                                              m29,
                                                              m0_sexo,
                                                              comuna)


dataf <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(. %in% c(-999, -888, -777, -666), NA, .)))



dataf  <- dataf %>%
  select(s30_01, s30_02, s30_03, s30_04,s30_05, s30_06, s30_07, s30_08, m29, m0_sexo, comuna, m29) %>%
  mutate(across(c(s30_01, s30_02, s30_03, s30_04,s30_05, s30_06, s30_07, s30_08), 
                ~ . - 1, .names = "{.col}"))


dataf <- dataf %>%
  mutate(across(starts_with("s30_"), ~ set_labels(., 
                                                  labels = c("Totalmente en desacuerdo" = 0,
                                                             "En desacuerdo" = 1,
                                                             "Ni de acuerdo ni en desacuerdo" = 2,
                                                             "De acuerdo" = 3,
                                                             "Totalmente de acuerdo" = 4))))


dataf <- dataf %>% rename(
                ingreso = m29,
                sexo = m0_sexo)

dataf <- dataf %>% mutate(cpf = rowSums(select(., s30_01, 
                                               s30_02, 
                                               s30_03,
                                               s30_04,
                                               s30_05,
                                               s30_06,
                                               s30_07,
                                               s30_08),na.rm = TRUE))


tab_corr(dataf %>% 
           select(s30_01, s30_02, s30_03, s30_04, s30_05, s30_06, s30_07, s30_08), 
         triangle = "lower")

alpha(dataf %>% 
        select(s30_01, s30_02, s30_03, s30_04, s30_05, s30_06, s30_07, s30_08))


dataf1 <- dataf %>%
  mutate(cpf_recodificado = cut(cpf,
                                breaks = c(-Inf, 0, 7, 15, 24, 31, Inf),
                                labels = c("0", "1", "2", "3", "4", "5"),
                                right = TRUE, 
                                include.lowest = TRUE))

dataf1 <- dataf1 %>%
  mutate(sexo = factor(sexo, 
                       levels = c(1, 2),
                       labels = c("mujer",
                                  "hombre")))


#descriptivos
summary(dataf$cpf)
var(dataf$cpf, na.rm = TRUE)
summary(dataf$ingreso)
var(dataf$ingreso, na.rm = TRUE)
summary(dataf$sexo)


dfSummary(dataf1 %>% select(cpf, sexo, ingreso)) %>% view

cruce_sex_cpf <- dataf1 %>% select(sexo, cpf_recodificado)

sjtab(cruce) ### ejemplo

tab_xtab(dataf1$sexo, dataf1$cpf_recodificado, show.row.prc = TRUE)

cruce_cpf_ingreso <- dataf1 %>% select(cpf, ingreso)

ggplot(cruce_cpf_ingreso, aes(x = cpf, y = ingreso)) +
  geom_point()

cor.test(dataf1$cpf, dataf1$ingreso)

dfSummary(ELSOC_Long_2016_2022_v1_00 %>% 
            select(t06_01, t06_02, t06_03, t06_04, t06_05, 
                   t06_06, t06_07, t06_08, t07_01, t07_02)) %>%
  view()






