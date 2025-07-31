pacman::p_load(tidyverse, 
               car,
               haven,
               sjPlot,
               sjmisc, 
               kableExtra, 
               psych,
               janitor,
               skimr,
               corrplot,
               ggmosaic,
               ggplot2,
               summarytools,
               broom,
               gmodels,
               rempsyc)

options(scipen = 999)
load("output/proc_elsoc.RData")

#Analisis

#Tabla descriptiva----

view(dfSummary(proc_elsoc$Nivel_Educacional, headings=FALSE, graph.col = FALSE))
view(dfSummary(proc_elsoc$Identificación_Partidaria, headings=FALSE, graph.col = FALSE))
view(dfSummary(proc_elsoc$Estatus_Social, headings=FALSE, graph.col = FALSE))

#coalición/ educación----

#Gráfico mosaico
ggplot(data = proc_elsoc) +
  geom_mosaic(aes(product(Identificación_Partidaria, Nivel_Educacional), fill = Nivel_Educacional)) + 
  geom_label(data = layer_data(last_plot(), 1),
             aes(x = (xmin + xmax)/2,
                 y = (ymin + ymax)/2,
                 label = paste0(round((.wt/sum(.wt)) * 100, 1), "%"))) + 
  scale_fill_discrete(name = "Nivel Educacional", 
                      labels = c("Sin estudios", 
                                 "Educación Básica o Preparatoria incompleta",
                                 "Educación Básica o Preparatoria completa", 
                                 "Educación Media o Humanidades incompleta", 
                                 "Educación Media o Humanidades completa", 
                                 "Técnica Superior incompleta", 
                                 "Técnica Superior completa", 
                                 "Universitaria incompleta", 
                                 "Universitaria completa", 
                                 "Estudios de posgrado (magíster o doctorado)")) +  # Nuevas categorías
  labs(title = "Distribución de Nivel Educacional por Identificación Partidaria", 
       x = "Identificación Partidaria", 
       y = "Nivel Educacional",
       caption = "Fuente: Encuesta Longitudinal de la Sociedad de Chile (ELSOC), 2016-2022.") +  
  theme_bw() +  
  theme(axis.text.x = element_blank())

#Tabla de contingencia entre Identificación Partidaria y Nivel Educacional
sjPlot::tab_xtab(
  var.row = proc_elsoc$Identificación_Partidaria,
  var.col = proc_elsoc$Nivel_Educacional,
  show.row.prc = TRUE,  
  show.col.prc = TRUE,
  show.summary = TRUE,
  encoding = "uft-8"
)

#Coalición y Estatus Social----

#Grafico de cajón
ggplot(proc_elsoc, aes(x = as.factor(Identificación_Partidaria_num), y = Estatus_Socialnum)) +
  geom_boxplot(aes(fill = as.factor(Identificación_Partidaria_num))) +
  scale_x_discrete(labels = c("No se identifica", "Se identifica")) +
  labs(title = "Distribución de Estatus Social por Identificación Partidaria", 
       x = "Identificación Partidaria", 
       y = "Estatus Social") +
  theme_minimal() +
  guides(fill = "none") 

# Correlación punto biserial entre Identificación Partidaria y Estatus Social
cor.test(proc_elsoc$Identificación_Partidaria_num, proc_elsoc$Estatus_Social)


# Prueba t de dos colas
resultado_ttest <- t.test(Estatus_Social ~ Identificación_Partidaria_num, 
                          data = proc_elsoc, 
                          alternative = "two.sided")

# Análisis t-test
Estatus_Socialnum <- as.numeric(as.character(proc_elsoc$Estatus_Social))
model <- t.test(Estatus_Socialnum ~ Identificación_Partidaria_num, 
                data = proc_elsoc, 
                var.equal = TRUE)

# Resumen del modelo
stats.table <- tidy(model, conf.int = TRUE)
nice_table(stats.table, broom = "t.test")

# Datos
t_value <- -11.96
df <- 14922
alpha <- 0.05

# Valores críticos
t_crit <- qt(alpha / 2, df, lower.tail = FALSE)

# Secuencia para la curva
x <- seq(-4, 4, length.out = 1000)
y <- dt(x, df)

# Gráfico
library(ggplot2)
ggplot(data.frame(x, y), aes(x, y)) +
  geom_line(color = "blue", size = 1) +
  geom_area(data = subset(data.frame(x, y), x < -t_crit | x > t_crit), 
            aes(y = y), fill = "red", alpha = 0.3) +
  geom_vline(xintercept = c(-t_crit, t_crit), color = "red", linetype = "dashed") +
  geom_vline(xintercept = t_value, color = "blue", size = 1) +
  labs(title = "Distribución t con dos colas", 
       subtitle = paste("t =", round(t_value, 2), ", df =", df),
       x = "Valor t", y = "Densidad") +
  theme_minimal()
