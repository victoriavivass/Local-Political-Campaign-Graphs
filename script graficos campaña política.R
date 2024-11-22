setwd("C:/Users/Victoria/OneDrive/Escritorio/Datos Valencia")
rm(list = ls())
getwd()
dir()
library(ggplot2)
library(jpeg)
library(cowplot)
library(grid)
library(dplyr)
library(tidyr)

#datos 
 
?read.csv
preelectoral <- read.csv("3402_num.csv", header = TRUE, sep = ";")
preelectoralvalencia <- subset(preelectoral, preelectoral$GESALC_VLC != 0)
str(preelectoralvalencia)

############################################################################3
#1. Gestión de Joan Ribó

preelectoralvalencia$gestion <- factor(preelectoralvalencia$GESALC_VLC, levels = c(1,2,3,4,5,8,9), labels = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala", "No sabe", "No contesta"))
str(preelectoralvalencia$gestion)
unique(preelectoralvalencia$gestion)
table(preelectoralvalencia$gestion)
?scale_x_discrete

color_naranja <- rgb(255, 140, 0, maxColorValue = 255)
plot1 <- ggplot(preelectoralvalencia, aes(gestion)) + geom_bar(color = "black", fill = color_naranja) + labs(x = "N = 1056", y = "Total") + geom_text(aes(label = scales::percent(..count../sum(..count..))), stat = "count", vjust = -0.5) 
                                                                                                                                                      
plot1 <- plot1 + ggtitle("Valoración de la gestión de Joan Ribó")+ theme_bw() +
  theme(axis.text.y = element_text(size = 10, hjust = 0.5),  
        axis.title.y = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ruta_imagen <- "Joan_Ribó_2019.jpg"

imagen <- readJPEG(ruta_imagen)

# Crear un objeto grob para la imagen
imagen_grob <- rasterGrob(imagen, width = unit(3, "inches"), height = unit(4, "inches"))

# Colocar el gráfico y la imagen juntos
plot_with_image <- plot_grid(plot1, imagen_grob, ncol = 2, align = "h", rel_widths = c(1, 0.5))


# Mostrar el gráfico combinado
plot_with_image

########################################################################
#2. Valoración del líder
#cada columna de LIDERVALENAY es un candidato:

#Eliminar los valores de NS, NS y No conoce
# Variables en las que deseas aplicar la condición
variables <- c("LIDERVALEN_1", "LIDERVALEN_2", "LIDERVALEN_3", "LIDERVALEN_4", "LIDERVALEN_5", "LIDERVALEN_6")

# Valores que deseas excluir de cada variable
valores_a_excluir <- c(98, 99, 97)

# Crear una copia del data frame original

preelectoralvalencia_limpio <- preelectoralvalencia[!apply(preelectoralvalencia[variables], 1, function(x) any(x %in% valores_a_excluir)), ]
preelectoralvalencia_limpio$grupo_edad <- cut(preelectoralvalencia_limpio$EDAD, breaks = seq(18, 100, by = 10), labels = c("De 18 a 30", "De 30 a 40", "De 40 a 50", "De 50 a 60", "De 60 a 70", "De 70 a 80", "De 80 a 90", "90+"))
preelectoralvalencia_limpio <- na.omit(preelectoralvalencia_limpio[, c("LIDERVALEN_1", "LIDERVALEN_2", "LIDERVALEN_3", "LIDERVALEN_4", "LIDERVALEN_5", "LIDERVALEN_6", "grupo_edad", "SEXO", "EDAD")])

##2.1 Sandra Gómez PSOE

mean1 <- round(mean(preelectoralvalencia_limpio$LIDERVALEN_1), 2)


#2.2 María José Catalá (PP)


mean2 <- round(mean(preelectoralvalencia_limpio$LIDERVALEN_2), digits = 2)
mean2

#2.3 Joan Ribó (Compromís)

mean3 <- round(mean(preelectoralvalencia_limpio$LIDERVALEN_3), digits = 2)
mean3


#2.4 Fernando Giner (CS)

mean4 <- round(mean(preelectoralvalencia_limpio$LIDERVALEN_4), digits = 2)
mean4

#2.5 Juan Manuel Badenas (Vox)

mean5 <- round(mean(preelectoralvalencia_limpio$LIDERVALEN_5), digits = 2)
mean5

#2.6 Pilar Lima (UP)

mean6 <- round(mean(preelectoralvalencia_limpio$LIDERVALEN_6), digits = 2)
mean6

mean1; mean2; mean3; mean4; mean5; mean6


#POR EDAD EN INTERVALOS DE DIEZ AÑOS

# Supongamos que tienes un data frame llamado 'datos' con las variables 'grupo_edad', 'LIDERVALEN_1', 'LIDERVALEN_2', ...


# Convertir el formato de datos al formato largo usando pivot_longer
datos_largos <- preelectoralvalencia_limpio %>%
  pivot_longer(cols = c("LIDERVALEN_1", "LIDERVALEN_2", "LIDERVALEN_3", "LIDERVALEN_4", "LIDERVALEN_5", "LIDERVALEN_6"), 
               names_to = "Lider", values_to = "Valoracion")

# Convertir Lider a factor con etiquetas
etiquetas <- c("Sandra Gómez(PSOE)", "María José Catalá(PP)", "Joan Ribó(Compromís)", "Fernando Giner(CS)", "Juan Manuel Baldenas(VOX)", "Pilar Lima (UP)")
datos_largos$Lider <- factor(datos_largos$Lider, labels = etiquetas)

# Convertir Valoracion a factor con niveles y orden
datos_largos$Valoracion <- factor(datos_largos$Valoracion, levels = 1:10, ordered = TRUE)

# Eliminar filas con NA
datos_largos <- na.omit(datos_largos)

# Mostrar estructura de datos
str(datos_largos)

# Convertir Valoracion a una variable numérica
datos_largos$Valoracion <- as.numeric(as.character(datos_largos$Valoracion))

# Reemplazar los NA en Valoracion por 0
datos_largos$Valoracion[is.na(datos_largos$Valoracion)] <- 0

# Calcular la media de la valoración por grupo de edad y líder
media_valoracion <- aggregate(Valoracion ~ grupo_edad + Lider, data = datos_largos, FUN = mean)

# Renombrar la columna resultante
colnames(media_valoracion)[3] <- "Media_Valoracion"

# Visualizar los resultados
print(media_valoracion)

# Define una paleta de colores personalizada
colores_lider <- c("Sandra Gómez(PSOE)" = "red",
                   "María José Catalá(PP)" = "blue",
                   "Joan Ribó(Compromís)" = "darkorange",
                   "Fernando Giner(CS)" = "lightcoral",
                   "Juan Manuel Baldenas(VOX)" = "green",
                   "Pilar Lima (UP)" = "purple")

# Crear un gráfico de dot plot facetado por Lider en cuadrados
dot_plot <- ggplot(media_valoracion, aes(x = Media_Valoracion, y = grupo_edad, color = Lider)) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.2f", Media_Valoracion)), vjust = -0.5, size = 3) +
  facet_wrap(~ Lider, scales = "free") +
  scale_color_manual(values = colores_lider) +
  labs(title = "Valoración de cada Líder político por grupos de edad\n", x = "Media") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5, size = 8),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"),
        legend.position = "none")

# Añadir ajustes para centrar el título
dot_plot <- dot_plot + theme(plot.title = element_text(hjust = 0.5))

# Imprimir el gráfico
print(dot_plot)


#POR GÉNERO 

datos_largos$SEXO <- factor(datos_largos$SEXO, levels = c(1, 2), labels = c("Hombre", "Mujer"))

# Calcular la media de la valoración por género
media_valoracionsexo <- aggregate(Valoracion ~ SEXO + Lider, data = datos_largos, FUN = mean)

# Renombrar la columna resultante
colnames(media_valoracionsexo)[3] <- "Media_Valoracionsexo"

# Visualizar los resultados
print(media_valoracionsexo)

# Crear un gráfico de dot plot facetado por Lider en cuadrados
dot_plotsexo <- ggplot(media_valoracionsexo, aes(x = Media_Valoracionsexo, y = reorder(Lider, -Media_Valoracionsexo), color = Lider)) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.2f", Media_Valoracionsexo)), vjust = -0.5, size = 3) +
  facet_wrap(~ SEXO, scales = "free") +
  scale_color_manual(values = colores_lider) +  # Aplicar la paleta de colores
  labs(title = "Valoración de cada Líder político por género", x = "Media") +  # Agregar título centrado y cambiar título del eje x
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5, size = 8),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"),
        legend.position = "none")

# Añadir ajustes para centrar el título
dot_plotsexo <- dot_plotsexo + theme(plot.title = element_text(hjust = 0.5))

# Imprimir el gráfico
print(dot_plotsexo)

#MEDIA DE VALORACIÓN EN GENERAL 

# Calcular la media de la valoración por líder
media_valoracion_candidato <- aggregate(Valoracion ~ Lider, data = datos_largos, FUN = mean)

# Renombrar la columna resultante
colnames(media_valoracion_candidato)[2] <- "Media_ValoracionCandidato"

# Crear un gráfico de dot plot para mostrar las medias de valoración por candidato
dot_plot_candidato <- ggplot(media_valoracion_candidato, aes(x = Media_ValoracionCandidato, y = Lider, color = Lider)) +
  geom_point(size = 3, shape = 16, stroke = 1.5) +  # Aumentar tamaño y personalizar forma de los puntos
  geom_text(aes(label = sprintf("%.2f", Media_ValoracionCandidato)), vjust = -0.5, size = 5) +
  labs(title = "Media de Valoración por Candidato", x = "Media") +
  scale_color_manual(values = colores_lider) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10, hjust = 0.5),  # Aumentar tamaño de texto en el eje y
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +  # Centrar el título
  coord_flip()

# Imprimir el gráfico
print(dot_plot_candidato)


####################################################################################################################################
#3. Qué le da más importancia a la hora de votar 
rm(list = ls())
rm(preelectoralseccion3)

preelectoralseccion3 <- data.frame(preelectoralvalencia$IMPORMUN, preelectoralvalencia$INTENCIONM)
preelectoralseccion3 <- na.omit(preelectoralseccion3)
colnames(preelectoralseccion3) [1] <- "Importancia"
colnames(preelectoralseccion3) [2] <- "Intencion de voto"
unique(preelectoralseccion3$`Intencion de voto`)
unique(preelectoralseccion3$Importancia)

preelectoralseccion3$`Intencion de voto` <- factor(preelectoralseccion3$`Intencion de voto`, levels = c(9998,2,8,1,1001,5,3,9999,1202,9,8995,4,9977,9997,6), labels = c("No sabe todavía", "PP(Partido Popular)", "Podemos", "PSOE", "Compromís", "Ciudadanos", "VOX", "No Contesta", "En Común + Unidas Podemos", "Izquierda Unida", "Otro Partido", "Podemos", "Voto Nulo", "PACMA", ""))
preelectoralseccion3$Importancia <- factor(preelectoralseccion3$Importancia, levels = c(2,1,3,9,8,4), labels = c("Al candidato que se presenta como alcalde", "Al Partido Político", "Al programa", "No sabe", "No contesta", "A todo por igual"))

respuestas_permitidas <- c("PSOE", "PP(Partido Popular)", "VOX", "Podemos", "Compromís", "Ciudadanos", "No sabe todavía", "No contesta")
preelectoralseccion3 <- subset(preelectoralseccion3, `Intencion de voto` %in% respuestas_permitidas)

table(preelectoralseccion3$Importancia, preelectoralseccion3$`Intencion de voto`)

#Grafiquito

colores_partido <- c("PSOE" = "red",
                   "PP(Partido Popular)" = "blue",
                   "Compromís" = "darkorange",
                   "Ciudadanos" = "lightcoral",
                   "VOX" = "green",
                   "Podemos" = "purple", "No sabe todavía" = "grey", "No contesta" = "black")

colores_partido <- c("PSOE" = "red",
                     "PP(Partido Popular)" = "blue",
                     "Compromís" = "darkorange",
                     "Ciudadanos" = "orange",
                     "Podemos" = "purple",
                     "VOX" = "green",
                     "No sabe todavía" = "white", 
                     "No contesta" = "grey")
                    

# Calcula el porcentaje de respuestas para cada combinación de Importancia e Intencion de voto
preelectoralseccion3 %>%
  group_by(Importancia, `Intencion de voto`) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) -> datos_porcentajes

# Crea el gráfico de barras apiladas con porcentajes
plot4 <- ggplot(data = datos_porcentajes, aes(x = Importancia, y = Percentage, fill = `Intencion de voto`)) +
  geom_bar(stat = "identity", colour = "black") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Importancia", y = "Porcentaje de respuestas", fill = "Intencion de voto") +
  scale_fill_manual(values = colores_partido) 

plot4 <- plot4 + labs(title = "¿A qué le da usted más importancia a la hora de decidir su voto?", x = "Respuesta") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10, hjust = 0.5),  # Aumentar tamaño de texto en el eje y
        axis.title.y = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) + coord_flip()

ggsave("importanciadef.png", plot = plot4, width = 10, height = 6, dpi = 300)


############################################################################################################################################
#5. ¿Cuándo se decide el voto?

is.na(preelectoralvalencia$MVOTO)
unique(preelectoralvalencia$MVOTO)
decisionvoto <- factor(preelectoralvalencia$MVOTO, levels = c(3,5,1,9,2,4,7), labels = c("Durante la última semana de la Campaña Electoral", "El mismo día de las elecciones", "Antes del comienzo de la Campaña Electoral", "No contesta", "Al comienzo de la Campaña Electoral", "Durante la Jornada de Reflexión", "Aun no ha votado en ningunas elecciones"))
decisionvoto<- data.frame(decisionvoto)

porcentaje_decisionvoto <- table(decisionvoto$decisionvoto) / length(decisionvoto$decisionvoto) * 100

# Calcular la distribución de `decisionvoto` en porcentaje
porcentaje_decisionvoto <- table(decisionvoto$decisionvoto) / length(decisionvoto$decisionvoto) * 100

# Crear un nuevo data frame con los porcentajes
datos_porcentaje <- data.frame(decision = names(porcentaje_decisionvoto), porcentaje = porcentaje_decisionvoto)
datos_porcentaje <- datos_porcentaje[-c(4,7),]

# Crear un gráfico de sectores en porcentaje con etiquetas por fuera
grafico_sectores_porcentaje <- ggplot(datos_porcentaje, aes(x = "", y = porcentaje.Freq, fill = decision, label = paste0(round(porcentaje.Freq, 1), "%"))) +
  geom_bar(stat = "identity", width = 0.33, color = "black") +
  geom_text(aes(x = 1.2, label = paste0(round(porcentaje.Freq, 1), "%")), position = position_stack(vjust = 0.5), check_overlap = TRUE, size = 4.5) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Decisiones de Voto en Porcentaje") +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") + labs(title = "¿Cuándo decide usted su voto?", fill = "Decisión") +  theme(axis.text.y = element_text(size = 10, hjust = 0.5),  # Aumentar tamaño de texto en el eje y
                                                                                                                      axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5)) 
                                                                                                                





#########################################################################################################################################
#6. Recuerdo de voto en valencia x identidad de partido


base6 <- data.frame(preelectoralvalencia$FIDEVOTO, preelectoralvalencia$RECUVOTOM)
colnames(base6)[1] <- "FIDEVOTO"
colnames(base6)[2] <- "RECUVOTOM"
unique(base6$FIDEVOTO)
unique(base6$RECUVOTOM)
base6$FIDEVOTO <- factor(base6$FIDEVOTO, levels = c(3,1,2,9,4,5,6,8), labels = c("Según lo que más le convenza en ese momento", "Vota siempre por el mismo partido", "Por lo general suele votar por el mismo partido", "No contesta", "Votan en blanco o nulo", "No suelen votar", "Es la primera vez que votan", "No sabe"))
base6$RECUVOTOM<- factor(base6$RECUVOTOM, levels = c(1001, 2, 8, 5,1,9,9999,3,8995,9998,8996,6,903,1301), labels = c("Compromís", "PP", "Podemos", "Ciudadanos", "PSOE", "Izquierda Unida", "No contesta", "VOX", "Otro partido", "No recuerda", "En blanco", "PACMA", "En Comun Podem", "Más Madrid"))
respuestas_permitidas <- c("PSOE", "PP", "VOX", "Podemos", "Compromís", "Ciudadanos", "En blanco")
base6 <- subset(base6, base6$RECUVOTOM %in% respuestas_permitidas)
base6 <- na.omit(base6)  
respuestas_permitidas2 <- c("Votan en blanco o nulo", "No suelen votar", "Es la primera vez que votan")
base6 <- subset(base6, !(base6$FIDEVOTO %in% respuestas_permitidas2))

colores_partido <- c("PSOE" = "red",
                     "PP" = "blue",
                     "Compromís" = "darkorange",
                     "Ciudadanos" = "lightcoral",
                     "VOX" = "green",
                     "Podemos" = "purple", "No recuerda" = "grey","En blanco" = "white")


str(base6)
table(base6$FIDEVOTO, base6$RECUVOTOM)

# Calcular las frecuencias y porcentajes de RECUVTOM dentro de cada categoría de FIDEVOTO
base6 <- base6 %>%
  group_by(FIDEVOTO, RECUVOTOM) %>%
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

grafico <- ggplot(base6, aes(x = FIDEVOTO, y = percentage, fill = RECUVOTOM, label = paste0(round(percentage, 1), "%"))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Porcentaje de RECUVTOM por categoría de FIDEVOTO", x = "FIDEVOTO", y = "Porcentaje") +
  scale_fill_manual(values = colores_partido) +
  theme_minimal() 

grafico <- grafico + theme(legend.position = "bottom") + coord_flip() + labs(title = "Fidelidad de partido según recuerdo de voto", x = "Fidelidad de partido", y = "Porcentaje", fill = "Recuerdo de voto") + theme(plot.title = element_text(hjust = 0.5)) 

# Imprimir el gráfico
print(grafico)

#########################################################################################################################################
#7. Probabilidad de ir a votar x intención de voto 


base7 <- data.frame(preelectoralvalencia$PROBVOTOM, preelectoralvalencia$INTENCIONM)
base7 <- na.omit(base7)
base7$preelectoralvalencia.INTENCIONM <- factor(base7$preelectoralvalencia.INTENCIONM, levels = c(9998,2,8,1,1001,5,3,9999,1202,9,8995,4,9977,9997,6), labels = c("No sabe todavía", "PP(Partido Popular)", "Podemos", "PSOE", "Compromís", "Ciudadanos", "VOX", "No Contesta", "En Común + Unidas Podemos", "Izquierda Unida", "Otro Partido", "Podemos", "Voto Nulo", "PACMA", ""))
colnames(base7)[1] <- "PROBVOTOM"
colnames(base7)[2] <- "INTENCIONM"
respuestas_permitidas <- c("PSOE", "PP(Partido Popular)", "VOX", "Podemos", "Compromís", "Ciudadanos", "No sabe todavía", "No contesta")
base7 <- subset(base7, INTENCIONM %in% respuestas_permitidas)
respuestas_permitidas2 <- c(0,1,2,3,4,5,6,7,8,9,10)
base7 <- subset(base7, PROBVOTOM %in% respuestas_permitidas2)

colores_partido <- c("PSOE" = "red",
                     "PP(Partido Popular)" = "blue",
                     "Compromís" = "darkorange",
                     "Ciudadanos" = "lightcoral",
                     "VOX" = "green",
                     "Podemos" = "purple", "No sabe todavía" = "grey", "No contesta" = "black")

mediaprobvotar <- aggregate(PROBVOTOM ~ INTENCIONM, data = base7, FUN = mean)


dot_plot_candidato <- ggplot(mediaprobvotar, aes(x = PROBVOTOM, y = INTENCIONM, color = INTENCIONM)) +
  geom_point(size = 3, shape = 16, stroke = 1.5) +  # Aumentar tamaño y personalizar forma de los puntos
  geom_text(aes(label = sprintf("%.2f", PROBVOTOM)), vjust = -0.5, size = 5) +  # Usar PROBVOTOM
  labs(title = "Movilización según intención de voto", subtitle = "Media", y = "En una escala del 0 al 10, ¿Cómo de probable es que usted vaya a votar?") +
  scale_color_manual(values = colores_partido) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10, hjust = 0.5),  # Aumentar tamaño de texto en el eje y
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +  # Centrar el título
  coord_flip()

# Imprimir el gráfico
print(dot_plot_candidato)

####################################################
##Barómetro Municipal Valencia Diciembre de 2022

rm(list = ls())
dir()
library(readxl)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
datos <- read_excel("MicrodatosDic2022.xlsx")


#P.4.1/P.4.2/P.4.3, esto es: pregunta por la gestión del ayuntamiento, del gobierno autonómico y del gobierno nacionaL. 
#Comparar.
#grafiquito de tres campanas de gauss para etsas tres variables

misdatos <- datos[!(datos$p4_1 %in% c(98, 99)) & !(datos$p4_2 %in% c(98, 99)) & !(datos$p4_3 %in% c(98, 99)), ]
misdatos <- data.frame("Valencia" = misdatos$p4_1, "Comunitat valenciana" = misdatos$p4_2, "Gobierno Nacional" = misdatos$p4_3)
unique(misdatos$Valencia); unique(misdatos$Comunitat.valenciana); unique(misdatos$Gobierno.Nacional)

#meltear la base para el gráfico
misdatos <- melt(misdatos, id.vars = NULL, variable.name = "Tipo de Gobierno", value.name = "Puntuacion") #id vars son las columnas que no pretendo derretir 
levels(misdatos$`Tipo de Gobierno`) <- c("Ayuntamiento de Valencia", "Generalitat Valenciana", "Gobierno Nacional")
str(misdatos)

?geom_boxplot

str(misdatos$`Tipo de Gobierno`)

mi_paleta <- c("#F4D166", "#EC6E1C", "#B71D3E")

plot <- ggplot(misdatos, aes(x = `Tipo de Gobierno`, y = Puntuacion, color = `Tipo de Gobierno`)) + geom_violin(trim = FALSE) + geom_boxplot(width = 0.10) + scale_y_continuous(breaks = c(0,2,4,6,8,10)) + scale_color_manual(values = mi_paleta, labels = c("Media = 5.56", "Media = 5.63", "Media = 4.50"))


# Calcular la media de la puntuación por cada categoría de gobierno
medias_por_tipo_gobierno <- aggregate(Puntuacion ~ `Tipo de Gobierno`, data = misdatos, FUN = mean)

# Agregar las medias al gráfico

plot <- plot +  labs(title = "Valoración de la gestión de los diferentes niveles de Gobierno", subtitle = 

                       plot <- ggplot(porcentajes, aes(x = Puntuacion, y = porcentaje, fill = `Tipo de Gobierno`)) +
                       geom_bar(stat = "identity", color = "black") +
                       geom_text(aes(label = percent(porcentaje)), vjust = -0.5, size = 3) +
                       scale_fill_manual(values = mi_paleta) +
                       facet_wrap(~`Tipo de Gobierno`, scales = "free", strip.position = "bottom") +  # Facetas con el eje x en la parte inferior
                       theme(strip.placement = "outside",  # Mover las etiquetas de las facetas fuera
                             strip.background = element_blank(),  # Eliminar fondo de facetas
                             panel.spacing = unit(0, "lines")))
plot <- plot +  theme(axis.text.y = element_text(size = 10, hjust = 0.5),  # Aumentar tamaño de texto en el eje y
                      axis.title.y = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"),
                      legend.position = "bottom",
                      plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, size = 4)) +  theme_bw()

print(plot)


#P.7 ¿Cree usted que la siguiente lista de servicios públicos ha mejorado o empeorado?

misnuevosdatos <- datos[!(datos$P7_1_01 %in% c(98, 99,97)) & !(datos$P7_1_02 %in% c(98, 99,97)) & !(datos$P7_1_03 %in% c(98, 99, 97)) & !(datos$P7_1_04 %in% c(98, 99, 97)) & !(datos$P7_1_05 %in% c(98, 99, 97)) & !(datos$P7_1_06 %in% c(98, 99, 97)) & !(datos$P7_1_07 %in% c(98, 99, 97)) & !(datos$P7_1_08 %in% c(98, 99, 97)) & !(datos$P7_1_09 %in% c(98, 99, 97)) & !(datos$P7_1_10 %in% c(98, 99, 97)) & !(datos$P7_1_11 %in% c(98, 99, 97)) & !(datos$P7_1_12 %in% c(98, 99, 97)) & !(datos$P7_1_13 %in% c(98, 99, 97)) & !(datos$P7_1_14 %in% c(98, 99, 97)), ]
misnuevosdatos <- data.frame("Recogida de Basura" = misnuevosdatos$P7_1_01, "Limpieza de las calles" = misnuevosdatos$P7_1_01, "Control de ruido" = misnuevosdatos$P7_1_03, "Zonas verdes" = misnuevosdatos$P7_1_04, "Alumbrado Público" = misnuevosdatos$P7_1_05, "Circulación" = misnuevosdatos$P7_1_06, "Parque Natural de L 'Albufera" = misnuevosdatos$P7_1_07, "Policía Municipal y Seguridad ciudadana" = misnuevosdatos$P7_1_08, "Aparcamientos Públicos" = misnuevosdatos$P7_1_09, "Autobús(EMT)" = misnuevosdatos$P7_1_10, "Carril Bici" = misnuevosdatos$P7_1_11, "Instalaciones deportivas" = misnuevosdatos$P7_1_12, "Bibliotecas y museos municipales" = misnuevosdatos$P7_1_13, "Mercados municipales" = misnuevosdatos$P7_1_14)
misnuevosdatos <- melt(misnuevosdatos, id.vars = NULL, variable.name = "Tipo de Gobierno", value.name = "Puntuacion") #id vars son las columnas que no pretendo derretir 
levels(misnuevosdatos$`Tipo de Gobierno`) <- c("Recogida de basura", "Limpieza de calles", "Control de ruido", "Zonas Verdes", "Alumbrado Público", "Circulación", "Parque Natural de L 'Albufera", "Policía Municipal y Seguridad ciudadana","Aparcamientos Públicos","Autobús(EMT)", "Carril Bici", "Instalaciones deportivas", "Bibliotecas y museos municipales", "Mercados municipales")
mi_paleta <- c("#F4D166", "#EC6E1C", "#B71D3E", "#0072B2", "#029F73", "#D45E00", "#08518B", "#A2C8EC", "#4E92B7", "#661D1D", "#AD9E00", "#686868", "#C0C0C0", "#5B5B5B")


porcentajes <- misnuevosdatos %>%
  group_by(`Tipo de Gobierno`, Puntuacion) %>%
  summarise(count = n()) %>%
  mutate(porcentaje = count / sum(count))


plot <- ggplot(porcentajes, aes(x = Puntuacion, y = porcentaje, fill = `Tipo de Gobierno`)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = mi_paleta, name = "Servicio Público") +
  facet_wrap(~`Tipo de Gobierno`, scales = "free", strip.position = "bottom") +  # Facetas con el eje x en la parte inferior
  theme(legend.position = "none", strip.placement = "outside",  # Mover las etiquetas de las facetas fuera
        strip.background = element_blank(),  # Eliminar fondo de facetas
        panel.spacing = unit(0, "lines")) 

plot <- plot + labs(title = "Valoración de la gestión de los servicios públicos de Valencia en 2022", subtitle = "En una escala donde el 0 representa 'Muy mal' y el 10 representa 'Muy bien'") + theme_bw()

# Imprimir el gráfico
print(plot)

#P.15/ P.16/ P.17 esto es: como calificaría la situación política actual de 1.España/2.Comunitat Valenciana/ 3. Valencia distrito 

misdatos <- datos[!(datos$p15 %in% c(98, 99)) & !(datos$p16 %in% c(98, 99)) & !(datos$p17 %in% c(98, 99)), ]
misdatos <- data.frame("Valencia" = misdatos$p15, "Comunitat valenciana" = misdatos$p16, "Gobierno Nacional" = misdatos$p17)
unique(misdatos$Valencia); unique(misdatos$Comunitat.valenciana); unique(misdatos$Gobierno.Nacional)

#meltear la base para el gráfico
misdatos <- melt(misdatos, id.vars = NULL, variable.name = "Tipo de Gobierno", value.name = "Valoración de la situación política") #id vars son las columnas que no pretendo derretir 
unique(misdatos$`Valoración de la situación política`)
misdatos$`Valoración de la situación política` <- factor(misdatos$`Valoración de la situación política`, levels = c(1,2,3,4,5), labels = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"))

mi_paleta <- c("#F4D166", "#EC6E1C", "#B71D3E")

misdatos$`Tipo de Gobierno`
plot <- ggplot(misdatos, aes(x = `Valoración de la situación política`, fill = `Tipo de Gobierno`)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(aes(label = scales::percent(..count.. / sum(..count..))),
            stat = "count", position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = mi_paleta, labels = c("Valencia", "Generalitat Valenciana", "Gobierno Nacional")) +
  labs(title = "Valoración de la Situación Política por Tipo de Gobierno",
       x = "Valoración de la Situación Política", y = "Conteo") +
  theme_minimal() + ylab("Total de respuestas")


# Imprimir el gráfico
print(plot)

#P.22 lista de temas y la correspondiente importancia que  han de tener para el país. 
#- Sugerencia: mezclar con P.39

misnuevosdatoss <- datos[!(datos$P22_01 %in% c(98, 99)) & !(datos$P22_02 %in% c(98, 99)) & !(datos$P22_03 %in% c(98, 99)) & !(datos$P22_04 %in% c(98, 99)) & !(datos$P22_05 %in% c(98, 99)) & !(datos$P22_06 %in% c(98, 99)) & !(datos$P22_07 %in% c(98, 99)) & !(datos$P22_08 %in% c(98, 99)) & !(datos$P22_09 %in% c(98, 99)) & !(datos$P22_10 %in% c(98, 99)), ]
misnuevosdatoss <- data.frame("Garantizar la seguridad pública" = misnuevosdatoss$P22_01, "Crecimiento económico y comercio" = misnuevosdatoss$P22_02, "Mejora del sistema educativo" = misnuevosdatoss$P22_03, "Desarrollo, investigación e innovación" = misnuevosdatoss$P22_04, "Proteger los derechos y las libertades de la ciudadanía" = misnuevosdatoss$P22_05, "Asegurar la asistencia sanitaria universal" = misnuevosdatoss$P22_06, "Promoción de la lengua y la cultura valenciana" = misnuevosdatoss$P22_07, "Reducir la desigualdad económica" = misnuevosdatoss$P22_08, "Protección del Medio Ambiente" = misnuevosdatoss$P22_09, "Mejorar las relaciones internacionales y asegurar la paz" = misnuevosdatoss$P22_10)

# Calcular la media de cada columna
medias <- colMeans(misnuevosdatoss)

# Crear un nuevo data frame con una sola observación (una fila) que contenga las medias
medias_df <- data.frame(t(medias))

# Puedes cambiar los nombres de las columnas si lo deseas
colnames(medias_df) <- c("Garantizar la seguridad pública", "Crecimiento económico y comercio", "Mejora del sistema educativo", "Desarrollo, investigación e innovación",
                         "Proteger los derechos y las libertades de la ciudadanía", "Asegurar la asistencia sanitaria universal",
                         "Promoción de la lengua y la cultura valenciana", "Reducir la desigualdad económica", "Protección del Medio Ambiente",
                         "Mejorar las relaciones internacionales y asegurar la paz")

# El nuevo data frame medias_df contendrá las medias de cada columna
print(medias_df)

medias_df <- melt(medias_df, variable.name = "Aspectos", value.name = "Media")
str(medias_df)
medias_df[2] <- round(medias_df[2], 2)

dot_plot <- ggplot(medias_df, aes(x = Media, y = Aspectos, color = Aspectos)) +
  geom_point(size = 3) + 
  geom_text(aes(label = round(Media, 2)), vjust = -0.5, size = 3) +  # Agregar los valores de las medias
  scale_color_manual(values = mi_paleta) +
  labs(title = "'Puntúe la importancia que cree que los siguientes temas deberían tener para el país'", subtitle = "Escala de 0 al 10", x = "Media") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5, size = 8),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"),
        legend.position = "none")

print(dot_plot)



#P-39

misnuevosdatoss <- datos[!(datos$p39_01 %in% c(98, 99,9)) & !(datos$p39_02 %in% c(98, 99,9)) & !(datos$p39_03 %in% c(98, 99,9)) & !(datos$p39_04 %in% c(98, 99,9)) & !(datos$p39_05 %in% c(98, 99,9)) & !(datos$p39_06 %in% c(98, 99,9)) & !(datos$p39_07 %in% c(98, 99,9)) & !(datos$p39_08 %in% c(98, 99,9)) & !(datos$p39_09 %in% c(98, 99,9)) & !(datos$p39_10 %in% c(98, 99,9)), ]
misnuevosdatoss <- data.frame("Garantizar la seguridad pública" = misnuevosdatoss$p39_01, "Crecimiento económico y comercio" = misnuevosdatoss$p39_02, "Mejora del sistema educativo" = misnuevosdatoss$p39_03, "Desarrollo, investigación e innovación" = misnuevosdatoss$p39_04, "Proteger los derechos y las libertades de la ciudadanía" = misnuevosdatoss$p39_05, "Asegurar la asistencia sanitaria universal" = misnuevosdatoss$p39_06, "Promoción de la lengua y la cultura valenciana" = misnuevosdatoss$p39_07, "Reducir la desigualdad económica" = misnuevosdatoss$p39_08, "Protección del Medio Ambiente" = misnuevosdatoss$p39_09, "Mejorar las relaciones internacionales y asegurar la paz" = misnuevosdatoss$p39_10)

misnuevosdatoss$Garantizar.la.seguridad.pública <- factor(misnuevosdatoss$Garantizar.la.seguridad.pública, levels = c(1,2,3,4,5,7), labels = c("Ciudadanos", "Compromís", "Podemos", "PP", "PSOE", "VOX"))
colnames(misnuevosdatoss)[1] <- "Garantizar la seguridad pública"                                                                                                                                                 
misnuevosdatoss$Crecimiento.económico.y.comercio <- factor(misnuevosdatoss$Crecimiento.económico.y.comercio, levels = c(1,2,3,4,5,7), labels = c("Ciudadanos", "Compromís", "Podemos", "PP", "PSOE", "VOX"))
colnames(misnuevosdatoss)[2] <- "Crecimiento económico y comercio"
misnuevosdatoss$Mejora.del.sistema.educativo <- factor(misnuevosdatoss$Mejora.del.sistema.educativo, levels = c(1,2,3,4,5,7), labels = c("Ciudadanos", "Compromís", "Podemos", "PP", "PSOE", "VOX"))
colnames(misnuevosdatoss)[3] <- "Mejora del sistema educativo"

misnuevosdatoss$`Desarrollo, investigación e innovación`<- factor(misnuevosdatoss$`Desarrollo, investigación e innovación`, levels = c(1,2,3,4,5,7), labels = c("Ciudadanos", "Compromís", "Podemos", "PP", "PSOE", "VOX"))
colnames(misnuevosdatoss)[4] <- "Desarrollo, investigación e innovación"

misnuevosdatoss$`Proteger los derechos y libertades de la ciudadanía` <- factor(misnuevosdatoss$`Proteger los derechos y libertades de la ciudadanía`, levels = c(1,2,3,4,5,7), labels = c("Ciudadanos", "Compromís", "Podemos", "PP", "PSOE", "VOX"))
colnames(misnuevosdatoss)[5] <- "Proteger los derechos y libertades de la ciudadanía"
misnuevosdatoss$`Asegurar la asistencia sanitaria universal`<- factor(misnuevosdatoss$`Asegurar la asistencia sanitaria universal`, levels = c(1,2,3,4,5,7), labels = c("Ciudadanos", "Compromís", "Podemos", "PP", "PSOE", "VOX"))
colnames(misnuevosdatoss)[6] <- "Asegurar la asistencia sanitaria universal"
misnuevosdatoss$`Promoción de la lengua y la cultura valenciana` <- factor(misnuevosdatoss$`Promoción de la lengua y la cultura valenciana`, levels = c(1,2,3,4,5,7), labels = c("Ciudadanos", "Compromís", "Podemos", "PP", "PSOE", "VOX"))
colnames(misnuevosdatoss)[7] <- "Promoción de la lengua y la cultura valenciana"
misnuevosdatoss$`Reducir la desigualdad económica` <- factor(misnuevosdatoss$`Reducir la desigualdad económica`, levels = c(1,2,3,4,5,7), labels = c("Ciudadanos", "Compromís", "Podemos", "PP", "PSOE", "VOX"))
colnames(misnuevosdatoss)[8] <- "Reducir la desigualdad económica"
misnuevosdatoss$`Protección del Medio Ambiente` <- factor(misnuevosdatoss$`Protección del Medio Ambiente`, levels = c(1,2,3,4,5,7), labels = c("Ciudadanos", "Compromís", "Podemos", "PP", "PSOE", "VOX"))
colnames(misnuevosdatoss)[9] <- "Protección del Medio Ambiente"
misnuevosdatoss$Mejorar.las.relaciones.internacionales.y.asegurar.la.paz <- factor(misnuevosdatoss$Mejorar.las.relaciones.internacionales.y.asegurar.la.paz, levels = c(1,2,3,4,5,7), labels = c("Ciudadanos", "Compromís", "Podemos", "PP", "PSOE", "VOX"))
colnames(misnuevosdatoss)[10] <- "Mejorar las relaciones internacionales y asegurar la paz" 

#melteamos

misnuevosdatoss_long <- melt(misnuevosdatoss, id.vars = "Aspecto")


###############################
#principales problemas de VALENCIA por barrios

library(readxl)
datosy <- read_excel("principales problemas por barrio .xlsx")
distrito <- datosy[1]
str(datosy)
datosy <- round(datosy[-1], 2)
datosy <- data.frame(distrito, datosy)
colnames(datosy)[1] <- "Distrito"
colnames(datosy)[-1] <- c("Tránsito y circulación", "Limpieza", "Ocupación", "Urbanismo", "Seguridad", "Política y políticas públicas", "Macroeconomía", "Convivencia", "Equipamiento/Servicios", "Salud y atención sanitaria", "Zonas Verdes", "Transporte público", "Medio Ambiente", "Vivienda", "Economía social", "Gestión", "Educación", "Corrupción", "Cultura y ocio", "Cap")
datosy <- datosy[-21]

library(reshape2)
datosy <- melt(datosy, id.vars = "Distrito", variable.name = "Problema", value.name = "Porcentaje")
is.na(datosy$Porcentaje)
str(datosy$Distrito)
datosy$Distrito <- as.factor(datosy$Distrito)
str(datosy$Distrito)

# Crea un vector con los niveles que deseas mantener
niveles_deseados <- c(
  "Tránsito y circulación",
  "Limpieza",
  "Ocupación",
  "Urbanismo",
  "Seguridad",
  "Política y políticas públicas",
  "Macroeconomía",
  "Convivencia",
  "Equipamiento/Servicios",
  "Salud y atención sanitaria",
  "Zonas Verdes",
  "Transporte público",
  "Medio Ambiente",
  "Vivienda"
)

# Filtra la base de datos para mantener solo los niveles deseados
datos_filtrados <- datosy %>% filter(Problema %in% niveles_deseados)
datos_filtrados <- datos_filtrados %>% filter(Porcentaje >= 5)


# Crear el mapa de calor
heatmap_plot <- ggplot(datos_filtrados, aes(x = Problema, y = Distrito, fill = Porcentaje)) +
  geom_tile() +
  scale_fill_gradient(low = "gold1", high = "red") +  # Colores cálidos
  labs(title = "Porcentaje (%) de ciudadanos que contestaron que X es el principal problema de Valencia por distritos", x = "Problema", y = "Distrito", fill = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "right", panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje)), size = 3, color = "black")

heatmap_plot + scale_y_discrete(labels = rev(c("Jesús", "Patraix", "l'Olivereta", "el Pla del Real", "la Saïdia", "Campanar", "Extramurs", "l'Eixample", "Pobles del Sud", "Pobles de l'Oest", "Pobles del Nord", "Benicalap", "Rascanya", "Benimaclet", "Algirós", "Camins al Grau", "Poblats Marítims", "Quatre Carreres", "Ciutat Vella")))

# Imprime el mapa de calor
print(heatmap_plot)

