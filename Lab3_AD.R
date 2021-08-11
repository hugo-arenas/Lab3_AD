#Laboratorio 3 - Análisis de Datos
#Integrantes:
#            -Hugo Arenas
#            -Juan Arredondo


library(cluster)
library(fpc)
library(NbClust)
library(factoextra)
library(FactoMineR)
library(Rtsne) 

library(ez)
library(dplyr)
library(ggpubr)
library(knitr)
library(tidyr)
library(car)
library(lmtest)
library(vcd)
library(arulesViz)
library("cowplot")

#Los nombres originales de las variables, una breve explicación y los tipos de los datos:

#age (rango de edad): 10-19, 20-29, 30-39, 40-49,.
#menopause (momento de la menopausia): lt40, ge40, premeno.
#tumor-size (tamaño del tumor extirpado en mm): 0-4, 5-9, 10-14, .
#inv-nodes (una métrica de presencia de células cancerosas en los nodos linfáticos): 0-2, 3-5, 6-8, 9-11,.
#node-caps (evidencia de que células cancerosas atravesaron la cápsula de los nódulos linfáticos): yes, no
#deg-malig (grado histológico del tumor: bajo, intermedio, alto): 1, 2, 3.
#breast (mama afectada): left, right.
#breast-quad (cuadrante de la mama): left-up, left-low, right-up, right-low, central.
#irradiat (radioterapia): yes, no.
#Class (clase) Indica recurrencia, es la variable a predecir (no-recurrencia: 201 casos, recurrencia: 85 casos)


# Leemos los datos
dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)
filename <- "breast-cancer.data"
file <- file.path(dirstudio, filename)

#Se definen los nombres de las columnas, estos son los mismos de provistos por la base de datos
columns <- c("class", 
             "age", 
             "menopause", 
             "tumor.size", 
             "inv.nodes", 
             "node.caps",
             "deg.malig",
             "breast",
             "breast.quad",
             "irradiat")

tabla <- read.csv(file, col.names = columns)

#Se sacan los datos nulos del datagrama
bool.values <- tabla$node.caps=='?'
tabla <- tabla[!bool.values,]

bool.values <- tabla$breast.quad =='?'
tabla <- tabla[!bool.values,]

menopause <- as.factor(tabla$menopause)
node.caps <- as.factor(tabla$node.caps)
deg.malig <- as.factor(tabla$deg.malig)
breast <- as.factor(tabla$breast)
irradiat <- as.factor(tabla$irradiat)


tabla$class <- as.factor(tabla$class)
tabla$age <- unclass(as.factor(tabla$age)) 
tabla$menopause <- unclass(as.factor(tabla$menopause)) 
tabla$tumor.size <- unclass(as.factor(tabla$tumor.size))
tabla$inv.nodes <- unclass(as.factor(tabla$inv.nodes)) 
tabla$node.caps <- unclass(as.factor(tabla$node.caps))
tabla$deg.malig <- unclass(as.factor(tabla$deg.malig))
tabla$breast <- unclass(as.factor(tabla$breast)) 
tabla$breast.quad <- unclass(as.factor(tabla$breast.quad)) 
tabla$irradiat <- unclass(as.factor(tabla$irradiat))

tabla$breast.quad[tabla$breast.quad == 4] <- 6
tabla$breast.quad[tabla$breast.quad == 3] <- 4
tabla$breast.quad[tabla$breast.quad == 6] <- 3

summary(tabla)

#Se realizan gráficos de cajas de cada variable con respecto a las clases.
#Se realiza el gráfico de cajas por clase respecto a la variables "age".
boxplot.age = ggboxplot(data = tabla, 
                        x = "class", 
                        y = "age", 
                        color = "class", 
                        add = "jitter") + border() 
ydens = axis_canvas(boxplot.age, axis = "y", coord_flip = TRUE) + geom_density(data = tabla, aes(x = age, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.age = insert_yaxis_grob(boxplot.age, ydens, grid::unit(.2, "null"), position = "right")
print(ggdraw(boxplot.age))

#Se realiza el gráfico de cajas por clase respecto a la variables "menopause".
boxplot.menopause = ggboxplot(data = tabla, 
                              x = "class", 
                              y = "menopause", 
                              color = "class", 
                              add = "jitter") + border() 
ydens = axis_canvas(boxplot.menopause, axis = "y", coord_flip = TRUE) + geom_density(data = tabla, aes(x = menopause, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.menopause = insert_yaxis_grob(boxplot.menopause, ydens, grid::unit(.2, "null"), position = "right")
print(ggdraw(boxplot.menopause))

#Se realiza el gráfico de cajas por clase respecto a la variables "tumor.size".
boxplot.tumor.size = ggboxplot(data = tabla, 
                               x = "class", 
                               y = "tumor.size", 
                               color = "class", 
                               add = "jitter") + border() 
ydens = axis_canvas(boxplot.tumor.size, axis = "y", coord_flip = TRUE) + geom_density(data = tabla, aes(x = tumor.size, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.tumor.size = insert_yaxis_grob(boxplot.tumor.size, ydens, grid::unit(.2, "null"), position = "right")
print(ggdraw(boxplot.tumor.size))

#Se realiza el gráfico de cajas por clase respecto a la variables "inv.nodes".
boxplot.inv.nodes =  ggboxplot(data = tabla, 
                               x = "class", 
                               y = "inv.nodes", 
                               color = "class", 
                               add = "jitter") + border() 
ydens = axis_canvas(boxplot.inv.nodes, axis = "y", coord_flip = TRUE) + geom_density(data = tabla, aes(x = inv.nodes, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.inv.nodes = insert_yaxis_grob(boxplot.inv.nodes, ydens, grid::unit(.2, "null"), position = "right")
print(ggdraw(boxplot.inv.nodes))

#Se realiza el gráfico de cajas por clase respecto a la variables "node.caps".
boxplot.node.caps = ggboxplot(data = tabla, 
                              x = "class", 
                              y = "node.caps", 
                              color = "class", 
                              add = "jitter") + border() 
ydens = axis_canvas(boxplot.node.caps, axis = "y", coord_flip = TRUE) + geom_density(data = tabla, aes(x = node.caps, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.node.caps = insert_yaxis_grob(boxplot.node.caps, ydens, grid::unit(.2, "null"), position = "right")
print(ggdraw(boxplot.node.caps))

#Se realiza el gráfico de cajas por clase respecto a la variables "deg.malig".
boxplot.deg.malig =  ggboxplot(data = tabla, 
                               x = "class", 
                               y = "deg.malig", 
                               color = "class", 
                               add = "jitter") + border() 
ydens = axis_canvas(boxplot.deg.malig, axis = "y", coord_flip = TRUE) + geom_density(data = tabla, aes(x = deg.malig, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.deg.malig = insert_yaxis_grob(boxplot.deg.malig, ydens, grid::unit(.2, "null"), position = "right")
print(ggdraw(boxplot.deg.malig))

#Se realiza el gráfico de cajas por clase respecto a la variables "breast".
boxplot.breast = ggboxplot(data = tabla, 
                           x = "class", 
                           y = "breast", 
                           color = "class", 
                           add = "jitter") + border() 
ydens = axis_canvas(boxplot.breast, axis = "y", coord_flip = TRUE) + geom_density(data = tabla, aes(x = breast, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.breast = insert_yaxis_grob(boxplot.breast, ydens, grid::unit(.2, "null"), position = "right")
print(ggdraw(boxplot.breast))

#Se realiza el gráfico de cajas por clase respecto a la variables "breast.quad".
boxplot.breast.quad = ggboxplot(data = tabla, 
                                x = "class", 
                                y = "breast.quad", 
                                color = "class", 
                                add = "jitter") + border() 
ydens = axis_canvas(boxplot.breast.quad, axis = "y", coord_flip = TRUE) + geom_density(data = tabla, aes(x = breast.quad, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.breast.quad = insert_yaxis_grob(boxplot.breast.quad, ydens, grid::unit(.2, "null"), position = "right")
print(ggdraw(boxplot.breast.quad))

#Se realiza el gráfico de cajas por clase respecto a la variables "irradiat".
boxplot.irradiat = ggboxplot(data = tabla, 
                             x = "class", 
                             y = "irradiat", 
                             color = "class", 
                             add = "jitter") + border() 
ydens = axis_canvas(boxplot.irradiat, axis = "y", coord_flip = TRUE) + geom_density(data = tabla, aes(x = irradiat, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.irradiat = insert_yaxis_grob(boxplot.irradiat, ydens, grid::unit(.2, "null"), position = "right")
print(ggdraw(boxplot.irradiat))

#Se crean las reglas para las variables "age", "tumor.size", "inv.nodes" y "breast.quad".
tabla.reglas = tabla
tabla.reglas$menopause <- menopause 
tabla.reglas$node.caps <- node.caps
tabla.reglas$deg.malig <- deg.malig
tabla.reglas$breast <- breast
tabla.reglas$irradiat <- irradiat
age = c(-Inf, 2, 4, Inf)
age.names = c("adulto joven", "adulto", "adulto mayor")

tumor.size = c(-Inf, 4, 7, Inf)
tumor.size.names = c("pequeño", "mediano", "grande")

inv.nodes = c(-Inf, 2, 5, Inf)
inv.nodes.names = c("bajo", "medio", "alto")

breast.quad = c(-Inf, 1, 3, Inf)
breast.quad.names = c("central", "low", "up")

#Se cambian variables por las reglas asociadas.
tabla.reglas$age = cut(tabla.reglas$age, breaks = age, labels = age.names)
tabla.reglas$tumor.size = cut(tabla.reglas$tumor.size, breaks = tumor.size, labels = tumor.size.names)
tabla.reglas$inv.nodes = cut(tabla.reglas$inv.nodes, breaks = inv.nodes, labels = inv.nodes.names)
tabla.reglas$breast.quad = cut(tabla.reglas$breast.quad, breaks = breast.quad, labels = breast.quad.names)


reglas = apriori(
  data = tabla.reglas, 
  parameter=list(support = 0.2, minlen = 2, maxlen = 6, target="rules"),
  appearance=list(rhs = c("class=no-recurrence-events", "class=recurrence-events"))
)

inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))

inspect(sort(x = reglas, decreasing = TRUE, by = "support"))

inspect(sort(x = reglas, decreasing = TRUE, by = "coverage"))

