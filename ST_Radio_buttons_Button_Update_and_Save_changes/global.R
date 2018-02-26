#library(dplyr)
#library (sp)
library(stats)
library(Matrix)
library(base)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinythemes)
library(scales)
library(curl)
library(lattice)
#library(dplyr)
#library(rsconnect)
library (sp)
library (htmltools)

datf <- data.frame(longitude = 115.2166672, latitude = -8.6499996, exist= TRUE)

g <- read.csv("data/blocks_den.csv")
a <- read.csv("data/hospitals_den.csv") # para calcular distancias
anew <- read.csv("data/vernewatractores.csv")  #cambiar por read.table o vernewatractores.csv
destiny <- read.csv("data/hospitals_denpasar_con_nombres.csv") # para ver marcadores con puntos y nombres de hospitales
hosptable <- read.csv("data/hospitals_den_table.csv") # para mostrar la tabla
destiny$latitude <- jitter(destiny$latitude)
destiny$longitude <- jitter(destiny$longitude)
destiny$code <-as.numeric(destiny$code)

row.names(destiny) <- destiny$code

# Calculate distance
values <- distance_table(generators = g,
                         attractors = a)
valuesnew <- distance_table(generators =g,
                              attractorsn = anew) 
write.csv(x=valuesnew, file="distanciasnews.csv")
iconify <- function(list.of.logical){
  list.of.urls <- gsub(pattern="TRUE", replacement="hosp_true.png", x=as.character(list.of.logical))
  list.of.urls <- gsub(pattern="FALSE", replacement="hospital_false.png", x=list.of.urls)
  cute.icons <- list(iconUrl = list.of.urls, iconSize = c(26, 27))
  return(cute.icons)
}
#write.csv(x=values,file="resultados_de_distancias.csv")
source("functions.R")