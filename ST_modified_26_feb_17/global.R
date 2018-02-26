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

source("functions.R")

datf <- data.frame(longitude = 115.2166672, latitude = -8.6499996, exist= TRUE)

g <- read.csv("data/blocks_den.csv")
a <- read.csv("data/hospitals_den.csv") 
newsattractors <- read.csv("data/newsattractors.csv")  
hosptable <- read.csv("data/hospitals_den_table.csv") # para mostrar la tabla

# Calculate distance
values <- distance_table(generators = g,
                         attractors = a)
newsvalues <- distance_new_table(generators =g,
                              attractorsn = newsattractors) 

iconify <- function(list.of.logical){
  list.of.urls <- gsub(pattern="TRUE", replacement="hosp_true.png", x=as.character(list.of.logical))
  list.of.urls <- gsub(pattern="FALSE", replacement="hospital_false.png", x=list.of.urls)
  cute.icons <- list(iconUrl = list.of.urls, iconSize = c(26, 27))
  return(cute.icons)
}

write.csv(x=values,file="data/distanciasexistentes.csv")