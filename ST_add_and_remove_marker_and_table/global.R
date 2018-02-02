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

datf <- data.frame(longitude = 115.2166672, latitude = -8.6499996)

g <- read.csv("data/blocks_den.csv")
a <- read.csv("data/hospitals_den.csv")
allzips <- read.csv("data/hospitals_denpasar_con_nombres.csv")
#datefra <- data.frame (latitude=Latitude,longitude=Longitude)
#allzips <- allzips[complete.cases(allzips),]
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$code <-as.numeric(allzips$code)

row.names(allzips) <- allzips$code

# Calculate distance
values <- distance_table(generators = g,
                         attractors = a)


source("functions.R")