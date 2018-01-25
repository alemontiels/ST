library(dplyr)
library (sp)

g <- read.csv("data/blocks_den.csv")
a <- read.csv("data/hospitals_den.csv")
allzips <- read.csv("data/hospitals_denpasar_con_nombres.csv")
#allzips <- allzips[complete.cases(allzips),]
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$code <-as.numeric(allzips$code)

row.names(allzips) <- allzips$code
# Calculate distance
values <- distance_table(generators = g,
                         attractors = a)


source("functions.R")