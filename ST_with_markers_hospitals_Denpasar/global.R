library(dplyr)
library (sp)

allzips <- read.csv("data/hospitals_denpasar_con_nombres.csv")
#allzips <- allzips[complete.cases(allzips),]
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$code <-as.numeric(allzips$code)

row.names(allzips) <- allzips$code
