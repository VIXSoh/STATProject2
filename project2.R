#setwd("C:/Users/joony/Documents/STAT")

library(tidyverse)

# read in data and exclude the first column (row number)
data_W<-read.table("wineQualityWhites.csv", header=TRUE, sep=",")[,-1]
data_R<-read.table("wineQualityReds.csv", header=TRUE, sep=",")[,-1]

# check if there is any empty element
table(is.na(data_W))
table(is.na(data_R))

# Add column indicating the wine type
data_W$type <- 'white'
data_R$type <- 'red'

# Combine the two dataframes
data <- rbind(data_W,data_R)
