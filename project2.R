#setwd("C:/Users/joony/Documents/STAT")

# load libraries
library(tidyverse)
library(faraway) 
library(multcomp)
library(lawstat)
library(MASS)

# read in data and exclude the first column (row number)
data_W<-read.table("wineQualityWhites.csv", header=TRUE, sep=",")[,-1]
data_R<-read.table("wineQualityReds.csv", header=TRUE, sep=",")[,-1]

# check if there is any empty element
table(is.na(data_W))
table(is.na(data_R))

# add which wine column
wine <- rownames(data_W)
wine[1:length(wine)] <- "w"

data_W <- as.data.frame(cbind(data_W, wine))

wine <- rownames(data_R)
wine[1:length(wine)] <- "r"

data_R <- as.data.frame(cbind(data_R, wine))

# make one data frame with all
data_t <- as.data.frame(rbind(data_W, data_R))

# save the total data
write.table(data_t, "data_t.txt", col.names=T, row.names=F, sep="\t", quote=F)

rm(wine)
################################
##################################
##################################
# read in the data  
data <- read.table("data_t.txt", header=T, sep="\t")

# check the data
head(data)

# check the class of the variables
for (i in colnames(data)) {
  print(i)
  print(class(data[[i]]))
  print("--------------")
}

# turn wine variable into factor
data$wine <- as.factor(data$wine)

# attach the data
attach(data)

# levene test and boxplots 
levene.test(fixed.acidity,wine)
boxplot(fixed.acidity~wine,data=data, main="fixed.acidity vs wine", xlab="wine", ylab="fixed.acidity")

levene.test(volatile.acidity,wine)
boxplot(volatile.acidity~wine,data=data, main="volatile.acidity vs wine", xlab="wine", ylab="volatile.acidity")

levene.test(citric.acid,wine)
boxplot(citric.acid~wine,data=data, main="citric.acid vs wine", xlab="wine", ylab="citric.acid")

levene.test(residual.sugar,wine)
boxplot(residual.sugar~wine,data=data, main="residual.sugar vs wine", xlab="wine", ylab="residual.sugar")

levene.test(chlorides,wine)
boxplot(chlorides~wine,data=data, main="chlorides vs wine", xlab="wine", ylab="chlorides")

levene.test(free.sulfur.dioxide,wine)
boxplot(free.sulfur.dioxide~wine,data=data, main="free.sulfur.dioxide vs wine", xlab="wine", ylab="free.sulfur.dioxide")

levene.test(total.sulfur.dioxide,wine)
boxplot(total.sulfur.dioxide~wine,data=data, main="total.sulfur.dioxide vs wine", xlab="wine", ylab="total.sulfur.dioxide")

levene.test(density,wine)
boxplot(density~wine,data=data, main="density vs wine", xlab="wine", ylab="density")

levene.test(pH,wine)
boxplot(pH~wine,data=data, main="pH vs wine", xlab="wine", ylab="pH")

levene.test(sulphates,wine)
boxplot(sulphates~wine,data=data, main="sulphates vs wine", xlab="wine", ylab="sulphates")

