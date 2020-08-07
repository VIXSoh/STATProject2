#setwd("C:/Users/joony/Documents/STAT")

# load libraries
library(tidyverse)
library(faraway) 
library(multcomp)
library(lawstat)
library(MASS)
library(ROCR)

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
###################done combining data
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

contrasts(data$wine)
# now see if there are outliers in either response or predictors
lmod <- glm(wine~., family = binomial, data =data)
summary(lmod)

n <- dim(data)[1]
p <- 13

# now look at predictor outliers 
lev<-lm.influence(lmod)$hat

plot(lev, main="Leverages", ylim=c(0,0.5))
abline(h=2*p/n, col="red")

# see the outliers
lev[lev > 2*p/n]

# get indices for leverage outliers
ind_L <- which(lev > 2*p/n)

# get COOKs outliers 
COOKS<-cooks.distance(lmod)
ind_COOKS <- which(COOKS>qf(0.5,p,n-p))

# see if the outliers are influential based on cook's distance
table(ind_L %in% ind_COOKS)
# one of the outliers are influential 

# save the filtered data
#none 

################################
############done filtering outliers
##################################

# read in the data  
data <- read.table("data_t.txt", header=T, sep="\t")
data$wine <- as.factor(data$wine)

attach(data)

# split data into train and test
# pick an arbitrary seed for replicability
set.seed(125)
sample<-sample.int(nrow(data), floor(.50*nrow(data)), replace = F)
train<-data[sample, ]
test<-data[-sample, ]

# use step function to see what is the optimal set of predictors for our model
##intercept only model
regnull <- glm(wine~1, family = binomial, data =train)
##model with all predictors
regfull <- glm(wine~., family = binomial, data =train)

# see if all coef are 0 - is model useful
1-pchisq(regnull$deviance-regfull$deviance,12)
# 0 no at least one of them are not

# use forward selection
forward <- step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")

# compare the forward selected model with the full model
lmod_S <- glm( wine ~ total.sulfur.dioxide + volatile.acidity + 
                 sulphates + density + residual.sugar + alcohol + free.sulfur.dioxide + 
                 chlorides + citric.acid + quality
               , family = binomial, data = train)
# removed fixed.acidity and pH


# compare with a model without non significant predictors ( - sulphates and - citric.acid)
# see if the reduced model is better
1-pchisq(lmod_S$deviance-regfull$deviance,2)
# p value = 0.9201967
# accept null : coef for sulphates and citric.acid are 0. 
# use the reduced model

# check ROC curve
preds<-predict(lmod_S,newdata=test, type="response")
rates<-prediction(preds, test$wine)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result)
lines(x = c(0,1), y = c(0,1), col="red")
# very close to (0,1)
# very good prediction

# check auc
auc<-performance(rates, measure = "auc")
auc@y.values[[1]]
# 0.9983453 very close to 1

# see confusion table
table(test$wine, preds>0.5)
# very nice

# find type 1 error ,type 2 error, sensitivity, and specificity 
# FP / (TN + FP)
t1e =  9 /(837+9) # 0.0106383
#  FN / (FN + TP)
t2e = 5 /(5+2398) # 0.002080732
# TP / (FN + TP)
sen = 2398 /(5+2398) # 0.9979193
# TN / (TN + FP)
spe = 837 /(837+9) # 0.9893617


########## backward elimination
backward <- step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")

lmod_back <- glm(formula = wine ~ volatile.acidity + citric.acid + residual.sugar + 
                   chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
                   density + sulphates + alcohol + quality, family = binomial, 
                 data = train)

# hypothesis test
1-pchisq(lmod_back$deviance-regfull$deviance,2) # 0.9201967

# Roc
preds<-predict(lmod_back,newdata=test, type="response")
rates<-prediction(preds, test$wine)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result)
lines(x = c(0,1), y = c(0,1), col="red")

# auc
auc<-performance(rates, measure = "auc")
auc@y.values[[1]] # 0.9983453

# see confusion table
table(test$wine, preds>0.5)
# very nice
