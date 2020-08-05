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

# externally studentized residuals plot for response outliers
ext.student.res<-rstudent(lmod)
plot(lmod$fitted.values,ext.student.res,main="Externally Studentized Residuals")
n <- dim(data)[1]
p <- 13
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")

# get the outliers and indices.
ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]
ind_R <- which(abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1))

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
table(ind_R %in% ind_COOKS)
table(ind_L %in% ind_COOKS)
# one of the outliers are influential 

# remove the influential response outlier
ind <- ind_R[ind_R %in% ind_COOKS]
data_F <- data[-ind,]

# save the filtered data
write.table(data_F, "data_F.txt", col.names=T, row.names=F, sep="\t", quote=F)

################################
############done filtering outliers
##################################

# read in the data  
data <- read.table("data_F.txt", header=T, sep="\t")
data$wine <- as.factor(data$wine)

attach(data)

# levene test and boxplots 
levene.test(fixed.acidity,wine) # 2.2e-16
boxplot(fixed.acidity~wine,data=data, main="fixed.acidity vs wine", xlab="wine", ylab="fixed.acidity")

levene.test(volatile.acidity,wine) # 2.2e-16
boxplot(volatile.acidity~wine,data=data, main="volatile.acidity vs wine", xlab="wine", ylab="volatile.acidity")

levene.test(citric.acid,wine) # 2.2e-16
boxplot(citric.acid~wine,data=data, main="citric.acid vs wine", xlab="wine", ylab="citric.acid")

levene.test(residual.sugar,wine) # 2.2e-16
boxplot(residual.sugar~wine,data=data, main="residual.sugar vs wine", xlab="wine", ylab="residual.sugar")

# log transformed to see the box plot more easily
levene.test(chlorides,wine) # 2.2e-16
boxplot(log(chlorides)~wine,data=data, main="chlorides vs wine", xlab="wine", ylab="chlorides")

# one very doninant outlier
levene.test(free.sulfur.dioxide,wine) # 2.2e-16
boxplot(free.sulfur.dioxide~wine,data=data, main="free.sulfur.dioxide vs wine", xlab="wine", ylab="free.sulfur.dioxide")

levene.test(total.sulfur.dioxide,wine) # 2.2e-16
boxplot(total.sulfur.dioxide~wine,data=data, main="total.sulfur.dioxide vs wine", xlab="wine", ylab="total.sulfur.dioxide")

levene.test(density,wine) # 2.2e-16
boxplot(density~wine,data=data, main="density vs wine", xlab="wine", ylab="density")

levene.test(pH,wine) # 0.4877
boxplot(pH~wine,data=data, main="pH vs wine", xlab="wine", ylab="pH")

levene.test(sulphates,wine) # 2.2e-16
boxplot(sulphates~wine,data=data, main="sulphates vs wine", xlab="wine", ylab="sulphates")

levene.test(alcohol,wine) # 2.2e-16
boxplot(alcohol~wine,data=data, main="alcohol vs wine", xlab="wine", ylab="alcohol")

levene.test(quality,wine) # 0.1267
boxplot(quality~wine,data=data, main="quality vs wine", xlab="wine", ylab="quality")

# pH and quality had no significant difference between two wines, but rest did

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

# see if all coef are 0
1-pchisq(regnull$deviance-regfull$deviance,12)
# 0 no at least one of them are not

# use forward selection
forward <- step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")

# compare the forward selected model with the full model
lmod_S <- glm( wine ~ total.sulfur.dioxide + density + residual.sugar + 
      alcohol + volatile.acidity + free.sulfur.dioxide + chlorides + 
      fixed.acidity + pH + quality, family = binomial, data = train)
# removed chitric.acid and sulphates

# compare with a model without non significant predictors ( - sulphates and - citric.acid)
# see if the reduced model is better
1-pchisq(lmod_S$deviance-regfull$deviance,2)
# p value = 0.5672371
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
# 0.9979405 very close to 1

# see confusion table
table(test$wine, preds>0.5)
# very nice

# find type 1 error ,type 2 error, sensitivity, and specificity 
# FP / (TN + FP)
t1e =  10 /(823+10) # 0.0120048
#  FN / (FN + TP)
t2e = 6 /(6+2409) # 0.002484472
# TP / (FN + TP)
sen = 2409 /(6+2409) # 0.9975155
# TN / (TN + FP)
spe = 823 /(823+10) # 0.9879952
