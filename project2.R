#setwd("C:/Users/joony/Documents/STAT")

library(tidyverse)

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

################################
##################################
##################################
# read in the data  
data <- read.table("data_t.txt", header=T, sep="\t")



