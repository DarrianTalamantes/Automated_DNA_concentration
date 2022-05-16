#!/usr/bin/env Rscript
library(reshape2)
library(tidyverse)
library(vegan)
library(Rfast)
library(car)
library(ggplot2)
library(plyr)

# # Argument inputs
Args <- commandArgs(trailingOnly=TRUE)
# grabing light intensity data 
data_table_comma <- read.table(Args[1], sep = ",", header = TRUE)





# # Getting the standard curve data
std1x = 500
std2x = 250
std3x = 125
std4x = 62.5
std1y = QPCRmeans2["std1","mean"]
std2y = QPCRmeans2["std2","mean"]
std3y = QPCRmeans2["std3","mean"]
std4y = QPCRmeans2["std4","mean"]

# # Creating a standard curve matrix
stdData = matrix(nrow = 4, ncol = 2)
stdData[1,1]=std1x
stdData[2,1]=std2x
stdData[3,1]=std3x
stdData[4,1]=std4x
stdData[1,2]=std1y
stdData[2,2]=std2y
stdData[3,2]=std3y
stdData[4,2]=std4y

# # finding the sum of X*Y
xy = matrix(nrow = 4, ncol = 1)
i=1
for(i in 1:nrow(stdData)){
  xy[i,1] = stdData[i,1] * stdData[i,2]
}
XY <- sum(xy)
# # finding the sum of X
X <- sum(stdData[,1])
# # finding the sum of Y
Y <- sum(stdData[,2])
# # finding X^2
x2 = matrix(nrow = 4, ncol = 1)
for(i in 1:nrow(stdData)){
  x2[i,1] = stdData[i,1]^2
}
X2 <- sum(x2)

# # finding m and b
n = nrow(stdData)
m = ((n * XY) - (X * Y)) / ((n * X2) - X^2)
b = (Y-(m*X))/n

# # using line of best fit equation to get ng of DNA
PredDNA <- QPCRmeans2 
names(PredDNA)[names(PredDNA) == 'mean'] <- 'ng.of.DNA'
samples <- rownames(PredDNA)
for (i in 1:length(samples)){
  DNAng <- (QPCRmeans2[samples[i],"mean"]-b)/m
  PredDNA[samples[i],"ng.of.DNA"] <- DNAng
}
PredDNA$Cp <- QPCRmeans2$mean
water_remover <- c("Water", ignore_case = TRUE)
PredDNA <- PredDNA[!(row.names(PredDNA) %in% water_remover),]


################################## Making Graphs ###############################



