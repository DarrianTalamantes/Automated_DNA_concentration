#!/usr/bin/env Rscript
library(tidyverse)
library(ggplot2)

# # Argument inputs
Args <- commandArgs(trailingOnly=TRUE)
# grabbing light intensity data 
data_table_comma <- read.table(Args[1], sep = ",", header = TRUE)
dilution_factor <- as.integer(Args[2])
# data_table_comma <- read.table("/home/drt83172/Documents/Automated_DNA_Concentration/Data/Test_data.csv", sep = ",", header = TRUE)
# dilution_factor <- 20

# # splitting data up 
std_data <- data_table_comma[row.names(data_table_comma) %in% 1:16,]
sample_data <- data_table_comma[row.names(data_table_comma) %in% 17:nrow(data_table_comma),]

# # finding the sum of X*Y
std_data$xy <- std_data[,3]*std_data[,4]
XY <- sum(std_data$xy)

# # finding X^2
std_data$x2 <- std_data[,4]^2
X2 <- sum(std_data$x2)

# # finding the sum of X

X <- sum(std_data[,4])
# # finding the sum of Y
Y <- sum(std_data[,3])

# # finding m and b
n = nrow(std_data)
m = ((n * XY) - (X * Y)) / ((n * X2) - X^2)
b = (Y-(m*X))/n

# # using line of best fit equation to get ng of DNA

for (i in 1:nrow(sample_data)){
  sample_data[i,4] <- round((sample_data[i,3]-b)/m, digits = 3)
}

# # Calculating true concentrations
sample_data$True_Concentration <- round(sample_data[,4]*dilution_factor, digits = 2)
# # exporting data
write.csv(sample_data,"DNA_Concentrations.csv", row.names = FALSE)

################################## Making Graphs ###############################

ggplot() +
  geom_point(data = std_data, aes(x = std_data[,4], y = std_data[,3]), color='black', size = 5) +
  geom_point(data = sample_data, aes(x = sample_data[,4], y = sample_data[,3]), color='Green') 
ggsave(file="Standard_Curve.png")
