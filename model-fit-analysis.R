#####################################
# Brandon Edwards
# melodus-analysis
# model-fit-analysis.R
# Created November 2017
# Last Updated November 2017
#####################################

#####################################
# Clear Memory
#####################################
remove(list = ls())

#####################################
# Import Libraries and Files
#####################################

#install.packages("easynls")

library(easynls)

#####################################
# Set Constants and Read Data
#####################################

# From Martens and Goossen 2008
K.exp <- 0.084
I.exp <- 11.271
A.exp <- 53.4

data <- read.csv("C:/Users/brand/Documents/melodus-analysis/baseDataSet.csv")

#####################################
# Analysis
#####################################

K.AbsBias <- NULL
I.AbsBias <- NULL
A.AbsBias <- NULL

for (i in unique(data$Run))
{
  runData <- data[ which(data$Run == i), ]
  fit <- nlsfit(runData[c("Day", "Mean.Weight")], model=10, start=c(600,4,0.05))
  A.AbsBias <- c(A.AbsBias, (fit$Parameters[1,1] - A.exp))
  I.AbsBias <- c(I.AbsBias, (fit$Parameters[2,1] - I.exp))
  K.AbsBias <- c(K.AbsBias, (fit$Parameters[3,1] - K.exp))
}

A.meanAbsBias <- mean(A.AbsBias)
I.meanAbsBias <- mean(I.AbsBias)
K.meanAbsBias <- mean(K.AbsBias)

A.meanRelBias <- mean(A.AbsBias/A.exp)
I.meanRelBias <- mean(I.AbsBias/I.exp)
K.meanRelBias <- mean(K.AbsBias/K.exp)