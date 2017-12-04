#####################################
# Brandon Edwards
# melodus-analysis
# model-fit-analysis.R
# Created November 2017
# Last Updated December 2017
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
library(ggplot2)

#####################################
# Read Data
#####################################

baseData <- read.csv("baseDataSet.csv")
treatmentData <- read.csv("treatmentDataSet.csv")

#####################################
# Set Constants
#####################################

# From Martens and Goossen 2008
K.exp <- 0.084
I.exp <- 11.271
A.exp <- 53.4

#####################################
# Base Estimate Generation
#####################################
A.Base <- NULL
I.Base <- NULL
K.Base <- NULL

K.Base.AbsBias <- NULL
I.Base.AbsBias <- NULL
A.Base.AbsBias <- NULL

for (i in unique(baseData$Run))
{
  data <- baseData[ which(baseData$Run == i), ]
  fit <- nlsfit(data[c("Day", "Mean.Weight")], model=10, start=c(200,4,0.05))
  A.Base <- c(A.Base, fit$Parameters[1,1])
  I.Base <- c(I.Base, fit$Parameters[2,1])
  K.Base <- c(K.Base, fit$Parameters[3,1])
  
  A.Base.AbsBias <- c(A.Base.AbsBias, (fit$Parameters[1,1] - A.exp))
  I.Base.AbsBias <- c(I.Base.AbsBias, (fit$Parameters[2,1] - I.exp))
  K.Base.AbsBias <- c(K.Base.AbsBias, (fit$Parameters[3,1] - K.exp))
}

A.Base.RelBias <- (A.Base.AbsBias/A.exp)*100
I.Base.RelBias <- (I.Base.AbsBias/I.exp)*100
K.Base.RelBias <- (K.Base.AbsBias/K.exp)*100

baseEstimates <- cbind(A.Base, I.Base, K.Base)
baseEstimates <- data.frame(baseEstimates)
names(baseEstimates) <- c("A.Est", "I.Est", "K.Est")
write.csv(baseEstimates, "output/fitAnalysis/baseEstimates.csv", row.names = FALSE)

baseEstimatesBias <- cbind(A.Base.AbsBias, I.Base.AbsBias, K.Base.AbsBias,
                           A.Base.RelBias, I.Base.RelBias, K.Base.RelBias)
baseEstimatesBias <- data.frame(baseEstimatesBias)
names(baseEstimatesBias) <- c("A.AbsBias", "I.AbsBias", "K.AbsBias",
                             "A.RelBias", "I.RelBias", "K.RelBias")
write.csv(baseEstimatesBias, "output/fitAnalysis/baseEstimatesBias.csv", row.names = FALSE)

#####################################
# Non-Exclosure Estimate Generation
#####################################

dataNoExclosure <- treatmentData[ which(treatmentData$Excl.Rad == "No Exclosure"), ]

noExclosureEstimates <- NULL

for (anthro in unique(dataNoExclosure$Anthro))
{
  eval(parse(text = paste("A.", anthro, ".N <- NULL", sep = "")))
  eval(parse(text = paste("I.", anthro, ".N <- NULL", sep = "")))
  eval(parse(text = paste("K.", anthro, ".N <- NULL", sep = "")))
  
#  eval(parse(text = paste("A.", anthro, ".AbsBias <- NULL", sep = "")))
#  eval(parse(text = paste("I.", anthro, ".AbsBias <- NULL", sep = "")))
#  eval(parse(text = paste("K.", anthro, ".AbsBias <- NULL", sep = "")))
  
  data <- dataNoExclosure[ which(dataNoExclosure$Anthro == anthro), ]
  for (i in unique(data$Run))
  {
    dataTemp <- data[ which(data$Run == i), ]
    fit <- nlsfit(dataTemp[c("Day", "Mean.Weight")], model=10, start=c(200,4,0.05))

    #Push parameter estimates
    eval(parse(text = paste("A.", 
                            anthro, 
                            ".N <- c(
                            eval(parse(text = paste(\"A.\", anthro, \".N\", sep = \"\"))), 
                            (fit$Parameters[1,1]))", 
                            sep = "")))
    eval(parse(text = paste("I.", 
                            anthro, 
                            ".N <- c(
                            eval(parse(text = paste(\"I.\", anthro, \".N\", sep = \"\"))), 
                            (fit$Parameters[2,1]))", 
                            sep = "")))
    eval(parse(text = paste("K.", 
                            anthro, 
                            ".N <- c(
                            eval(parse(text = paste(\"K.\", anthro, \".N\", sep = \"\"))), 
                            (fit$Parameters[3,1]))", 
                            sep = "")))
    
    #Push absolute bias, do we need this?
#    eval(parse(text = paste("A.", 
#                            anthro, 
#                            ".AbsBias <- c(
#                            eval(parse(text = paste(\"A.\", anthro, \".AbsBias\", sep = \"\"))), 
#                             (fit$Parameters[1,1] - A.exp))", 
#                            sep = "")))
#    eval(parse(text = paste("I.", 
#                            anthro, 
#                            ".AbsBias <- c(
#                            eval(parse(text = paste(\"I.\", anthro, \".AbsBias\", sep = \"\"))), 
#                             (fit$Parameters[2,1] - I.exp))", 
#                            sep = "")))
##    eval(parse(text = paste("K.", 
#                            anthro, 
#                            ".AbsBias <- c(
#                            eval(parse(text = paste(\"K.\", anthro, \".AbsBias\", sep = \"\"))), 
#                            (fit$Parameters[3,1] - K.exp))", 
#                            sep = "")))
    
  }
  
  noExclosureEstimates <- cbind(noExclosureEstimates,
                                eval(parse(text = paste("A.", anthro, ".N", sep = ""))),
                                eval(parse(text = paste("I.", anthro, ".N", sep = ""))),
                                eval(parse(text = paste("K.", anthro, ".N", sep = ""))))
  
}

noExclosureEstimates <- data.frame(noExclosureEstimates)

names(noExclosureEstimates) <- c("A.20", "I.20", "K.20",
                                 "A.40", "I.40", "K.40",
                                 "A.60", "I.60", "K.60")

write.csv(noExclosureEstimates, "output/fitAnalysis/noExclosureEstimates.csv", 
          row.names = FALSE)

#####################################
# Exclosure Estimate Generation
#####################################

dataExclosure <- treatmentData[ which(treatmentData$Excl.Rad == "Exclosure"), ]
exclosureEstimates <- NULL

for (anthro in unique(dataExclosure$Anthro))
{
  eval(parse(text = paste("A.", anthro, ".E <- NULL", sep = "")))
  eval(parse(text = paste("I.", anthro, ".E <- NULL", sep = "")))
  eval(parse(text = paste("K.", anthro, ".E <- NULL", sep = "")))
  
  #  eval(parse(text = paste("A.", anthro, ".AbsBias <- NULL", sep = "")))
  #  eval(parse(text = paste("I.", anthro, ".AbsBias <- NULL", sep = "")))
  #  eval(parse(text = paste("K.", anthro, ".AbsBias <- NULL", sep = "")))
  
  data <- dataExclosure[ which(dataExclosure$Anthro == anthro), ]
  for (i in unique(data$Run))
  {
    dataTemp <- data[ which(data$Run == i), ]
    fit <- nlsfit(dataTemp[c("Day", "Mean.Weight")], model=10, start=c(200,4,0.05))
    
    #Push parameter estimates
    eval(parse(text = paste("A.", 
                            anthro, 
                            ".E <- c(
                            eval(parse(text = paste(\"A.\", anthro, \".E\", sep = \"\"))), 
                            (fit$Parameters[1,1]))", 
                            sep = "")))
    eval(parse(text = paste("I.", 
                            anthro, 
                            ".E <- c(
                            eval(parse(text = paste(\"I.\", anthro, \".E\", sep = \"\"))), 
                            (fit$Parameters[2,1]))", 
                            sep = "")))
    eval(parse(text = paste("K.", 
                            anthro, 
                            ".E <- c(
                            eval(parse(text = paste(\"K.\", anthro, \".E\", sep = \"\"))), 
                            (fit$Parameters[3,1]))", 
                            sep = "")))
    
    #Push absolute bias, do we need this?
    #    eval(parse(text = paste("A.", 
    #                            anthro, 
    #                            ".AbsBias <- c(
    #                            eval(parse(text = paste(\"A.\", anthro, \".AbsBias\", sep = \"\"))), 
    #                             (fit$Parameters[1,1] - A.exp))", 
    #                            sep = "")))
    #    eval(parse(text = paste("I.", 
    #                            anthro, 
    #                            ".AbsBias <- c(
    #                            eval(parse(text = paste(\"I.\", anthro, \".AbsBias\", sep = \"\"))), 
    #                             (fit$Parameters[2,1] - I.exp))", 
    #                            sep = "")))
    ##    eval(parse(text = paste("K.", 
    #                            anthro, 
    #                            ".AbsBias <- c(
    #                            eval(parse(text = paste(\"K.\", anthro, \".AbsBias\", sep = \"\"))), 
    #                            (fit$Parameters[3,1] - K.exp))", 
    #                            sep = "")))
    
  }
  
  exclosureEstimates <- cbind(exclosureEstimates,
                                eval(parse(text = paste("A.", anthro, ".E", sep = ""))),
                                eval(parse(text = paste("I.", anthro, ".E", sep = ""))),
                                eval(parse(text = paste("K.", anthro, ".E", sep = ""))))
  
}

exclosureEstimates <- data.frame(exclosureEstimates)

names(exclosureEstimates) <- c("A.20", "I.20", "K.20",
                                 "A.40", "I.40", "K.40",
                                 "A.60", "I.60", "K.60")

write.csv(exclosureEstimates, "output/fitAnalysis/exclosureEstimates.csv", 
          row.names = FALSE)

#####################################
# Plots
#####################################

# Relative bias of base model plots
png("output/fitAnalysis/relBias.png", width = 7, height = 3, units = "in", res = 300)
par(mfrow = c(1,3))
hist(A.Base.RelBias, xlab = "Relative Bias (%)", main = "Relative Bias of\nAsymptotic Mass (A)")
hist(I.Base.RelBias, xlab = "Relative Bias (%)", main = "Relative Bias of\nInflection Point (I)")
hist(K.Base.RelBias, xlab = "Relative Bias (%)", main = "Relative Bias of\nGrowth Rate Constant (K)")
dev.off()

#####################################
# Statistical Tests
#####################################

# Simualated Day 31 vs Target
day31Data <- baseData[ which(baseData$Day == 31), ]
chars <- capture.output(print(t.test(day31Data$Mean.Weight, mu = 53.4*exp(-exp(-0.084*(31-11.271))))))
writeLines(chars, con = file("output/fitAnalysis/tests/meanSimDay31VsTarget.txt"))


# Mean Simulated Growth Rate Constant vs. Target
chars <- capture.output(print(t.test(baseEstimates$K.Est, mu = 0.084)))
writeLines(chars, con = file("output/fitAnalysis/tests/meanSimGrowthRateVsTarget.txt"))

# Mean Simulated Mass Asymptote vs. Target
chars <- capture.output(print(t.test(baseEstimates$A.Est, mu = 53.3)))
writeLines(chars, con = file("output/fitAnalysis/tests/meanSimMassAsympVsTarget.txt"))

closeAllConnections()
