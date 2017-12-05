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

library(ggplot2)

#####################################
# Read Data
#####################################

baseData <- read.csv("input/baseDataSet.csv")
treatmentData <- read.csv("input/treatmentDataSet.csv")
baseEstimates <- read.csv("input/baseEstimates.csv")
baseEstimatesBias <- read.csv("input/baseEstimatesBias.csv")
exclosureEstimates <- read.csv("input/exclosureEstimates.csv")
noExclosureEstimates <- read.csv("input/noExclosureEstimates.csv")

#####################################
# Set Constants
#####################################

# From Martens and Goossen 2008
K.exp <- 0.084
I.exp <- 11.271
A.exp <- 53.4

#####################################
# Plots
#####################################

# Relative bias of base model plots
png("output/fitAnalysis/relBias.png", width = 7, height = 3, units = "in", res = 300)
par(mfrow = c(1,3))
hist(baseEstimatesBias$A.RelBias, xlab = "Relative Bias (%)", main = "Relative Bias of\nAsymptotic Mass (A)")
hist(baseEstimatesBias$I.RelBias, xlab = "Relative Bias (%)", main = "Relative Bias of\nInflection Point (I)")
hist(baseEstimatesBias$K.RelBias, xlab = "Relative Bias (%)", main = "Relative Bias of\nGrowth Rate Constant (K)")
dev.off()

#####################################
# Base Model Statistical Tests
#####################################

# Simualated Day 31 vs Target
day31Data <- baseData[ which(baseData$Day == 31), ]
chars <- capture.output(print(t.test(day31Data$Mean.Weight, mu = 53.4*exp(-exp(-0.084*(31-11.271))))))
writeLines(chars, con = file("output/fitAnalysis/tests/base/meanSimDay31VsTarget.txt"))

# Mean Simulated Growth Rate Constant vs. Target
chars <- capture.output(print(t.test(baseEstimates$K.Est, mu = K.exp)))
writeLines(chars, con = file("output/fitAnalysis/tests/base/meanSimGrowthRateVsTarget.txt"))

# Mean Simulated Mass Asymptote vs. Target
chars <- capture.output(print(t.test(baseEstimates$A.Est, mu = A.exp)))
writeLines(chars, con = file("output/fitAnalysis/tests/base/meanSimMassAsympVsTarget.txt"))

# Mean Simulated Inflection vs Target
chars <- capture.output(print(t.test(baseEstimates$I.Est, m = I.exp)))
writeLines(chars, con = file("output/fitAnalysis/tests/base/meanSimInflectionVsTarget.txt"))

closeAllConnections()

#####################################
# Anthro Presence Statistical Tests
#####################################

noExclosureData <- treatmentData[ which(treatmentData$Excl.Rad == "No Exclosure"), ]

for (anthro in unique(noExclosureData$Anthro))
{
  # Simulated Day 31 vs Base 31
  data <- noExclosureData[ which(noExclosureData$Anthro == anthro), ]
  data <- data[ which(data$Day == 31), ]
  chars <- capture.output(print(t.test(day31Data$Mean.Weight, data$Mean.Weight)))
  writeLines(chars, con = file(paste("output/fitAnalysis/tests/anthro/", 
                                     anthro, "/31Day.txt", sep="")))
  
  # Anthro Asymptotic Mass vs Base
  data1 <- baseEstimates$A.Est
  data2 <- eval(parse(text = paste("noExclosureEstimates$A.", anthro, sep="")))
  chars <- capture.output(print(t.test(data1, data2)))
  writeLines(chars, con = file(paste("output/fitAnalysis/tests/anthro/", 
                                     anthro, "/meanMassAsymptoteEstimate.txt", sep="")))
  
  # Anthro Asymptotic Mass vs Base
  data1 <- baseEstimates$K.Est
  data2 <- eval(parse(text = paste("noExclosureEstimates$K.", anthro, sep="")))
  chars <- capture.output(print(t.test(data1, data2)))
  writeLines(chars, con = file(paste("output/fitAnalysis/tests/anthro/", 
                                     anthro, "/meanGrowthEstimate.txt", sep="")))
}

closeAllConnections()

#####################################
# Exclosure Statistical Tests
#####################################

exclosureData <- treatmentData[ which(treatmentData$Excl.Rad == "Exclosure"), ]

for (anthro in unique(exclosureData$Anthro))
{
  # Simulated Day 31 vs Base 31
  data <- exclosureData[ which(exclosureData$Anthro == anthro), ]
  data <- data[ which(data$Day == 31), ]
  chars <- capture.output(print(t.test(day31Data$Mean.Weight, data$Mean.Weight)))
  writeLines(chars, con = file(paste("output/fitAnalysis/tests/exclosure/", 
                                     anthro, "/31Day.txt", sep="")))
  
  # Anthro Asymptotic Mass vs Base
  data1 <- baseEstimates$A.Est
  data2 <- eval(parse(text = paste("exclosureEstimates$A.", anthro, sep="")))
  chars <- capture.output(print(t.test(data1, data2)))
  writeLines(chars, con = file(paste("output/fitAnalysis/tests/exclosure/", 
                                     anthro, "/meanMassAsymptoteEstimate.txt", sep="")))
  
  # Anthro Asymptotic Mass vs Base
  data1 <- baseEstimates$K.Est
  data2 <- eval(parse(text = paste("exclosureEstimates$K.", anthro, sep="")))
  chars <- capture.output(print(t.test(data1, data2)))
  writeLines(chars, con = file(paste("output/fitAnalysis/tests/exclosure/", 
                                     anthro, "/meanGrowthEstimate.txt", sep="")))
}

closeAllConnections()

#####################################
# Exclosure vs. Non Statistical Tests
#####################################

exclosureData <- treatmentData[ which(treatmentData$Excl.Rad == "Exclosure"), ]
noExclosureData <- treatmentData[ which(treatmentData$Excl.Rad == "No Exclosure"), ]

for (anthro in unique(treatmentData$Anthro))
{
  # Day 31 Chick weight
  data1 <- noExclosureData[ which(noExclosureData$Anthro == anthro), ]
  data1 <- data1[ which(data1$Day == 31), ]
  data2 <- exclosureData[ which(exclosureData$Anthro == anthro), ]
  data2 <- data2[ which(data2$Day == 31), ]
  chars <- capture.output(print(t.test(data1$Mean.Weight, data2$Mean.Weight)))
  writeLines(chars, con = file(paste("output/fitAnalysis/tests/interaction/",
                                     anthro, "/31Day.txt", sep = "")))
  
  # Asymptotic mass
  data1 <- eval(parse(text = paste("noExclosureEstimates$A.", anthro, sep="")))
  data2 <- eval(parse(text = paste("exclosureEstimates$A.", anthro, sep="")))
  chars <- capture.output(print(t.test(data1, data2)))
  writeLines(chars, con = file(paste("output/fitAnalysis/tests/interaction/",
                                     anthro, "/meanMassAsymptoteEstimate.txt", sep = "")))
  
  # Growth Rate
  data1 <- eval(parse(text = paste("noExclosureEstimates$K.", anthro, sep="")))
  data2 <- eval(parse(text = paste("exclosureEstimates$K.", anthro, sep="")))
  chars <- capture.output(print(t.test(data1, data2)))
  writeLines(chars, con = file(paste("output/fitAnalysis/tests/interaction/",
                                     anthro, "/meanGrowthEstimate.txt", sep = "")))  
}
