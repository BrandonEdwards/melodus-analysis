#####################################
# Brandon Edwards
# melodus-analysis
# anthro-level-analysis.R
# Created July 2017
# Last Updated July 2017
#####################################

#####################################
# Clear Memory
#####################################

remove(list = ls())

#####################################
# Import Libraries and Files
#####################################

#install.packages("ggplot2")

library(ggplot2)

#####################################
# Read Data
#####################################

# Create empty data frame to bind data to
data <- data.frame(Day=integer(),
                   Num.Chicks=integer(),
                   Weight=character(),
                   Mean.Weight=double(),
                   Anthro = integer(),
                   Run=integer(),
                   stringsAsFactors=FALSE)

dateDirs <- list.files("input")
run <- 0
for (i in 1:length(dateDirs))
{
  runDirs <- list.files(paste("input/", dateDirs[i], sep=""))
  for (j in 1:length(runDirs))
  {
    temp <- data.frame(read.csv(paste("input/", dateDirs[i], "/", runDirs[j], "/chickWeights.csv", sep="")))
    temp$Run <- run
    run <- run + 1
    names(temp)<-c("Day", "Num.Chicks", "Weight", "Mean.Weight", "Anthro", "Run")
    data <- rbind(data, temp)
  }
}

#####################################
# Create Gompertz Growth Curve
#####################################

# From Martens and Goossen 2008
K <- 0.084
I <- 11.271
A <- 53.4

growth <- data.frame(Day = c(1:31))

growth$Gompertz <- A * exp(1)^(-exp(1)^(-K * (growth$Day - I)))

#####################################
# Create Logistic Growth Curve
#####################################

#From Martens adn Goossen 2008
K <- 0.129
I <- 15.890
A <- 53.4

growth$Logistic <- A / (1 + exp(1)^(-K * (growth$Day - I)))


