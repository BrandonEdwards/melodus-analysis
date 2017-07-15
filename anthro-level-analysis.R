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
