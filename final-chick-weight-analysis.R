#####################################
# Brandon Edwards
# melodus-analysis
# final-chick-weight-analysis.R
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

#####################################
# Import Non-Exclosure Data
#####################################

directory <- c("anthro-no-exclosure", "anthro-exclosure",
                    "base-model-calibration", "base-model-calibration")
radius <- c("No Exclosure", "Exclosure", "No Exclosure", "Exclosure")

# Create empty data frame to bind data to
data <- data.frame(Day=integer(),
                   Num.Chicks=integer(),
                   Weight=character(),
                   Mean.Weight=double(),
                   Anthro = integer(),
                   Excl.Rad = integer(),
                   Run=integer(),
                   stringsAsFactors=FALSE)

for (i in 1:length(directory))
{
  dateDirs <- list.files(paste("input/", directory[i], sep=""))
  run <- 0
  for (j in 1:length(dateDirs))
  {
    runDirs <- list.files(paste("input/", directory[i], "/", dateDirs[j], sep=""))
    for (k in 1:length(runDirs))
    {
      temp <- data.frame(read.csv(paste("input/", directory[i], "/", dateDirs[j], "/", runDirs[k], "/chickWeights.csv", sep="")))
      temp$Excl.Rad <- radius[i]      
      temp$Run <- run
      run <- run + 1
      names(temp)<-c("Day", "Num.Chicks", "Weight", "Mean.Weight", "Anthro", "Excl.Rad", "Run")
      data <- rbind(data, temp)
    }
  }  
}

# Get only final day values
data <- data[ which(data$Day == 31), ]

#####################################
# Calculate Grand Daily Mean Weights
#####################################
day <- NULL
anthro <- NULL
mean <- NULL
stddev.pos <- NULL
stddev.neg <- NULL

for (i in unique(data$Day))
{
  temp <- data[ which(data$Day == i), ]
  for (j in unique(temp$Anthro))
  {
    temp2 <- temp[ which(temp$Anthro == j), ]
    n <- sum(temp2$Num.Chicks)
    popTotal <- 0
    for (k in 1:nrow(temp2))
    {
      popTotal <- popTotal + (temp2$Mean.Weight[k] * temp2$Num.Chicks[k])
    }
    
    day <- c(day, i)
    anthro <- c(anthro, j)
    mean <- c(mean, (popTotal/n))
    stddev.pos <- c(stddev.pos, ((popTotal/n) + sd(temp2$Mean.Weight)))
    stddev.neg <- c(stddev.neg, ((popTotal/n) - sd(temp2$Mean.Weight)))    
  }
}

dataSummary <- data.frame(Day = day, Anthro = anthro, Mean.Weight = mean, STDDEV.Neg = stddev.neg, STDDEV.Pos = stddev.pos)
dataSummary$Anthro <- as.factor(dataSummary$Anthro)
