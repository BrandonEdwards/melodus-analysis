################################
# Brandon Edwards
# melodus-analysis
# chick-weight-by-day.R
# June 2017
################################

################################
# Clear Memory
################################
remove(list = ls())

################################
# Import Libraries
################################
library(ggplot2)

################################
# Read Data
################################

# Create empty data frame to bind data to
data <- data.frame(Day=integer(),
                 Num.Chicks=integer(),
                 c1=double(),
                 c2=double(),
                 c3=double(),
                 c4=double(),
                 Mean.Weight=double(),
                 Run=integer(),
                 stringsAsFactors=FALSE)

# Number of files to read in
numFiles <- 8
for (i in 1:numFiles)
{
  temp <- read.csv(paste("testResultsNoHumans", i, ".csv", sep=""), header = FALSE)
  temp$Run <- i
  names(temp)<-c("Day", "Num.Chicks", "c1", "c2", "c3", "c4", "Mean.Weight", "Run")
  data <- rbind(data, temp)
}

################################
# Plot
################################
ggplot(data = data, aes(x = Day, y = Weight (g), group=Run)) + 
  geom_line(aes(y = data[3], color = Run)) + 
  geom_line(aes(y = data[4], colour = Run)) + 
  geom_line(aes(y = data[5], colour = Run)) + 
  geom_line(aes(y = data[6], colour = Run)) + 
  geom_abline(mapping = NULL, data = NULL, colour = "black", size = 1, slope = 1.375, intercept = 3.625) + 
  annotate("text", x = 13, y = 32.5, label = "Expected growth curve (Martens and Goossen 2008)")
