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

#install.packages("ggplot2")

library(ggplot2)

################################
# Read Data
################################

# Create empty data frame to bind data to
data <- data.frame(Day=integer(),
                 Num.Chicks=integer(),
                 Weight=character(),
                 Mean.Weight=double(),
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
    names(temp)<-c("Day", "Num.Chicks", "Weight", "Mean.Weight", "Run")
    data <- rbind(data, temp)
  }
}


# Number of files to read in
#numFiles <- 2
#for (i in 0:numFiles)
#{
#  temp <- data.frame(read.csv(paste("05-07-2017/run", i, "/chickWeights.csv", sep="")))
#  temp$Run <- i
#  names(temp)<-c("Day", "Num.Chicks", "Weight", "Mean.Weight", "Run")
#  data <- rbind(data, temp)
#}

################################
# Plot
################################

pdf("chickWeight.pdf")

p <- ggplot(data = data, aes(x = Day, y = Weight (g), group=Run)) + 
  geom_line(aes(y = Mean.Weight, color = Run)) +
  geom_abline(mapping = NULL, data = NULL, colour = "black", size = 2, slope = 1.375, intercept = 3.625)
  #+ annotate("text", x = 13, y = 4, label = "Expected growth curve (Martens and Goossen 2008)")

print(p)
dev.off()