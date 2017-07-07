#####################################
# Brandon Edwards
# melodus-analysis
# chick-weight-by-day.R
# Created June 2017
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

#####################################
# Calculate Grand Daily Mean Weights
#####################################
day <- NULL
mean <- NULL
stddev.pos <- NULL
stddev.neg <- NULL

for (i in unique(data$Day))
{
  temp <- data[ which(data$Day == i), ]
  n <- sum(temp$Num.Chicks)
  popTotal <- 0
  for (j in 1:nrow(temp))
  {
    popTotal <- popTotal + (temp$Mean.Weight[j] * temp$Num.Chicks[j])
  }
  
  day <- c(day, i)
  mean <- c(mean, (popTotal/n))
  stddev.pos <- c(stddev.pos, ((popTotal/n) + sd(temp$Mean.Weight)))
  stddev.neg <- c(stddev.neg, ((popTotal/n) - sd(temp$Mean.Weight)))
}

dataSummary <- data.frame(Day = day, Mean.Weight = mean, STDDEV.Neg = stddev.neg, STDDEV.Pos = stddev.pos)

#####################################
# Plot Mean Simulated Chick Weights
#####################################

png("meanSimChickWeight.png", width = 1100, height = 1000)

p <- ggplot() +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.title = element_text(size = 20, face = "bold")) + 
  labs(title = "Mean Simulated Chick Weights vs. Expected Chick Weights", x = "Day", y = "Weight (g)") + 
  geom_line(data = data, aes(x = Day, y = Mean.Weight, group=Run), size = 0.33, alpha = 0.5) +
  geom_abline(mapping = NULL, data = NULL, colour = "red", size = 1, slope = 1.375, intercept = 3.625) +
  annotate("text", x = 20, y = 20, label = paste("n = ", length(unique(data$Run)), " simulations", sep = ""))

print(p)
dev.off()

#####################################
# Plot Mean Time Series
#####################################

png("meanTimeSeries.png", width = 1100, height = 1000)

p <- ggplot(data = dataSummary) +
  theme(plot.title = element_text(size = 30, face = "bold"), axis.title = element_text(size = 20, face = "bold")) + 
  labs(title = "Grand Mean Simulated Chick Weights vs. Expected Chick Weights", x = "Day", y = "Weight (g)") +
  geom_line(data = dataSummary, aes(x = Day, y= Mean.Weight), size = 1.5, colour = "blue") +
  geom_ribbon(aes(x = Day, ymax = STDDEV.Pos, ymin = STDDEV.Neg), alpha = 0.5) +
  annotate("text", x = 20, y = 20, label = paste("n = ", length(unique(data$Run)), " simulations", sep = ""))
  #+ geom_abline(mapping = NULL, data = NULL, colour = "red", size = 1, slope = 1.375, intercept = 3.625)
print(p)
dev.off()