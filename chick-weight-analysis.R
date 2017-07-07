#####################################
# Brandon Edwards
# melodus-analysis
# chick-weight-by-day.R
# July 2017
#####################################

#####################################
# Clear Memory
#####################################
remove(list = ls())

#####################################
# Import Libraries
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
# Calculate Daily Mean Weights
#####################################
day <- NULL
mean <- NULL

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
}

dailyMean <- data.frame(Day = day, Mean.Weight = mean)

#####################################
# Plot Mean Simulated Chick Weights
#####################################

pdf("meanChickWeightPerRun.pdf")

p <- ggplot() +
  theme(plot.title = element_text(size = 16, face = "bold"), axis.title = element_text(size = 12, face = "bold")) + 
  labs(title = "Simulated Chick Weights vs. Expected Chick Weights", x = "Day", y = "Weight (g)") + 
  geom_line(data = data, aes(x = Day, y = Mean.Weight, group=Run), size = 0.33, alpha = 0.2) +
  geom_line(data = dailyMean, aes(x = Day, y= Mean.Weight), size = 1.5, colour = "blue") +
  geom_abline(mapping = NULL, data = NULL, colour = "red", size = 1, slope = 1.375, intercept = 3.625) +
  annotate("text", x = 20, y = 20, label = paste("n = ", length(unique(data$Run)), " simulations", sep = ""))

print(p)
dev.off()

