#####################################
# Brandon Edwards
# melodus-analysis
# chick-weight-analysis.R
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

p <- ggplot() +
  theme(plot.title = element_text(size = 20, face = "bold"), axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12)) + 
  labs(title = "(a) Mean Simulated Chick Weights vs. Expected Chick Weights", x = "Day", y = "Weight (g)") + 
  geom_point(data = data, aes(x = Day, y = Mean.Weight, group=Run, colour = "Simulated"), size = 0.5, alpha = 0.5) +
  geom_line(data = dataSummary, aes(x = Day, y = Mean.Weight, colour = "Mean Simulated"), size = 2) +
  geom_line(data = growth, aes(x = Day, y = Gompertz, colour = "Gompertz Expected"), size = 1.25) +
  geom_line(data = growth, aes(x = Day, y = Logistic, colour = "Logistic Expected"), size = 1.25) +
  annotate("text", x = 20, y = 20, size = 6, label = paste("n = ", length(unique(data$Run)), " simulations", sep = "")) + 
  scale_color_manual(name = "", values=c("red", "blue", "black", "green3"))

png("meanSimChickWeight.png", width = 10, height = 7, units = "in", res = 300)
print(p)
dev.off()
