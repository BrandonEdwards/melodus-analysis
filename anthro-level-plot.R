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
#install.packages("extrafont")

library(ggplot2)
library(extrafont)
#font_import(pattern="FRAMDCN")
#y

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
#####################################
# Plot Mean Simulated Chick Weights
#####################################

p <- ggplot() +
  theme(plot.title = element_text(size = 30, face = "bold", family = "Franklin Gothic Book"), 
        axis.title = element_text(size = 20, family = "Franklin Gothic Book"),
        axis.text = element_text(size = 18, family = "Franklin Gothic Book"), 
        legend.title = element_text(size = 20, family = "Franklin Gothic Book"), 
        legend.text = element_text(size = 18, family = "Franklin Gothic Book"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  labs(title = "(a) Anthro Effects with No Exclosure", x = "Day", y = "Weight (g)") +
  geom_line(data = dataSummary, aes(x = Day, y = Mean.Weight, group = Anthro, colour = Anthro), size = 1.5)+
  scale_color_manual(name = "Anthro Level (%)", values=c("black", "#66C2A5", "#FC8D62", "#8DA0CB"), labels = c("Base Model", "20", "40", "60"))

png("meanSimChickWeightAnthroNoExclosure.png", width = 10.5, height = 4, units = "in", res = 300)
print(p)
dev.off()
