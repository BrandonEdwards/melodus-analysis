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

#include.packages("ggplot2")

library(ggplot2)

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
anthro <- NULL
exclosure <- NULL
mean <- NULL
stddev.pos <- NULL
stddev.neg <- NULL

for (i in unique(data$Anthro))
{
  temp <- data[ which(data$Anthro == i), ]
  for (j in unique(temp$Excl.Rad))
  {
    temp2 <- temp[ which(temp$Excl.Rad == j), ]
    n <- sum(temp2$Num.Chicks)
    popTotal <- 0
    for (k in 1:nrow(temp2))
    {
      popTotal <- popTotal + (temp2$Mean.Weight[k] * temp2$Num.Chicks[k])
    }
    anthro <- c(anthro, i)
    exclosure <- c(exclosure, j)
    mean <- c(mean, (popTotal/n))
    stddev.pos <- c(stddev.pos, ((popTotal/n) + sd(temp2$Mean.Weight)))
    stddev.neg <- c(stddev.neg, ((popTotal/n) - sd(temp2$Mean.Weight)))    
  }
}

dataSummary <- data.frame(Anthro = anthro, Exclosure = exclosure, Mean.Weight = mean, STDDEV.Neg = stddev.neg, STDDEV.Pos = stddev.pos)
dataSummary$Anthro <- as.factor(dataSummary$Anthro)

#####################################
# Generate Interaction Plot
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
  labs(title = "Interaction Plot", x = "Anthropogenic Level (%)", y = "Weight (g)") +
  geom_line(data = dataSummary, aes(x = Anthro, y = Mean.Weight, group = Exclosure, colour = Exclosure), size = 2)+
  scale_color_manual(name = "Exclosure Level", values=c("black", "#E41A1C", "green3"), labels = c("100m Exclosure", " No Exclosure"))

png("interactionPlot.png", width = 10.5, height = 6, units = "in", res = 300)
print(p)
dev.off()
