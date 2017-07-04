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
library(wesanderson)

################################
# Read Data
################################

d1 <- read.csv(file.choose(), header = FALSE)
d1$Humans <- TRUE
d2 <- read.csv(file.choose(), header = FALSE)
d2$Humans <- FALSE

names(d1)<-c("Day", "Num.Chicks", "c1", "c2", "c3", "c4", "Mean.Weight", "Humans")
names(d2)<-c("Day", "Num.Chicks", "c1", "c2", "c3", "c4", "Mean.Weight", "Humans")

data <- rbind(d1, d2)

################################
# Plot
################################
ggplot(data = data, aes(x = Day, y = Weight (g), group=Humans)) + 
  geom_line(aes(y = data[3], color = Humans)) + 
  geom_line(aes(y = data[4], colour = Humans)) + 
  geom_line(aes(y = data[5], colour = Humans)) + 
  geom_line(aes(y = data[6], colour = Humans)) + 
  geom_abline(mapping = NULL, data = NULL, colour = "black", size = 1, slope = 1.375, intercept = 3.625) + 
  annotate("text", x = 13, y = 32.5, label = "Expected growth curve (Martens and Goossen 2008)")
