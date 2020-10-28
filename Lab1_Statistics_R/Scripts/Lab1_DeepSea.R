### Question 3 ###

#Importing Data Deep Sea Squid
DeepSea <- read.csv ("Lab1_Statistics_R/Data/DeepSea_Squid.csv") #Importing data into R from my working directory

#slope
plot(Body_Mass ~ Ratio, data = DeepSea) #scatterplot

Squid <- lm(Body_Mass ~ Ratio, data = DeepSea) #creating a variable with the linear model

abline(Squid) #draws slope from linear model on plot
summary(Squid) #Summary of the model

