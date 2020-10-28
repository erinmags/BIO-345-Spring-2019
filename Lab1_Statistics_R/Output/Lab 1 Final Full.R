### Question 1 ###

#Importing Data Body Length
Cucumber <- read.csv ("Lab1_Statistics_R/Data/SeaCucumber_Length.csv") #Importing data into R from my working directory

#Hardcoding Statistics for Purple.SC
species.Purple.SC <- subset(Cucumber,Species=="Purple.SC")
n.species.Purple.SC <- length(species.Purple.SC$Length) #Sample size (n) of Purple.SC
sum.species.Purple.SC <-sum(species.Purple.SC$Length) #Sum of Lengths of Purple.SC
mean.species.Purple.SC<-mean(species.Purple.SC$Length) #Mean of Lengths of Purple.SC
as.species.Purple.SC<-sum(species.Purple.SC$Length^2) #Sum of Lengths of Purple.SC squared
ss.species.Purple.SC<-as.species.Purple.SC - (sum.species.Purple.SC^2/n.species.Purple.SC) #Sum of Squares for Purple.SC
df.species.Purple.SC<-n.species.Purple.SC-1 #Degrees of Freedom
var.species.Purple.SC<-ss.species.Purple.SC/df.species.Purple.SC #Variance of Purple.SC
sd.species.Purple.SC<-sqrt(var.species.Purple.SC) #Standard Deviation
se.species.Purple.SC<-sd.species.Purple.SC/(sqrt(n.species.Purple.SC)) #Standard Error
cf.species.Purple.SC<-(sum.species.Purple.SC)^2/n.species.Purple.SC #Correction Factor Purple.SC

#Hardcoding Statistics for CA.SC
species.CA.SC<-subset(Cucumber,Species=="CA.SC")
n.species.CA.SC<-length(species.CA.SC$Length) #Sample size (n) of CA.SC
sum.species.CA.SC<-sum(species.CA.SC$Length) #Sum of Lengths of CA.SC
mean.species.CA.SC<-mean(species.CA.SC$Length) #Mean of Lengths of CA.SC
as.species.CA.SC<-sum(species.CA.SC$Length^2) #Sum of Lengths of CA.SC squared
ss.species.CA.SC<-as.species.CA.SC-(sum.species.CA.SC^2/n.species.CA.SC) #Sum of Squares
df.species.CA.SC<-n.species.CA.SC-1 #Degrees of Freedom
var.species.CA.SC<-ss.species.CA.SC/df.species.CA.SC #Variance of CA.SC
sd.species.CA.SC<-sqrt(var.species.CA.SC) #Standard Deviation
se.species.CA.SC<-sd.species.CA.SC/(sqrt(n.species.CA.SC)) #Standard Error
cf.species.CA.SC<-(sum.species.CA.SC)^2/n.species.CA.SC #Correction factor CA.SC

#Hardcoding Statistics Pooled
n.pooled<-length(Cucumber$Length) #Sample size (n) of both species
sum.pooled<-sum(Cucumber$Length) #Sum of Lengths of both species
mean.pooled<-mean(Cucumber$Length) #Mean of Lengths of both species
as.pooled<-sum(Cucumber$Length^2) #Sum of Lengths of both species squared
ss.pooled<-as.pooled-(sum.pooled^2/n.pooled) #Sum of Squares
df.pooled<-n.pooled-1 #Degrees of Freedom
var.pooled<-ss.pooled/df.pooled #Variance of both species
sd.pooled<-sqrt(var.pooled) #Standard Deviation
se.pooled<-sd.pooled/(sqrt(n.pooled)) #Standard Error

#T-test
t.test(Cucumber$Length ~ Cucumber$Species) #t-test
#Boxplot
boxplot(Cucumber$Length ~ Cucumber$Species)


### Question 2 ###

#Importing Data Dive Time
DiveTime <- read.csv ("Lab1_Statistics_R/Data/DiveTime.csv") #Importing data into R from my working directory

#Hardcoding Statistics for Before
Before <- subset(DiveTime,Birth.Status=="Before")
n.Before <- length(Before$Birth.Status) #Sample size (n) of Before
sum.Before <-sum(Before$Dive.Time) #Sum of Dive Time of Before
mean.Before<-mean(Before$Dive.Time) #Mean of Dive Times of Before
as.Before<-sum(Before$Dive.Time^2) #Sum of Dive Time of Before squared
ss.Before<-as.Before - (sum.Before^2/n.Before) #Sum of Squares for Before
df.Before<-n.Before-1 #Degrees of Freedom
var.Before<-ss.Before/df.Before #Variance of Before
sd.Before<-sqrt(var.Before) #Standard Deviation
se.Before<-sd.Before/(sqrt(n.Before)) #Standard Error
cf.Before<-(sum.Before)^2/n.Before #Correction factor Before

##Hardcoding Statistics for After
After <- subset(DiveTime,Birth.Status=="After")
n.After <- length(After$Birth.Status) #Sample size (n) of After
sum.After <-sum(After$Dive.Time) #Sum of Dive Time of After
mean.After<-mean(After$Dive.Time) #Mean of Dive Times of After
as.After<-sum(After$Dive.Time^2) #Sum of Dive Time of After squared
ss.After<-as.After - (sum.After^2/n.After) #Sum of Squares for After
df.After<-n.After-1 #Degrees of Freedom
var.After<-ss.After/df.After #Variance of After
sd.After<-sqrt(var.After) #Standard Deviation
se.After<-sd.After/(sqrt(n.After)) #Standard Error
cf.After<-(sum.After)^2/n.After #Correction factor After

#Hardcoding Statistics Pooled
n.pooled <- length(DiveTime$Birth.Status) #Sample size (n) pooled
sum.pooled <-sum(DiveTime$Dive.Time) #Sum of Dive Time pooled
mean.pooled<-mean(DiveTime$Dive.Time) #Mean of Dive Times pooled
as.pooled<-sum(DiveTime$Dive.Time^2) #Sum of Dive Time of pooled squared
ss.pooled<-as.pooled - (sum.pooled^2/n.pooled) #Sum of Squares pooled
df.pooled<-n.pooled-1 #Degrees of Freedom
var.pooled<-ss.pooled/df.pooled #Variance pooled
sd.pooled<-sqrt(var.pooled) #Standard Deviation
se.pooled<-sd.pooled/(sqrt(n.pooled)) #Standard Error

#T-test
t.test(DiveTime$Dive.Time ~ DiveTime$Birth.Status) #t-test
#Boxplot
boxplot(DiveTime$Dive.Time ~ DiveTime$Birth.Status)


### Question 3 ###

#Importing Data Deep Sea Squid
DeepSea <- read.csv ("Lab1_Statistics_R/Data/DeepSea_Squid.csv") #Importing data into R from my working directory

#slope
plot(Body_Mass ~ Ratio, data = DeepSea) #scatterplot

Squid <- lm(Body_Mass ~ Ratio, data = DeepSea) #creating a variable with the linear model

abline(Squid) #draws slope from linear model on plot
summary(Squid) #Summary of the model



