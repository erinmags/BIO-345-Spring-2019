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


