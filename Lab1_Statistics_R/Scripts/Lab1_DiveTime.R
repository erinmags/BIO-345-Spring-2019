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

                                 
                