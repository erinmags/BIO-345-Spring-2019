#Lab 7#

#Load Necessary Packages
library(Rmisc)
library(lattice)
library(plyr)
library(tidyverse)
library(tidyselect)
library(car)

#Importing Data
A04_Data <- read.csv ("Lab7_Photosynthesis/Data/A04_20200401.csv") #Importing data into R from my working directory
A19_Data <- read.csv ("Lab7_Photosynthesis/Data/A19_20200401.csv")
A31_Data <- read.csv ("Lab7_Photosynthesis/Data/A31_20200401.csv")
A39_Data <- read.csv ("Lab7_Photosynthesis/Data/A39_20200401.csv")
A41_Data <- read.csv ("Lab7_Photosynthesis/Data/A41_20200401.csv")
A45_Data <- read.csv ("Lab7_Photosynthesis/Data/A45_20200401.csv")
B1_Data <- read.csv ("Lab7_Photosynthesis/Data/B1_20200401.csv")
B2_Data <- read.csv ("Lab7_Photosynthesis/Data/B2_20200401.csv")

#Selecting Columns
select_B1<-select(B1_Data,delta_t,Value)
select_B2<-select(B2_Data,delta_t,Value)
select_A04<-select(A04_Data,delta_t,Value)
select_A19<-select(A19_Data,delta_t,Value)
select_A31<-select(A31_Data,delta_t,Value)
select_A39<-select(A39_Data,delta_t,Value)
select_A41<-select(A41_Data,delta_t,Value)
select_A45<-select(A45_Data,delta_t,Value)

#Plotting B1
plot.select_B1<- ggplot(data = select_B1,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model B1
lmB1 <- lm (Value ~ delta_t, data = select_B1) 
lmsummaryB1 <- summary(lmB1) 

#Plotting B2
plot.select_B2<- ggplot(data = select_B2,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model B2
lmB2 <- lm (Value ~ delta_t, data = select_B2) 
lmsummaryB2 <- summary(lmB2) 

#Plotting A04
plot.select_A04<- ggplot(data = select_A04,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model A04
lmA04 <- lm (Value ~ delta_t, data = select_A04) 
lmsummaryA04 <- summary(lmA04) 

#Plotting A19
plot.select_A19<- ggplot(data = select_A19,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model A19
lmA19 <- lm (Value ~ delta_t, data = select_A19) 
lmsummaryA19 <- summary(lmA19) 

#Plotting A31
plot.select_A31<- ggplot(data = select_A31,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model A31
lmA31 <- lm (Value ~ delta_t, data = select_A31) 
lmsummaryA31 <- summary(lmA31) 

#Plotting A39
plot.select_A39<- ggplot(data = select_A39,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model A39
lmA39 <- lm (Value ~ delta_t, data = select_A39) 
lmsummaryA39 <- summary(lmA39) 

#Plotting A41
plot.select_A41<- ggplot(data = select_A41,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model A41
lmA41 <- lm (Value ~ delta_t, data = select_A41) 
lmsummaryA41 <- summary(lmA41) 

#Plotting A45
plot.select_A45<- ggplot(data = select_A45,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model A45
lmA45 <- lm (Value ~ delta_t, data = select_A45) 
lmsummaryA45 <- summary(lmA45) 

#Import Calculated Rates Data
Calc.Rates_Data <- read.csv ("Lab7_Photosynthesis/Data/Calculated_rates.csv") #Importing data into R from my working directory

#Subset Hot
subset.hot<-subset(Calc.Rates_Data,Treatment=="Hot")

#Subset Cold
subset.cold<-subset(Calc.Rates_Data,Treatment=="Cold")

#Correcting for Blank Hot
subset.hot$Rate.corr<-subset.hot$Rate.O2.L.min-(0.0133)

#Correcting for Blank Cold
subset.cold$Rate.corr<-subset.cold$Rate.O2.L.min-(0.012)

#Rebind Data
Rebind_Data<-rbind(subset.cold,subset.hot)

#Filter Samples
samples <- Rebind_Data %>% 
  filter(Sample.Type == "Sample") 

#Normalize Corrected Rate for Surface Area
samples$Corr.Rate.O2.L.min.cm2<-samples$Rate.O2.L.min/samples$Surface.Area.cm2

#Boxplot Treatment
plot.photo<- ggplot(data = samples, aes(x=Treatment, y=Corr.Rate.O2.L.min.cm2))+ 
  geom_boxplot() +
  ylab("Photosynthesis Rate (umolO2L^-1min^-1cm^-2)")+ xlab("Tank") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model
lmphoto <- lm (Corr.Rate.O2.L.min.cm2~Treatment, data = samples) #Creating Linear Model
lmsummaryphoto <- summary(lmphoto)

#Check Homogeneity of Variances
boxplot(resid(lmphoto)~samples$Treatment)
leveneTest(resid(lmphoto) ~ samples$Treatment, samples, center=mean)

#Check Normality
qqnorm(resid(lmphoto))
qqline(resid(lmphoto))
shapiro.test(resid(lmphoto))

#T-test
t.test(samples$Corr.Rate.O2.L.min.cm2~samples$Treatment)

