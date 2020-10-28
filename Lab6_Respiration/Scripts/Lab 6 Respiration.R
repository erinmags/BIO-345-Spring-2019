#Lab 6#

library(Rmisc)
library(lattice)
library(plyr)
library(tidyverse)
library(tidyselect)
library(car)

#Importing Data
B1_Data <- read.csv ("Lab6_Respiration/Data/B1_20190306.csv") #Importing data into R from my working directory
B2_Data<-read.csv ("Lab6_Respiration/Data/B2_20190306.csv")
M01_Data<-read.csv ("Lab6_Respiration/Data/M01_20190306.csv")
M07_Data<-read.csv("Lab6_Respiration/Data/M07_20190306.csv")
M09_Data<-read.csv("Lab6_Respiration/Data/M09_20190306.csv")
M11_Data<-read.csv("Lab6_Respiration/Data/M11_20190306.csv")
M20_Data<-read.csv("Lab6_Respiration/Data/M20_20190306.csv")
M23_Data<-read.csv("Lab6_Respiration/Data/M23_20190306.csv")

#Selecting Columns
select_B1<-select(B1_Data,delta_t,Value)
select_B2<-select(B2_Data,delta_t,Value)
select_M01<-select(M01_Data,delta_t,Value)
select_M07<-select(M07_Data,delta_t,Value)
select_M09<-select(M09_Data,delta_t,Value)
select_M11<-select(M11_Data,delta_t,Value)
select_M20<-select(M20_Data,delta_t,Value)
select_M23<-select(M23_Data,delta_t,Value)

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

#Plotting M01
plot.select_M01<- ggplot(data = select_M01,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M01
lmM01 <- lm (Value ~ delta_t, data = select_M01) 
lmsummaryM01 <- summary(lmM01) 

#Plotting M07
plot.select_M07<- ggplot(data = select_M07,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M07
lmM07 <- lm (Value ~ delta_t, data = select_M07) 
lmsummaryM07 <- summary(lmM07) 

#Plotting M09
plot.select_M09<- ggplot(data = select_M09,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M09
lmM09 <- lm (Value ~ delta_t, data = select_M09) 
lmsummaryM09 <- summary(lmM09)  

#Plotting M11
plot.select_M11<- ggplot(data = select_M11,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M11
lmM11 <- lm (Value ~ delta_t, data = select_M11) 
lmsummaryM11 <- summary(lmM11)  

#Plotting M20
plot.select_M20<- ggplot(data = select_M20,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M20
lmM20 <- lm (Value ~ delta_t, data = select_M20) 
lmsummaryM20 <- summary(lmM20) 

#Plotting M23
plot.select_M23<- ggplot(data = select_M23,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M23
lmM23 <- lm (Value ~ delta_t, data = select_M23) 
lmsummaryM23 <- summary(lmM23) 

#Import Calculated Rates Data
Calc.Rates_Data <- read.csv ("Lab6_Respiration/Data/Calculated_rates.csv") #Importing data into R from my working directory

#Subset Hot
subset.hot<-subset(Calc.Rates_Data,Treatment=="Hot")

#Subset Cold
subset.cold<-subset(Calc.Rates_Data,Treatment=="Cold")

#Correcting for Blank Hot
subset.hot$Rate.corr<-subset.hot$Rate.O2.L.min-(-0.118140)

#Correcting for Blank Cold
subset.cold$Rate.corr<-subset.cold$Rate.O2.L.min-(-0.023500)

#Rebind Data
Rebind_Data<-rbind(subset.cold,subset.hot)

#Filter Samples
samples <- Rebind_Data %>% 
  filter(Sample.Type == "Sample") 

#Normalize Corrected Rate for Weight
samples$Corr.Rate.O2.L.min<-samples$Rate.O2.L.min-samples$Weight.g 

#Boxplot Treatment
plot.resp<- ggplot(data = samples, aes(x=Treatment, y=Corr.Rate.O2.L.min))+ 
  geom_boxplot() +
  ylab("Respiration Rate (umolO2L^-1min^-1g^-1)")+ xlab("Tank") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model
lmresp <- lm (Corr.Rate.O2.L.min~Treatment, data = samples) #Creating Linear Model
lmsummaryresp <- summary(lmresp)

#Check Homogeneity of Variances
boxplot(resid(lmresp)~samples$Treatment)
leveneTest(resid(lmresp) ~ samples$Treatment, samples, center=mean)

#Check Normality
qqnorm(resid(lmresp))
qqline(resid(lmresp))
shapiro.test(resid(lmresp))

#Mann-U Whitney Test
wilcox.test(samples$Rate.O2.L.min ~ samples$Treatment)


