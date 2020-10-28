#Lab 9#

library(Rmisc)
library(lattice)
library(plyr)
library(tidyverse)
library(tidyselect)
library(car)

#Importing Data
B1_Data <- read.csv ("Lab9_Respiration/Data/B1_20190403.csv") 
B2_Data <- read.csv ("Lab9_Respiration/Data/B2_20190403.csv") 
B3_Data <- read.csv ("Lab9_Respiration/Data/B3_20190403.csv") 
B4_Data <- read.csv ("Lab9_Respiration/Data/B4_20190403.csv") 
M01_Data<-read.csv ("Lab9_Respiration/Data/M01_20190403.csv")
M07_Data<-read.csv ("Lab9_Respiration/Data/M07_20190403.csv")
M08_Data<-read.csv ("Lab9_Respiration/Data/M08_20190403.csv")
M30_Data<-read.csv ("Lab9_Respiration/Data/M30_20190403.csv")
M31_Data<-read.csv ("Lab9_Respiration/Data/M31_20190403.csv")
M36_Data<-read.csv ("Lab9_Respiration/Data/M36_20190403.csv")
M37_Data<-read.csv ("Lab9_Respiration/Data/M37_20190403.csv")
M39_Data<-read.csv ("Lab9_Respiration/Data/M39_20190403.csv")
M44_Data<-read.csv ("Lab9_Respiration/Data/M44_20190403.csv")
M46_Data<-read.csv ("Lab9_Respiration/Data/M46_20190403.csv")
M49_Data<-read.csv ("Lab9_Respiration/Data/M49_20190403.csv")

#Selecting Columns
select_B1<-select(B1_Data,delta_t,Value)
select_B2<-select(B2_Data,delta_t,Value)
select_B3<-select(B3_Data,delta_t,Value)
select_B4<-select(B4_Data,delta_t,Value)
select_M01<-select(M01_Data,delta_t,Value)
select_M07<-select(M07_Data,delta_t,Value)
select_M08<-select(M08_Data,delta_t,Value)
select_M30<-select(M30_Data,delta_t,Value)
select_M31<-select(M31_Data,delta_t,Value)
select_M36<-select(M36_Data,delta_t,Value)
select_M37<-select(M37_Data,delta_t,Value)
select_M39<-select(M39_Data,delta_t,Value)
select_M44<-select(M44_Data,delta_t,Value)
select_M46<-select(M46_Data,delta_t,Value)
select_M49<-select(M49_Data,delta_t,Value)

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

#Plotting B3
plot.select_B3<- ggplot(data = select_B3,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model B3
lmB3 <- lm (Value ~ delta_t, data = select_B3) 
lmsummaryB3 <- summary(lmB3) 

#Plotting B4
plot.select_B4<- ggplot(data = select_B4,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model B4
lmB4 <- lm (Value ~ delta_t, data = select_B4) 
lmsummaryB4 <- summary(lmB4) 

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

#Plotting M08
plot.select_M08<- ggplot(data = select_M08,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M08
lmM08 <- lm (Value ~ delta_t, data = select_M08) 
lmsummaryM08 <- summary(lmM08) 

#Plotting M30
plot.select_M30<- ggplot(data = select_M30,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M30
lmM30 <- lm (Value ~ delta_t, data = select_M30) 
lmsummaryM30 <- summary(lmM30) 

#Plotting M31
plot.select_M31<- ggplot(data = select_M31,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M31
lmM31 <- lm (Value ~ delta_t, data = select_M31) 
lmsummaryM31 <- summary(lmM31) 

#Plotting M36
plot.select_M36<- ggplot(data = select_M36,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M36
lmM36 <- lm (Value ~ delta_t, data = select_M36) 
lmsummaryM36 <- summary(lmM36) 

#Plotting M37
plot.select_M37<- ggplot(data = select_M37,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M37
lmM37 <- lm (Value ~ delta_t, data = select_M37) 
lmsummaryM37 <- summary(lmM37) 

#Plotting M39
plot.select_M39<- ggplot(data = select_M39,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M39
lmM39 <- lm (Value ~ delta_t, data = select_M39) 
lmsummaryM39 <- summary(lmM39) 

#Plotting M44
plot.select_M44<- ggplot(data = select_M44,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M44
lmM44 <- lm (Value ~ delta_t, data = select_M44) 
lmsummaryM44 <- summary(lmM44) 

#Plotting M46
plot.select_M46<- ggplot(data = select_M46,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M46
lmM46 <- lm (Value ~ delta_t, data = select_M46) 
lmsummaryM46 <- summary(lmM46) 

#Plotting M49
plot.select_M49<- ggplot(data = select_M49,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M49
lmM49 <- lm (Value ~ delta_t, data = select_M49) 
lmsummaryM49 <- summary(lmM49) 

#Import Calculated Rates Data
Calc.Rates_Data <- read.csv ("Lab9_Respiration/Data/Calculated_rates.csv") #Importing data into R from my working directory

#Subset Hot
subset.hot<-subset(Calc.Rates_Data,Temp.Treatment=="Hot")

#Subset Cold
subset.cold<-subset(Calc.Rates_Data,Temp.Treatment=="Cold")

#Subset Cold Low
subset.cold.low<-subset(subset.cold,Salinity.Treatment=="Low")

#Subset Cold Ambient
subset.cold.ambient<-subset(subset.cold,Salinity.Treatment=="Ambient")

#Subset Hot Low
subset.hot.low<-subset(subset.hot,Salinity.Treatment=="Low")

#Subset Hot Ambient
subset.hot.ambient<-subset(subset.hot,Salinity.Treatment=="Ambient")

#Correcting for Blank Hot and Low
subset.hot.low$Rate.corr<-subset.hot.low$Rate.O2.L.min-(-0.06)

#Correcting for Blank Hot and Ambient
subset.hot.ambient$Rate.corr<-subset.hot.ambient$Rate.O2.L.min-(-0.0807)

#Correcting for Blank Cold and Low
subset.cold.low$Rate.corr<-subset.cold.low$Rate.O2.L.min-(-0.0628)

#Correcting for Blank Cold and Ambient
subset.cold.ambient$Rate.corr<-subset.cold.ambient$Rate.O2.L.min-(-0.064125)

#Rebind Data
Rebind_Data<-rbind(subset.cold.low,subset.cold.ambient,subset.hot.low,subset.hot.ambient)

#Filter Samples
samples <- Rebind_Data %>% 
  filter(Sample.Type == "Sample") 

#Normalize Corrected Rate for Weight
samples$Corr.Rate.O2.L.min<-samples$Rate.O2.L.min/samples$Weight.g 

#Plot Respiration Rate
plot.resp<- ggplot(data = samples,aes(x=Salinity.Treatment,y=Corr.Rate.O2.L.min,fill=Temp.Treatment))+ 
  geom_boxplot() +
  ylab("Respiration Rate (umol.O2.L.min)")+ xlab("Salinity Treatment") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(fill="Temperature Treatment")+
  scale_fill_manual(values=c("dodgerblue3", "tomato3"))
  
#Linear Model
lmresp <- aov (Corr.Rate.O2.L.min~Salinity.Treatment*Temp.Treatment, data = samples)
lmsummary<- summary(lmresp)

#Check Normality
qqnorm(resid(lmresp))
qqline(resid(lmresp))
shapiro.test(resid(lmresp))

#Check Homogeneity of Variances Salinity
boxplot(resid(lmresp)~samples$Salinity.Treatment)
leveneTest(resid(lmresp) ~ samples$Salinity.Treatment, samples, center=mean)

#Check Homogeneity of Variances Temperature
boxplot(resid(lmresp)~samples$Temp.Treatment)
leveneTest(resid(lmresp) ~ samples$Temp.Treatment, samples, center=mean)
