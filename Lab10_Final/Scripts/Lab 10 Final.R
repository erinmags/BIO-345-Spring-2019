#Lab 10#

library(Rmisc)
library(lattice)
library(plyr)
library(tidyverse)
library(tidyselect)
library(car)

#Respiration
#Importing Data
B1_Data <- read.csv ("Lab10_Final/Data/B1_20190403.csv")
B2_Data <- read.csv ("Lab10_Final/Data/B2_20190403.csv")
B3_Data <- read.csv ("Lab10_Final/Data/B3_20190403.csv")
B4_Data <- read.csv ("Lab10_Final/Data/B4_20190403.csv")
M05_Data<-read.csv("Lab10_Final/Data/M05_20190403.csv")
M06_Data<-read.csv("Lab10_Final/Data/M06_20190403.csv")
M08_Data<-read.csv("Lab10_Final/Data/M08_20190403.csv")
M25_Data<-read.csv("Lab10_Final/Data/M25_20190403.csv")
M31_Data<-read.csv("Lab10_Final/Data/M31_20190403.csv")
M35_Data<-read.csv("Lab10_Final/Data/M35_20190403.csv")
M37_Data<-read.csv("Lab10_Final/Data/M37_20190403.csv")
M39_Data<-read.csv("Lab10_Final/Data/M39_20190403.csv")
M45_Data<-read.csv("Lab10_Final/Data/M45_20190403.csv")
M46_Data<-read.csv("Lab10_Final/Data/M46_20190403.csv")
M51_Data<-read.csv("Lab10_Final/Data/M51_20190403.csv")

#Selecting Columns
select_B1<-select(B1_Data,delta_t,Value)
select_B2<-select(B2_Data,delta_t,Value)
select_B3<-select(B3_Data,delta_t,Value)
select_B4<-select(B4_Data,delta_t,Value) 
select_M05<-select(M05_Data,delta_t,Value)
select_M06<-select(M06_Data,delta_t,Value)
select_M08<-select(M08_Data,delta_t,Value)
select_M25<-select(M25_Data,delta_t,Value)
select_M31<-select(M31_Data,delta_t,Value)
select_M35<-select(M35_Data,delta_t,Value)
select_M37<-select(M37_Data,delta_t,Value)
select_M39<-select(M39_Data,delta_t,Value)
select_M45<-select(M45_Data,delta_t,Value)
select_M46<-select(M46_Data,delta_t,Value)
select_M51<-select(M51_Data,delta_t,Value)

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

#Plotting M05
plot.select_M05<- ggplot(data = select_M05,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M05
lmM05 <- lm (Value ~ delta_t, data = select_M05) 
lmsummaryM05 <- summary(lmM05) 

#Plotting M06
plot.select_M06<- ggplot(data = select_M06,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M06
lmM06 <- lm (Value ~ delta_t, data = select_M06) 
lmsummaryM06 <- summary(lmM06) 

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

#Plotting M25
plot.select_M25<- ggplot(data = select_M25,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M25
lmM25 <- lm (Value ~ delta_t, data = select_M25) 
lmsummaryM25 <- summary(lmM25) 

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

#Plotting M35
plot.select_M35<- ggplot(data = select_M35,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M35
lmM35 <- lm (Value ~ delta_t, data = select_M35) 
lmsummaryM35 <- summary(lmM35) 

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

#Plotting M45
plot.select_M45<- ggplot(data = select_M45,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M45
lmM45 <- lm (Value ~ delta_t, data = select_M45) 
lmsummaryM45 <- summary(lmM45) 

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

#Plotting M51
plot.select_M51<- ggplot(data = select_M51,aes(x=delta_t,y=Value))+ 
  geom_point() +
  ylab("Oxygen Concentration (umol/L)")+ xlab("Time (min)") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model M51
lmM51 <- lm (Value ~ delta_t, data = select_M51) 
lmsummaryM51 <- summary(lmM51)

#Import Calculated Rates Data
Calc.Rates_Data <- read.csv ("Lab10_Final/Data/Calculated_rates.csv") #Importing data into R from my working directory

#Subset Hot
subset.hot<-subset(Calc.Rates_Data,Temp.Treatment=="Hot")

#Subset Cold
subset.cold<-subset(Calc.Rates_Data,Temp.Treatment=="Cold")

#Subset Hot T1
subset.hot.T1<-subset(subset.hot,Timepoint=="1")

#Subset Hot T2
subset.hot.T2<-subset(subset.hot,Timepoint=="2")

#Subset Cold T1
subset.cold.T1<-subset(subset.cold,Timepoint=="1")

#Subset Cold T2
subset.cold.T2<-subset(subset.cold,Timepoint=="2")

#Correcting for Blank Hot T1
subset.hot.T1$Rate.corr<-subset.hot.T1$Rate.O2.L.min-(-0.0807)

#Correcting for Blank Hot T2
subset.hot.T2$Rate.corr<-subset.hot.T2$Rate.O2.L.min-(-0.0628)

#Correcting for Blank Cold T1
subset.cold.T1$Rate.corr<-subset.cold.T1$Rate.O2.L.min-(-0.064125)

#Correcting for Blank Cold T2
subset.cold.T2$Rate.corr<-subset.cold.T2$Rate.O2.L.min-(-0.06)

#Rebind Data
Rebind_Data<-rbind(subset.cold.T1,subset.cold.T2,subset.hot.T1,subset.hot.T2)

#Filter Samples
samples <- Rebind_Data %>% 
  filter(Sample.Type == "Sample") 

#Normalize Corrected Rate for Weight
samples$Corr.Rate.O2.L.min<-samples$Rate.corr/samples$Weight.g 

#Absolute Rate values 
samples$abs.Corr.Rate.O2.L.min <- -(samples$Corr.Rate.O2.L.min)

#Plot Respiration Rate
plot.resp<- ggplot(data = samples,aes(x=as.factor(Timepoint),y=abs.Corr.Rate.O2.L.min,fill=Temp.Treatment))+ 
  geom_boxplot() +
  ylab("Respiration Rate (umol.O2.L.min)")+ xlab("Timepoint") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(fill="Temperature Treatment")+
  scale_fill_manual(values=c("dodgerblue3", "tomato3"))

#Linear Model
lmresp <- aov (abs.Corr.Rate.O2.L.min~Timepoint*Temp.Treatment, data = samples)
lmsummaryresp<- summary(lmresp)

#Check Normality
qqnorm(resid(lmresp))
qqline(resid(lmresp))
shapiro.test(resid(lmresp))

#Check Homogeneity of Variances Salinity
boxplot(resid(lmresp)~samples$Timepoint)

#Check Homogeneity of Variances Temperature
boxplot(resid(lmresp)~samples$Temp.Treatment)

#Protein Content
#Importing Data 
Protein_data <- read.csv ("Lab10_Final/Data/Total_Protein.csv") 

#Subset Standard
subset.standard<-subset(Protein_data,Sampe.Type=="Standard")

#Plot Standard Curve
plot.Standard<- ggplot(data = subset.standard, aes(x=Concentration, y=Absorbance))+ 
  ylab("Absorbance (nm)")+ xlab("Concentration") + 
  geom_point()+
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Regression
lmstandard <- lm (Concentration ~ Absorbance, data = subset.standard) 
lmsummary <- summary(lmstandard)

#Subset Sample
subset.sample<-subset(Protein_data,Sampe.Type=="Sample")
subset.sample$Concentration.calc <- predict(lmstandard, newdata = subset.sample) 

#Correcting for Weight
subset.sample$protein.ug.g<-(subset.sample$Absorbance*10)/subset.sample$Tissue_Mass

#Plot Respiration Rate
plot.protein<- ggplot(data = subset.sample,aes(x=as.factor(Timepoint),y=protein.ug.g,fill=Temp.Treatment))+ 
  geom_boxplot() +
  ylab("Protein Content (ug.g)")+ xlab("Timepoint") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(fill="Temperature Treatment")+
  scale_fill_manual(values=c("dodgerblue3", "tomato3"))

#Linear Model
lmprotein<- aov (protein.ug.g~Timepoint*Temp.Treatment, data = subset.sample)
lmsummarypro<- summary(lmprotein)

#Check Normality
qqnorm(resid(lmprotein))
qqline(resid(lmprotein))
shapiro.test(resid(lmprotein))

#Check Homogeneity of Variances Timepoint
boxplot(resid(lmprotein)~subset.sample$Timepoint)

#Check Homogeneity of Variances Temperature
boxplot(resid(lmprotein)~subset.sample$Temp.Treatment)
