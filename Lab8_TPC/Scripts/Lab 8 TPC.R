#Lab 8#

#Load Necessary Packages
library(Rmisc)
library(lattice)
library(plyr)
library(tidyverse)
library(tidyselect)
library(car)
library(ggplot2)

#Importing Data
M08_Data <- read.csv ("Lab8_TPC/Data/M08_20190327.csv") 
M23_Data <- read.csv ("Lab8_TPC/Data/M23_20190327.csv") 
M26_Data <- read.csv ("Lab8_TPC/Data/M26_20190327.csv") 
M31_Data <- read.csv ("Lab8_TPC/Data/M31_20190327.csv") 
M39_Data <- read.csv ("Lab8_TPC/Data/M39_20190327.csv") 
M44_Data <- read.csv ("Lab8_TPC/Data/M44_20190327.csv") 
B1_Data <- read.csv ("Lab8_TPC/Data/B1_20190327.csv") 
B2_Data <- read.csv ("Lab8_TPC/Data/B2_20190327.csv") 

#Selecting Columns
select_B1<-select(B1_Data,Time,Value,Temp)
select_B2<-select(B2_Data,Time,Value,Temp)
select_M08<-select(M08_Data,Time,Value,Temp)
select_M23<-select(M23_Data,Time,Value,Temp)
select_M26<-select(M26_Data,Time,Value,Temp)
select_M31<-select(M31_Data,Time,Value,Temp)
select_M39<-select(M39_Data,Time,Value,Temp)
select_M44<-select(M44_Data,Time,Value,Temp)

#Reading time column as numeric
B1_numeric<-transform(select_B1, Time=as.numeric(Time))
B2_numeric<-transform(select_B2, Time=as.numeric(Time))
M08_numeric<-transform(select_M08, Time = as.numeric(Time))
M23_numeric<-transform(select_M23, Time = as.numeric(Time))
M26_numeric<-transform(select_M26, Time = as.numeric(Time))
M31_numeric<-transform(select_M31, Time = as.numeric(Time))
M39_numeric<-transform(select_M39, Time = as.numeric(Time))
M44_numeric<-transform(select_M44, Time = as.numeric(Time))

#Subset based on runs B1
B1_Run1<-B1_numeric[c(66:1326),c(1:2)] #8:21 through 8:42
B1_Run2<-B1_numeric[c(1663:2742),c(1:2)] #9:01 through 9:19
B1_Run3<-B1_numeric[c(2868:3948),c(1:2)] #9:29 through 9:47
B1_Run4<-B1_numeric[c(4086:5226),c(1:2)] #10:05 through 10:24
B1_Run5<-B1_numeric[c(5351:6071),c(1:2)] #10:43 through 10:55

#Subset based on runs B2
B2_Run1<-B2_numeric[c(66:1326),c(1:2)] #8:21 through 8:42
B2_Run2<-B2_numeric[c(1663:2742),c(1:2)] #9:01 through 9:19
B2_Run3<-B2_numeric[c(2868:3948),c(1:2)] #9:29 through 9:47
B2_Run4<-B2_numeric[c(4086:5226),c(1:2)] #10:05 through 10:24
B2_Run5<-B2_numeric[c(5351:6071),c(1:2)] #10:43 through 10:55

#Subset based on runs M08
M08_Run1<-M08_numeric[c(71:1331),c(1:2)] #8:21 through 8:42
M08_Run2<-M08_numeric[c(1801:2831),c(1:2)] #9:01 through 9:19
M08_Run3<-M08_numeric[c(3013:4093),c(1:2)] #9:29 through 9:47
M08_Run4<-M08_numeric[c(4239:5379),c(1:2)] #10:05 through 10:24
M08_Run5<-M08_numeric[c(5751:6223),c(1:2)] #10:43 through 10:55

#Subset based on runs M23
M23_Run1<-M23_numeric[c(66:1326),c(1:2)] #8:21 through 8:42
M23_Run2<-M23_numeric[c(1663:2742),c(1:2)] #9:01 through 9:19
M23_Run3<-M23_numeric[c(2868:3948),c(1:2)] #9:29 through 9:47
M23_Run4<-M23_numeric[c(4086:5226),c(1:2)] #10:05 through 10:24
M23_Run5<-M23_numeric[c(5351:6071),c(1:2)] #10:43 through 10:55

#Subset based on runs M26
M26_Run1<-M26_numeric[c(66:1326),c(1:2)] #8:21 through 8:42
M26_Run2<-M26_numeric[c(1663:2742),c(1:2)] #9:01 through 9:19
M26_Run3<-M26_numeric[c(2868:3948),c(1:2)] #9:29 through 9:47
M26_Run4<-M26_numeric[c(4086:5226),c(1:2)] #10:05 through 10:24
M26_Run5<-M26_numeric[c(5351:6071),c(1:2)] #10:43 through 10:55

#Subset based on runs M31
M31_Run1<-M31_numeric[c(66:1326),c(1:2)] #8:21 through 8:42
M31_Run2<-M31_numeric[c(1663:2742),c(1:2)] #9:01 through 9:19
M31_Run3<-M31_numeric[c(2868:3948),c(1:2)] #9:29 through 9:47
M31_Run4<-M31_numeric[c(4086:5226),c(1:2)] #10:05 through 10:24
M31_Run5<-M31_numeric[c(5351:6071),c(1:2)] #10:43 through 10:55

#Subset based on runs M39
M39_Run1<-M39_numeric[c(66:1326),c(1:2)] #8:21 through 8:42
M39_Run2<-M39_numeric[c(1663:2742),c(1:2)] #9:01 through 9:19
M39_Run3<-M39_numeric[c(2868:3948),c(1:2)] #9:29 through 9:47
M39_Run4<-M39_numeric[c(4086:5226),c(1:2)] #10:05 through 10:24
M39_Run5<-M39_numeric[c(5351:6071),c(1:2)] #10:43 through 10:552

#Subset based on runs M44
M44_Run1<-M44_numeric[c(66:1326),c(1:2)] #8:21 through 8:42
M44_Run2<-M44_numeric[c(1663:2742),c(1:2)] #9:01 through 9:19
M44_Run3<-M44_numeric[c(2868:3948),c(1:2)] #9:29 through 9:47
M44_Run4<-M44_numeric[c(4086:5226),c(1:2)] #10:05 through 10:24
M44_Run5<-M44_numeric[c(5351:6071),c(1:2)] #10:43 through 10:55

#Finding Rate B1
#Run 1
model_B1.1<-lm(Value~Time,data=B1_Run1)
lmsummary.B1.1<-summary(model_B1.1)
#Run 2
model_B1.2<-lm(Value~Time,data=B1_Run2)
lmsummary.B1.2<-summary(model_B1.2)
#Run 3
model_B1.3<-lm(Value~Time,data=B1_Run3)
lmsummary.B1.3<-summary(model_B1.3)
#Run 4
model_B1.4<-lm(Value~Time,data=B1_Run4)
lmsummary.B1.4<-summary(model_B1.4)
#Run 5
model_B1.5<-lm(Value~Time,data=B1_Run5)
lmsummary.B1.5<-summary(model_B1.5)

#Finding Rate B2
#Run 1
model_B2.1<-lm(Value~Time,data=B2_Run1)
lmsummary.B2.1<-summary(model_B2.1)
#Run 2
model_B2.2<-lm(Value~Time,data=B2_Run2)
lmsummary.B2.2<-summary(model_B2.2)
#Run 3
model_B2.3<-lm(Value~Time,data=B2_Run3)
lmsummary.B2.3<-summary(model_B2.3)
#Run 4
model_B2.4<-lm(Value~Time,data=B2_Run4)
lmsummary.B2.4<-summary(model_B2.4)
#Run 5
model_B2.5<-lm(Value~Time,data=B2_Run5)
lmsummary.B2.5<-summary(model_B2.5)

#Finding Rate M08
#Run 1
model_M08.1<-lm(Value~Time,data=M08_Run1)
lmsummary.M08.1<-summary(model_M08.1)
#Run 2
model_M08.2<-lm(Value~Time,data=M08_Run2)
lmsummary.M08.2<-summary(model_M08.2)
#Run 3
model_M08.3<-lm(Value~Time,data=M08_Run3)
lmsummary.M08.3<-summary(model_M08.3)
#Run 4
model_M08.4<-lm(Value~Time,data=M08_Run4)
lmsummary.M08.4<-summary(model_M08.4)
#Run 5
model_M08.5<-lm(Value~Time,data=M08_Run5)
lmsummary.M08.5<-summary(model_M08.5)

#Finding Rate M23
#Run 1
model_M23.1<-lm(Value~Time,data=M23_Run1)
lmsummary.M23.1<-summary(model_M23.1)
#Run 2
model_M23.2<-lm(Value~Time,data=M23_Run2)
lmsummary.M23.2<-summary(model_M23.2)
#Run 3
model_M23.3<-lm(Value~Time,data=M23_Run3)
lmsummary.M23.3<-summary(model_M23.3)
#Run 4
model_M23.4<-lm(Value~Time,data=M23_Run4)
lmsummary.M23.4<-summary(model_M23.4)
#Run 5
model_M23.5<-lm(Value~Time,data=M23_Run5)
lmsummary.M23.5<-summary(model_M23.5)

#Finding Rate M26
#Run 1
model_M26.1<-lm(Value~Time,data=M26_Run1)
lmsummary.M26.1<-summary(model_M26.1)
#Run 2
model_M26.2<-lm(Value~Time,data=M26_Run2)
lmsummary.M26.2<-summary(model_M26.2)
#Run 3
model_M26.3<-lm(Value~Time,data=M26_Run3)
lmsummary.M26.3<-summary(model_M26.3)
#Run 4
model_M26.4<-lm(Value~Time,data=M26_Run4)
lmsummary.M26.4<-summary(model_M26.4)
#Run 5
model_M26.5<-lm(Value~Time,data=M26_Run5)
lmsummary.M26.5<-summary(model_M26.5)

#Finding Rate M31
#Run 1
model_M31.1<-lm(Value~Time,data=M31_Run1)
lmsummary.M31.1<-summary(model_M31.1)
#Run 2
model_M31.2<-lm(Value~Time,data=M31_Run2)
lmsummary.M31.2<-summary(model_M31.2)
#Run 3
model_M31.3<-lm(Value~Time,data=M31_Run3)
lmsummary.M31.3<-summary(model_M31.3)
#Run 4
model_M31.4<-lm(Value~Time,data=M31_Run4)
lmsummary.M31.4<-summary(model_M31.4)
#Run 5
model_M31.5<-lm(Value~Time,data=M31_Run5)
lmsummary.M31.5<-summary(model_M31.5)

#Finding Rate M39
#Run 1
model_M39.1<-lm(Value~Time,data=M39_Run1)
lmsummary.M39.1<-summary(model_M39.1)
#Run 2
model_M39.2<-lm(Value~Time,data=M39_Run2)
lmsummary.M39.2<-summary(model_M39.2)
#Run 3
model_M39.3<-lm(Value~Time,data=M39_Run3)
lmsummary.M39.3<-summary(model_M39.3)
#Run 4
model_M39.4<-lm(Value~Time,data=M39_Run4)
lmsummary.M39.4<-summary(model_M39.4)
#Run 5
model_M39.5<-lm(Value~Time,data=M39_Run5)
lmsummary.M39.5<-summary(model_M39.5)

#Finding Rate M44
#Run 1
model_M44.1<-lm(Value~Time,data=M44_Run1)
lmsummary.M44.1<-summary(model_M44.1)
#Run 2
model_M44.2<-lm(Value~Time,data=M44_Run2)
lmsummary.M44.2<-summary(model_M44.2)
#Run 3
model_M44.3<-lm(Value~Time,data=M44_Run3)
lmsummary.M44.3<-summary(model_M44.3)
#Run 4
model_M44.4<-lm(Value~Time,data=M44_Run4)
lmsummary.M44.4<-summary(model_M44.4)
#Run 5
model_M44.5<-lm(Value~Time,data=M44_Run5)
lmsummary.M44.5<-summary(model_M44.5)

#Import Calculated Rates Data
Calc.Rates_Data <- read.csv ("Lab8_TPC/Data/Calculated_rates.csv")

#Filtering for blanks
blank<-Calc.Rates_Data %>%
  filter(Sample.Type =="Blank")

#Filtering by run 1
blank1<-blank %>%
  filter(Run.Number=="1")
#Average blank 1
mean.blank1<-mean(blank1$Rate.O2.L.min)

#Filtering by run 2
blank2<-blank %>%
  filter(Run.Number=="2")
#Average blank 2
mean.blank2<-mean(blank2$Rate.O2.L.min)

#Filtering by run 3
blank3<-blank %>%
  filter(Run.Number=="3")
#Average blank 3
mean.blank3<-mean(blank3$Rate.O2.L.min)

#Filtering by run 4
blank4<-blank %>%
  filter(Run.Number=="4")
#Average blank 4
mean.blank4<-mean(blank4$Rate.O2.L.min)

#Filtering by run 5
blank5<-blank %>%
  filter(Run.Number=="5")
#Average blank 5
mean.blank5<-mean(blank5$Rate.O2.L.min)

#Subtracting the average blank from the runs
#Run 1
run1<-Calc.Rates_Data %>%
  filter(Run.Number=="1")
run1$Rate.corr<-run1$Rate.O2.L.min-mean.blank1
#Run 2
run2<-Calc.Rates_Data %>%
  filter(Run.Number=="2")
run2$Rate.corr<-run2$Rate.O2.L.min-mean.blank2
#Run 3
run3<-Calc.Rates_Data %>%
  filter(Run.Number=="3")
run3$Rate.corr<-run3$Rate.O2.L.min-mean.blank3
#Run 4
run4<-Calc.Rates_Data %>%
  filter(Run.Number=="4")
run4$Rate.corr<-run4$Rate.O2.L.min-mean.blank4
#Run 5
run5<-Calc.Rates_Data %>%
  filter(Run.Number=="5")
run5$Rate.corr<-run5$Rate.O2.L.min-mean.blank5

#Rebind Data
Rebind_Data<-rbind(run1,run2,run3,run4,run5)

#Filter Samples
samples <- Rebind_Data %>% 
  filter(Sample.Type == "Sample") 

#Normalize by Weight
samples$Corr.Rate.O2.L.min.g<-samples$Rate.corr/samples$Weight.g

#Subset Cold
subset.cold<-subset(samples,Treatment=="Cold")

#Subset.Hot
subset.hot<-subset(samples,Treatment=="Hot")

#Boxplot Cold Treatment
plot.cold<- ggplot(data = subset.cold, aes(x=Temperature, y=Corr.Rate.O2.L.min.g, group=Temperature,fill=Treatment))+ 
  geom_boxplot() +
  scale_fill_manual(values="dodgerblue3")+
  ylab("Respiration Rate (umol.O2.L^(-1).min^(-1).g^(-1)")+ xlab("Temperature") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")

#Changing Ticks
plot.cold + scale_x_continuous(breaks=seq(11, 19, 2))

#Boxplot Hot Treatment
plot.hot<- ggplot(data = subset.hot, aes(x=Temperature, y=Corr.Rate.O2.L.min.g, group=Temperature,fill="red3"))+ 
  geom_boxplot() +
  ylab("Respiration Rate (umol.O2.L^(-1).min^(-1).g^(-1)")+ xlab("Temperature") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")
  
#Changing Ticks
plot.hot + scale_x_continuous(breaks=seq(11, 19, 2))

