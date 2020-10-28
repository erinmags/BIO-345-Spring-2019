#Lab 5#

#Importing Data 
Coral_data <- read.csv ("Lab5_CoralStress/Data/Coral_Stress.csv") #Importing data into R from my working directory

#Hypothesis - CO2 effect treatment affect on survival in m.cap

#Subset M.capitata
subset.m.capitata<-subset(Coral_data,Species=="M. capitata")

#Plot Survival CO2
plot.survival<- ggplot(data = subset.m.capitata,aes(x=CO2_Treatment,y=Survival_days,fill=Temp_Treatment))+ 
  geom_boxplot() +
  ylab("Days")+ xlab("CO2 Treatment") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(fill="Temperature Treatment")

#ANOVA
days.aov <- aov(Survival_days ~ Temp_Treatment+CO2_Treatment, data = subset.m.capitata)

#Summary of ANOVA
summary(days.aov)

#Plot Scatter
plot.scatter<- ggplot(data = subset.m.capitata,aes(x=Bleaching_Score,y=Survival_days,color=Treatment))+ 
  geom_point() +
  ylab("Survival Days")+ xlab("Bleaching") + 
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Changing Ticks
plot.scatter + scale_x_continuous(breaks=seq(-80, 10, 5))

#Modifying Legend
plot.scatter + scale_color_manual(name="State", 
                        labels = c("Ambient Temp, Ambient CO2", 
                                   "Ambient Temp, High CO2", 
                                   "High Temp, Ambient CO2 ", 
                                   "High Temp, High CO2"),
                        values = c("ATAC"="deepskyblue", 
                                   "ATHC"="blueviolet", 
                                   "HTAC"="darkolivegreen4", 
                                   "HTHC"="hotpink1"))
                                
                              
#Subset ATAC
subset.ATAC<-subset(Coral_data,Treatment=="ATAC")

#Linear Regression ATAC
lmATAC <- lm (Bleaching_Score ~ Survival_days, data = subset.ATAC) #Creating Linear Model
lmsummary1 <- summary(lmATAC) #Summary of Model

#Subset ATHC
subset.ATHC<-subset(Coral_data,Treatment=="ATHC")

#Linear Regression ATHC
lmATHC <- lm (Bleaching_Score ~ Survival_days, data = subset.ATHC) #Creating Linear Model
lmsummary2 <- summary(lmATHC) #Summary of Model

#Subset HTAC
subset.HTAC<-subset(Coral_data,Treatment=="HTAC")

#Linear Regression HTAC
lmHTAC <- lm (Bleaching_Score ~ Survival_days, data = subset.HTAC) #Creating Linear Model
lmsummary3 <- summary(lmHTAC) #Summary of Model

#Subset HTHC
subset.HTHC<-subset(Coral_data,Treatment=="HTHC")

#Linear Regression HTHC
lmHTHC <- lm (Bleaching_Score ~ Survival_days, data = subset.HTHC) #Creating Linear Model
lmsummary4 <- summary(lmHTHC) #Summary of Model

#Tukey
TukeyHSD(days.aov)
