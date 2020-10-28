### Lab 2 ###

BIO345_Lab2_Data <- read.csv("Lab2_StandardCurves/Data/BIO345_Lab2_Data.csv") #Importing Data

#Subset Standard
standard <- BIO345_Lab2_Data %>% 
  filter(Sample.Type == "Standard") 

#Plot Standard Curve #
plot.Standard<- ggplot(data = standard, aes(x=Concentration, y=Absorbance))+ 
  ylab("Absorbance (nm)")+ xlab("Concentration") + 
  geom_point()+
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lmstandard <- lm (Concentration ~ Absorbance, data = standard) #Creating Linear Model
lmsummary <- summary(lmstandard) #Summary of Model

#Subset Samples
samples <- BIO345_Lab2_Data %>% 
  filter(Sample.Type == "Sample") 
samples$Concentration.calc <- predict(lmstandard, newdata = samples) #Using model to get concentration

#Boxplot Unknowns
plot.Unknowns<- ggplot(data = samples, aes(x=Sample.ID, y=Concentration.calc))+ 
  geom_boxplot() +
  ylab("Concentration (cells/mL)")+ xlab("Unknowns") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ANOVA
model=lm(samples$Concentration.calc~samples$Sample.ID)
ANOVA=aov(model)
ANOVAsummary<-summary(ANOVA) #Summary of ANOVA

#Tukey
TUKEY<-TukeyHSD(x=ANOVA,'samples$Sample.ID',conf.level = 0.95)

#Tukey Plot
plot(TUKEY,las=1,col="blue")
     