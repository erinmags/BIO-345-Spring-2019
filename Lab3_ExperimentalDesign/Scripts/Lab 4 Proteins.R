#Lab 4#

#Importing Data 
Protein_data <- read.csv ("Lab4_ProteinContent/Data/Mussel_Protein_1.csv") #Importing data into R from my working directory

#Subset Blank
subset.blank<-subset(Protein_data,Type=="Blank")

#Mean Blanks
mean.blank<-mean(subset.blank$Absorbance)

#Correcting for Blank
Protein_data$Abs.corr<-Protein_data$Absorbance-mean.blank

#Subset Standard
subset.standard<-subset(Protein_data,Type=="Standard")

#Plot Standard Curve
plot.Standard<- ggplot(data = subset.standard, aes(x=Concentration, y=Abs.corr))+ 
  ylab("Absorbance (nm)")+ xlab("Concentration") + 
  geom_point()+
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Regression
lmstandard <- lm (Concentration ~ Abs.corr, data = subset.standard) #Creating Linear Model
lmsummary <- summary(lmstandard) #Summary of Model

#Subset Sample
subset.sample<-subset(Protein_data,Type=="Sample")
subset.sample$Concentration.calc <- predict(lmstandard, newdata = subset.sample) 

#Correcting for Weight
subset.sample$Weight.mussel<-subset.sample$Weight_Tube_Mussel-subset.sample$Weight_Tube #Finding weight of mussel
subset.sample$protein.ug.g<-(subset.sample$Weight_Tube_Mussel_Water*10)/subset.sample$Weight.mussel #Finding final weight

#Boxplot Protein
plot.protein<- ggplot(data = subset.sample, aes(x=Tank, y=protein.ug.g))+ 
  geom_boxplot() +
  ylab("Protein Concentration (μg/g)")+ xlab("Tank") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Linear Model
lmprotein <- lm (protein.ug.g~Tank, data = subset.sample) #Creating Linear Model
lmsummary2 <- summary(lmprotein)

#Check Homogeneity of Variances
boxplot(resid(lmprotein)~subset.sample$Tank)

#Check Normality
qqnorm(resid(lmprotein))
qqline(resid(lmprotein))

#Mann-Whitney U
wilcox.test(subset.sample$protein.ug.g ~ subset.sample$Tank)