#Lab 3#

#Importing Data For Mussle ID
Mussels.Data <- read.csv ("Lab3_ExperimentalDesign/Data/Master_Mussel_Data_Spring2020.csv") #Importing data into R from my working directory

#T-test Width
t.test(Mussels.Data$Width_cm ~ Mussels.Data$Tank) #t-test

#Boxplot Width
plot.Width<- ggplot(data = Mussels.Data, aes(x=Tank, y=Width_cm))+ 
  geom_boxplot() +
  ylab("Width (cm)")+ xlab("Tanks") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#T-test Length
t.test(Mussels.Data$Length_cm~Mussels.Data$Tank)

#Boxplot Length
plot.Length<- ggplot(data = Mussels.Data, aes(x=Tank, y=Length_cm))+ 
  geom_boxplot() +
  ylab("Length (cm)")+ xlab("Tanks") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#T-test Weight
t.test(Mussels.Data$Weight_g~Mussels.Data$Tank)

#Boxplot Weight
plot.Weight<- ggplot(data = Mussels.Data, aes(x=Tank, y=Weight_g))+ 
  geom_boxplot() +
  ylab("Weight (g)")+ xlab("Tanks") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

