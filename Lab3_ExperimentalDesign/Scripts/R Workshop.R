my_data<-ToothGrowth

#Dependent Variable: length
#Independent Variable: Dose and supply method

#Linear Model
tooth_ANOVA<-aov(len~supp+dose+supp*dose,data=my_data) #ANOVA

#Check Normality
qqnorm(resid(tooth_ANOVA))
qqline(resid(tooth_ANOVA))

#Check Homeogeneity of Variances
boxplot(resid(tooth_ANOVA)~my_data$supp)
boxplot(resid(tooth_ANOVA)~my_data$dose)

#Summary of ANOVA
summary.ANOVA<-summary(tooth_ANOVA)

#Tukey Post-Hoc
TukeyHSD(tooth_ANOVA)

#Plot
plot.Data<- ggplot(data = my_data, aes(x=my_data$supp,my_data$dose, y=len,color=supp))+ 
  geom_boxplot() +
  ylab("Length (cm)")+ xlab("Treatments") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(aes(y = len, x = dose, fill = supp), data = my_data) + geom_boxplot()

library("ggpubr")
ggboxplot(my_data,x="dose",y="len",color="supp",
          palette=c("#00AFBB",#E78500")

