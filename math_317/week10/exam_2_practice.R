library(tidyverse)
library(RColorBrewer)

hands<-read.csv("week10/practice_hand_data.csv")
View(hands)

lefty<-filter(hands,hand=="left")
righty<-filter(hands,hand=="right")
View(lefty)

hist(lefty$size_cm,breaks=20)
hist(righty$size_cm,breaks=20)

qqnorm(lefty$size_cm)
qqline(lefty$size_cm)

qqnorm(righty$size_cm)
qqline(righty$size_cm)

shapiro.test(lefty$size_cm)
shapiro.test(righty$size_cm)

bartlett.test(size_cm~hand,data=hands)

hands1<-aov(size_cm~hand,data=hands)
View(hands1) 
summary(hands1)

ggplot(data=hands)+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  geom_boxplot(mapping=aes(x=hand,y=size_cm,fill=hand),alpha=0.5)+
  geom_jitter(aes(x=hand,y=size_cm,color=hand),height=0)+
  ylab("Hand Size (cm)")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(limits=c(5,20))+
  scale_x_discrete(labels=c("Left","Right"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

#leaves
leaves<-read.csv("week10/practice_leaf_bivariate.csv")
View(leaves)

plot(~leaf_length_cm,data=leaves)

leaf_width<-filter(leaves,leaf=="width")
leaf_length<-filter(leaves,leaf=="length")

hist(leaf_width$measurement_cm,breaks=10)
hist(leaf_length$measurement_cm,breaks=10)

qqnorm(leaf_width$measurement_cm)
qqline(leaf_width$measurement_cm)

qqnorm(leaf_length$measurement_cm)
qqline(leaf_length$measurement_cm)

shapiro.test(leaf_width$leaf_width_cm)
shapiro.test(leaf_length$leaf_length_cm)

leaf_model<-lm(measurment_cm~leaf,data=leaves)
View(resid(leaf_model))
summary(resid(leaf_model))

hist(resid(leaf_model))

qqnorm(resid(leaf_model))
qqline(resid(leaf_model))

shapiro.test(resid(leaf_model))

leaf1<-resid(leaf_model)

hist(leaf1)

qqnorm(leaf1)
qqline(leaf1)

shapiro.test(leaf1)

plot(leaf1~leaf_width$measurement_cm,ylab="Residuals",xlab="Leaf Width(cm)",main="Residuals plot")
abline(h=0,lty=2)

cor.test(leaf_length$measurement_cm,leaf_width$measurement_cm)

leaf3<-read.csv("week10/practice_leaf_bivar.csv")
View(leaf3)

ggplot(data=leaf3)+
  geom_point(mapping=aes(x=leaf_width_cm,y=leaf_length_cm))+
  geom_smooth(mapping=aes(x=leaf_width_cm,y=leaf_length_cm),method="lm", se=FALSE,color="black")+
  ylab("Length(cm)")+
  xlab("Width(cm)")+
  theme_classic()+
  scale_x_continuous(limits=c(3,7))+
  scale_y_continuous(limits=c(0,15))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))










