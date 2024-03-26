library(tidyverse)

rutin_anova<-read.csv("week6/comparative_rutin_data.csv")
View(rutin_anova)
str(rutin_anova)

boxplot(intercept~treatment,data=rutin_anova)

v_max<-mutate(rutin_anova,vmax=1/intercept)
View(v_max)
str(v_max)

ggplot(data=v_max)+
  geom_boxplot(mapping=aes(x=treatment,y=vmax))+
  geom_jitter(mapping=aes(x=treatment,y=vmax),height=0)+
  ylab("Vmax")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(limits=c(1,3))+
  scale_x_discrete(labels=c("2.5uM Rutin","5uM Rutin","Uninhibited"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

uninhib<-filter(v_max,treatment=="uninhibited")
mid_inhib<-filter(v_max,treatment=="2.5uM")
most_inhib<-filter(v_max,treatment=="5um")
View(most_inhib)

hist(uninhib$vmax,breaks=12)
hist(mid_inhib$vmax,breaks=12)
hist(most_inhib$vmax,breaks=12)

qqnorm(uninhib$vmax)
qqline(uninhib$vmax)

qqnorm(mid_inhib$vmax)
qqline(mid_inhib$vmax)

qqnorm(most_inhib$vmax)
qqline(most_inhib$vmax)

shapiro.test(uninhib$vmax)
shapiro.test(mid_inhib$vmax)
shapiro.test(most_inhib$vmax)

rutin1<-aov(vmax~treatment,data=v_max)
View(rutin1)

hist(resid(rutin1))

qqnorm(resid(rutin1))
qqline(resid(rutin1))

shapiro.test(resid(rutin1))

bartlett.test(vmax~treatment,data=v_max)

summary(rutin1)

TukeyHSD(rutin1)

mydata<-read.csv("week8/two_way_anova.csv")
View(mydata)

boxplot(height_cm~group_names,data=mydata)

m1<-aov(height_cm~light_tx*water_tx,data=mydata)
summary(m1)

ggplot(data=mydata)+
  geom_boxplot(mapping=aes(x=group_names,y=height_cm))+
  geom_jitter(mapping=aes(x=group_names,y=height_cm),height=0)+
  ylab("Height(cm)")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(limits=c(0,14))+
  scale_x_discrete(labels=c("Dark Drought","Dark Watered","Light Drought","Light Watered"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))
 

















