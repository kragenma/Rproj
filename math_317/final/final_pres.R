library(tidyverse)
library(RColorBrewer)

assay<-read.csv("final/final_rutin.csv")
View(assay)

r1<-lm(inverse_rate~inverse_.s.,data=assay)
View(resid(r1))

r2<-resid(r1)

hist(r2)

qqnorm(r2)
qqline(r2)

shapiro.test(r2)

plot(r2~assay$inverse_.s.,ylab="Residuals",xlab="1/[S]",main="Residuals plot")
abline(h=0,lty=2)

assay$group<-factor(assay$group,c("most_inhib","mid_inhib","no_inhib"))

ggplot(data=assay)+
  scale_color_brewer(name="Treatment",labels=c("5uM","2.5uM","Uninhibited"),palette="Set2")+
  scale_shape_discrete(name="Treatment",labels=c("5uM","2.5uM","Uninhibited"))+
  geom_point(mapping=aes(x=inverse_.s.,y=inverse_rate,color=group,pch=group),size=3)+
  geom_smooth(mapping=aes(x=inverse_.s.,y=inverse_rate,group=group,color=group),method="lm", se=FALSE,size=.5,fullrange=TRUE)+
  ylab("1/Rate (sec/uM NADH)")+
  xlab("1/[S] (1/uM 4:1 NAC)")+
  theme_classic()+
  scale_x_continuous(limits=c(0,.0011),expand=c(0,0))+
  scale_y_continuous(limits=c(0,5.1),expand=c(0,0))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.title=element_text(size=20))+
  theme(legend.text=element_text(size=20))


comparative_rutin_5um<-read.csv("week6/comparative_rutin_data.csv")
View(comparative_rutin_5um)

#add vmax
v_max<-mutate(comparative_rutin_5um,vmax=1/intercept)
View(v_max)

vmax_data2<-group_by(v_max,treatment)
vmax_data3<-summarize(vmax_data2,vmax_mean=mean(vmax),
                      vmax_sd=sd(vmax),
                      vmax_n1=n(),vmax_n2=sum(!is.na(vmax)))
View(vmax_data3)

vmax_summary<-mutate(vmax_data3,vmax_se=vmax_sd/sqrt(vmax_n2))
View(vmax_summary)

#barplot
ggplot(data=vmax_summary)+
  scale_fill_brewer(name="Treatment",labels=c("2.5uM","5uM","Uninhibited"),palette="Set2")+
  geom_col(mapping=aes(x=treatment,y=vmax_mean,fill=treatment),alpha=0.5)+
  geom_errorbar(aes(x=treatment,y=vmax_mean,ymin=vmax_mean-vmax_se,ymax=vmax_mean+vmax_se),width=.02,color="black",linetype="solid")+
  ylab("Vmax (mean)")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(limits=c(0,2.9),breaks=c(0,.4,.8,1.2,1.6,2,2.4,2.8),expand=c(0,0))+
  scale_x_discrete(labels=c("2.5uM","5uM","Uninhibited"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.title=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

#histograms
uninhib<-filter(v_max,treatment=="uninhibited")
treatment_5um<-filter(v_max,treatment=="5um")
treatment_2.5um<-filter(v_max,treatment=="2.5uM")
View(treatment_2.5um)

hist(uninhib$vmax,breaks=6)
hist(treatment_5um$vmax,breaks=6)
hist(treatment_2.5um$vmax,breaks=6)

qqnorm(uninhib$vmax)
qqline(uninhib$vmax)

qqnorm(treatment_5um$vmax)
qqline(treatment_5um$vmax)

qqnorm(treatment_2.5um$vmax)
qqline(treatment_2.5um$vmax)

shapiro.test(uninhib$vmax)
shapiro.test(treatment_5um$vmax)
shapiro.test(treatment_2.5um$vmax)

mymodel<-lm(vmax~treatment,data=v_max)
View(resid(mymodel))

hist(resid(mymodel))

qqnorm(resid(mymodel))
qqline(resid(mymodel))

shapiro.test(resid(mymodel))

bartlett.test(vmax~treatment,data=v_max)

v1<-aov(vmax~treatment,data=v_max)
View(v1) 

summary.aov(v1)











