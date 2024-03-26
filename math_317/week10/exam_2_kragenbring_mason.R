library(tidyverse)
library(RColorBrewer)

lizards<-read.csv("week10/kragenbring_lizards.csv")
View(lizards)

rearr_lizards<-read.csv("week10/lizards_rearranged.csv")
View(rearr_lizards)

plot(leg_length~tail_length,data=lizards)

lizard_leg<-filter(rearr_lizards,appendage=="leg")
lizard_tail<-filter(rearr_lizards,appendage=="tail")

hist(lizard_leg$length,breaks=10)
hist(lizard_tail$length,breaks=10)

qqnorm(lizard_leg$length)
qqline(lizard_leg$length)

qqnorm(lizard_tail$length)
qqline(lizard_tail$length)

shapiro.test(lizard_leg$length)
shapiro.test(lizard_tail$length)

log_liz<-mutate(lizards,log_leg_length=log(leg_length))
View(log_liz)

hist(log_liz$log_leg_length,breaks=10)

qqnorm(log_liz$log_leg_length)
qqline(log_liz$log_leg_length)

shapiro.test(log_liz$log_leg_length)

liz_model<-lm(tail_length~log_leg_length,data=log_liz)
liz1<-resid(liz_model)
summary(liz1)

hist(liz1)

qqnorm(liz1)
qqline(liz1)

shapiro.test(liz1)

plot(liz1~log_liz$log_leg_length,ylab="Residuals",xlab="Leg Length",main="Residuals plot")
abline(h=0,lty=2)

cor.test(lizards$leg_length,lizards$tail_length)

ggplot(data=lizards)+
  geom_point(mapping=aes(x=leg_length,y=tail_length))+
  geom_smooth(mapping=aes(x=leg_length,y=tail_length),method="lm", se=FALSE,color="black")+
  ylab("Tail Length")+
  xlab("Leg Length")+
  theme_classic()+
  scale_x_continuous(limits=c(0,40))+
  scale_y_continuous(limits=c(0,35))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

mammals<-read.csv("week10/kragenbring_mammals.csv")
View(mammals)

mountain_lion<-filter(mammals,species=="mountain lion")
coyotes<-filter(mammals,species=="coyote")
View(coyotes)

hist(mountain_lion$mass_kg,breaks=10)
hist(coyotes$mass_kg,breaks=10)

qqnorm(mountain_lion$mass_kg)
qqline(mountain_lion$mass_kg)

qqnorm(coyotes$mass_kg)
qqline(coyotes$mass_kg)

shapiro.test(coyotes$mass_kg)
shapiro.test(mountain_lion$mass_kg)

mammals_summary1<-group_by(mammals,species)
mammals_summary2<-summarize(mammals_summary1,weight_mean=mean(mass_kg),
                      weight_sd=sd(mass_kg),
                      weight_n1=n(),weight_n2=sum(!is.na(mass_kg)))
View(mammals_summary2)

mam_summary<-mutate(mammals_summary2,weight_se=weight_sd/sqrt(weight_n2))
View(mam_summary)

t.test(mass_kg~species,data=mammals,var.equal=TRUE)

ggplot(data=mam_summary)+
  geom_col(mapping=aes(x=species,y=weight_mean),color="black",fill="aliceblue")+
  geom_errorbar(aes(x=species,y=weight_mean,ymin=weight_mean-weight_se,ymax=weight_mean+weight_se),width=.2,color="black",linetype="solid")+
  ylab("Mammal Mass(kg)")+
  xlab("Species")+
  theme_classic()+
  scale_y_continuous(limits=c(0,45))+
  scale_x_discrete(labels=c("Coyote","Mountain Lion"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

rain<-read.csv("week10/kragenbring_precipitation.csv")
View(rain)

boxplot(rain_in~city,data=rain)

azkaban_prec<-filter(rain,city=="azkaban")
hogwarts_prec<-filter(rain,city=="hogwarts")
london_prec<-filter(rain,city=="london")

shapiro.test(azkaban_prec$rain_in)
shapiro.test(hogwarts_prec$rain_in)
shapiro.test(london_prec$rain_in)

log_az<-mutate(azkaban_prec,log_azrain=log(rain_in))
log_hog<-mutate(hogwarts_prec,log_hograin=log(rain_in))
log_london<-mutate(london_prec,log_londrain=log(rain_in))

hist(log_az$log_azrain,breaks=10)

qqnorm(log_az$log_azrain)
qqline(log_az$log_azrain)

shapiro.test(log_az$log_azrain)

hist(log_london$log_londrain,breaks=10)

qqnorm(log_london$log_londrain)
qqline(log_london$log_londrain)

shapiro.test(log_london$log_londrain)

hist(log_hog$log_hograin,breaks=10)

qqnorm(log_hog$log_hograin)
qqline(log_hog$log_hograin)

shapiro.test(log_hog$log_hograin)

log_rain<-mutate(rain,rain_log=log(rain_in))
View(log_rain)

rain_resid<-lm(rain_log~city,data=log_rain)
View(resid(rain_resid))

hist(resid(rain_resid))

qqnorm(resid(rain_resid))
qqline(resid(rain_resid))

shapiro.test(resid(rain_resid))

bartlett.test(rain_log~city,data=log_rain)

rain1<-aov(rain_log~city,data=log_rain)
View(rain1) 
summary.aov(rain1)

TukeyHSD(rain1)

ggplot(data=rain)+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  geom_boxplot(mapping=aes(x=city,y=rain_in,fill=city),alpha=0.5)+
  geom_jitter(aes(x=city,y=rain_in,color=city),height=0)+
  ylab("Rain (inches)")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(limits=c(0,60))+
  scale_x_discrete(labels=c("Azkaban","Hogwarts","London"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))














