library(tidyverse)
prec<-read.csv("week9/homework_8a.csv")
View(prec)

boxplot(rain_in~city,data=prec)

pasadena_prec<-filter(prec,city=="pasadena")
lax_prec<-filter(prec,city=="lax")

shapiro.test(pasadena_prec$rain_in)
shapiro.test(lax_prec$rain_in)

log_pasadena<-mutate(pasadena_prec,log_pasrain=log(rain_in))
View(log_pasadena)

hist(log_pasadena$log_pasrain,breaks=20)

qqnorm(log_pasadena$log_pasrain)
qqline(log_pasadena$log_pasrain)

shapiro.test(log_pasadena$log_pasrain)

log_lax<-mutate(lax_prec,log_laxrain=log(rain_in))
View(log_lax)

hist(log_lax$log_laxrain,breaks=20)

qqnorm(log_lax$log_laxrain)
qqline(log_lax$log_laxrain)

shapiro.test(log_lax$log_laxrain)

log_prec<-mutate(prec,log_rain=log(rain_in))
View(log_prec)

prec_resid<-lm(log_rain~city,data=log_prec)
View(resid(prec_resid))

hist(resid(prec_resid))

qqnorm(resid(prec_resid))
qqline(resid(prec_resid))

shapiro.test(resid(prec_resid))

bartlett.test(log_rain~city,data=log_prec)

t.test(log_rain~city,data=log_prec,var.equal=TRUE)

ggplot(data=prec)+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  geom_boxplot(mapping=aes(x=city,y=rain_in,fill=city),alpha=0.5)+
  geom_jitter(aes(x=city,y=rain_in,color=city),height=0)+
  ylab("Rain (inches)")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(limits=c(0,60))+
  scale_x_discrete(labels=c("Pasadena","LAX"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

plant_water<-read.csv("week9/homework_8b.csv")
View(plant_water)

plot(height_cm~ml_day,data=plant_water)

hist(plant_water$ml_day)
hist(plant_water$height_cm)

qqnorm(plant_water$ml_day)
qqline(plant_water$ml_day)

qqnorm(plant_water$height_cm)
qqline(plant_water$height_cm)

shapiro.test(plant_water$ml_day)
shapiro.test(plant_water$height_cm)

cor.test(plant_water$ml_day,plant_water$height_cm)

plant1<-lm(height_cm~ml_day,data=plant_water)
View(resid(plant1))

p1<-resid(plant1)

hist(p1)

qqnorm(p1)
qqline(p1)

shapiro.test(p1)

plot(p1~plant_water$ml_day,ylab="Residuals",xlab="Water (mL/day)",main="Residuals plot")
abline(h=0,lty=2)

ggplot(data=plant_water)+
  geom_point(mapping=aes(x=ml_day,y=height_cm))+
  geom_smooth(mapping=aes(x=ml_day,y=height_cm),method="lm", se=FALSE,color="black")+
  ylab("Height (cm)")+
  xlab("Water (mL/day)")+
  theme_classic()+
  scale_x_continuous(limits=c(0,200))+
  scale_y_continuous(limits=c(0,100))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

beavers<-read.csv("week9/homework_8c.csv")
View(beavers)

boxplot(length_cm~city,data=beavers)

shapiro.test(malibu_beavs$length_cm)
shapiro.test(london_malibu$length_cm)
shapiro.test(ba$length_cm)
shapiro.test(heidelber$length_cm)

log_beavs<-mutate(beavers,log_hair=log(length_cm))
View(log_beavs)

malibu_beavs<-filter(log_beavs,city=="malibu")
london_beavs<-filter(log_beavs,city=="london")
ba_beavs<-filter(log_beavs,city=="buenos_aires")
heidelberg_beavs<-filter(log_beavs,city=="heidelberg")

hist(malibu_beavs$length_cm,breaks=20)
hist(london_beavs$length_cm,breaks=20)
hist(ba_beavs$length_cm,breaks=20)
hist(heidelberg_beavs$length_cm,breaks=20)

qqnorm(malibu_beavs$length_cm)
qqline(malibu_beavs$length_cm)

qqnorm(london_beavs$length_cm)
qqline(london_beavs$length_cm)

qqnorm(ba_beavs$length_cm)
qqline(ba_beavs$length_cm)

qqnorm(heidelberg_beavs$length_cm)
qqline(heidelberg_beavs$length_cm)

shapiro.test(malibu_beavs$length_cm)
shapiro.test(london_beavs$length_cm)
shapiro.test(ba_beavs$length_cm)
shapiro.test(heidelberg_beavs$length_cm)
  
bartlett.test(log_hair~city,data=log_beavs)

beav1<-aov(log_hair~city,data=log_beavs)
View(beav1) 

beav2<-lm(log_hair~city,data=log_beavs)
View(resid(beav2))

b1<-resid(beav2)

hist(b1)

qqnorm(b1)
qqline(b1)

shapiro.test(b1)

summary(beav1)

TukeyHSD(beav1)

ggplot(data=beavers)+
  geom_boxplot(mapping=aes(x=city,y=length_cm))+
  geom_jitter(mapping=aes(x=city,y=length_cm),height=0)+
  ylab("Fur Length (cm)")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(limits=c(0,8))+
  scale_x_discrete(labels=c("Buenos Aires","Heidelberg", "London", "Malibu"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))
  







