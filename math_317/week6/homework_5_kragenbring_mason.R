library(tidyverse)
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
  geom_col(mapping=aes(x=treatment,y=vmax_mean),color="black",fill="aliceblue")+
  geom_errorbar(aes(x=treatment,y=vmax_mean,ymin=vmax_mean-vmax_se,ymax=vmax_mean+vmax_se),width=.02,color="black",linetype="solid")+
  ylab("Vmax (mean)")+
  xlab("Treatment")+
  theme_classic()+
  scale_y_continuous(limits=c(0,2.4),breaks=c(0,.4,.8,1.6,2,2.4),expand=c(0,0))

#histograms
uninhib<-filter(v_max,treatment=="uninhibited")
treatment<-filter(v_max,treatment=="5um")

hist(uninhib$vmax,breaks=6)
hist(treatment$vmax,breaks=6)

qqnorm(uninhib$vmax)
qqline(uninhib$vmax)

qqnorm(treatment$vmax)
qqline(treatment$vmax)

shapiro.test(uninhib$vmax)
shapiro.test(treatment$vmax)

mymodel<-lm(vmax~treatment,data=v_max)
View(resid(mymodel))

hist(resid(mymodel))

qqnorm(resid(mymodel))
qqline(resid(mymodel))

shapiro.test(resid(mymodel))

bartlett.test(vmax~treatment,data=v_max)

t.test(vmax~treatment,data=v_max,var.equal=TRUE)






