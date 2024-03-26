library(tidyverse)

practice1<-read.csv("week5/exam_1_practice.csv")
View(practice1)

new_practice1<-mutate(practice1,height_mm=height_m*1000)
View(new_practice1)

control_practice1<-filter(new_practice1,group=="control")
View(control_practice1)

summary_cpractice<-summarize(control_practice1,cheight_mean=mean(height_mm,na.rm=TRUE),cheight_sd=sd(height_mm,na.rm=TRUE),cheight_n1=n(),cheight_n2=sum(!is.na(height_mm)))
View(summary_cpractice)

summary_cheight_se<-mutate(summary_cpractice,cheight_se=cheight_sd/sqrt(cheight_n2))
View(summary_cheight_se)
summary_cfinal<-select(summary_cpractice,-(cheight_n1))
View(summary_final)

treatment_practice1<-filter(new_practice1,group=="treatment")
View(treatment_practice1)

summary_tpractice<-summarize(treatment_practice1,theight_mean=mean(height_mm,na.rm=TRUE),theight_sd=sd(height_mm,na.rm=TRUE),theight_n1=n(),theight_n2=sum(!is.na(height_mm)))
View(summary_tpractice)

summary_theight_se<-mutate(summary_tpractice,theight_se=theight_sd/sqrt(theight_n2))
View(summary_theight_se)
summary_tfinal<-select(summary_tpractice,-(theight_n1))
View(summary_tfinal)

practice1_data<-group_by(new_practice1,group)
practice1_data2<-summarize(practice1_data,height_mean=mean(height_mm,na.rm=TRUE),
                      height_sd=sd(height_mm,na.rm=TRUE),
                      height_n1=n(),height_n2=sum(!is.na(height_mm)))
View(practice1_data2)

practice1_summary<-mutate(practice1_data2,height_se=height_sd/sqrt(height_n2))
View(practice1_summary)

ggplot(data=practice1_summary)+
  geom_col(mapping=aes(x=group,y=height_mean),color="black",fill="aliceblue")+
  geom_errorbar(aes(x=group,y=height_mean,ymin=height_mean-height_se,ymax=height_mean+height_se),width=.1,color="black",linetype="solid")+
  ylab("Height(mm)")+
  xlab("Group")+
  theme_classic()+
  scale_y_continuous(limits=c(0,330),breaks=c(0,50,100,150,200,250,300,330),expand=c(0,0))