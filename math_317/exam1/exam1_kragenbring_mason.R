library(tidyverse)

seedlings_a<-read.csv("exam1/exam_1a_kragenbring.csv")
View(seedlings_a)

no_na<-filter(seedlings_a,!is.na(seedling_height_cm),!is.na(seed_mass_g))
View(no_na)

ggplot(data=no_na)+
  geom_point(mapping=aes(x=seed_mass_g,y=seedling_height_cm))+
  geom_smooth(mapping=aes(x=seed_mass_g,y=seedling_height_cm),method="lm",se=FALSE,color="black")+
  ylab("Seedling height (cm)")+
  xlab("Seed mass (g)")+
  theme_classic()+
  scale_y_continuous(limits=c(5,15),breaks=c(5,7,9,11,13,15))+
  scale_x_continuous(limits=c(1.5,2.5),breaks=c(1.5,1.7,1.9,2.1,2.3,2.5))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

#question 2
organism<-c("Lion","Tiger","Bobcat")
whisker_length<-c(30,16,10)
feline_data<-data.frame(organism,whisker_length)
View(feline_data)

ggplot(data=feline_data)+
  geom_boxplot(mapping=aes(x=organism,y=whisker_length))+
  ylab("Petal Length (cm)")+
  xlab("Species")+
  theme_classic()+
  scale_y_continuous(limits=c(0,8),breaks=c(0,2,4,6,8))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

#question 3
cheer_volume<-read.csv("exam1/exam_1c_kragenbring.csv")
View(cheer_volume)

waves_cheer<-filter(cheer_volume,group=="waves")
View(waves_cheer)

summary_waves<-summarize(waves_cheer,decibel_mean=mean(volume_decibels,na.rm=TRUE),wavedb_sd=sd(volume_decibels,na.rm=TRUE),wavesdb_n1=n(),wavesdb_n2=sum(!is.na(volume_decibels)))
View(summary_waves)

summary_wavedb_se<-mutate(summary_waves,wavedb_se=wavedb_sd/sqrt(wavesdb_n2))
View(summary_wavedb_se)
summary_waves_final<-select(summary_waves,-(wavesdb_n1))
View(summary_waves_final)

ucla_cheer<-filter(cheer_volume,group=="bruins")
View(ucla_cheer)

summary_bruins<-summarize(ucla_cheer,bruinsdb_mean=mean(volume_decibels,na.rm=TRUE),bruinsdb_sd=sd(volume_decibels,na.rm=TRUE),bruinsdb_n1=n(),bruinsdb_n2=sum(!is.na(volume_decibels)))
View(summary_bruins)

summary_bruinsdb_se<-mutate(summary_bruins,bruinsdb_se=bruinsdb_sd/sqrt(bruinsdb_n2))
View(summary_bruinsdb_se)
summary_bruinsdb<-select(summary_bruins,-(bruinsdb_n1))
View(summary_bruinsdb)

cheerdb_data<-group_by(cheer_volume,group)
cheerdb_data2<-summarize(cheerdb_data,db_mean=mean(volume_decibels,na.rm=TRUE),
                           db_sd=sd(volume_decibels,na.rm=TRUE),
                           db_n1=n(),db_n2=sum(!is.na(volume_decibels)))
View(cheerdb_data2)

cheerdb_summary<-mutate(cheerdb_data2,db_se=db_sd/sqrt(db_n2))
View(cheerdb_summary)

ggplot(data=cheerdb_summary)+
  geom_col(mapping=aes(x=group,y=db_mean),color="black",fill="aliceblue")+
  geom_errorbar(aes(x=group,y=db_mean,ymin=db_mean-db_se,ymax=db_mean+db_se),width=.1,color="black",linetype="solid")+
  ylab("Volume (db)")+
  xlab("Group")+
  theme_classic()+
  scale_y_continuous(limits=c(25,30),breaks=c(25,25.5,26,26.5,27,27.5,28,28.5,29,29.5,30),expand=c(0,0))