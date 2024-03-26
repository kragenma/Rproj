library(tidyverse)

#read in data
fern_summary<-read.csv("week4/piped_summary.csv")
View(fern_summary)

#da summary
da_summary<-filter(fern_summary,species=="da")
View(da_summary)

#da dot plot
ggplot(data=da_summary)+
  geom_point(mapping=aes(x=julian_date,y=wp_mean))+
  geom_errorbar(aes(x=julian_date,y=wp_mean,ymin=wp_mean-wp_se,ymax=wp_mean+wp_se),width=2,color="black",linetype="solid")+
  geom_line(aes(x=julian_date,y=wp_mean))+
  ylab("Water potential (MPa)")+
  xlab("Julian Date")+
  theme_classic()+
  scale_y_continuous(limits=c(-10,0),breaks=c(-10,-8,-6,-4,-2,0))+
  scale_x_continuous(limits=c(300,700),breaks=c(300,400,500,600,700))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

#self plot
days<-c(1,2,3,4,5,6,7)
inches<-c(5,8,4,10,14,18,21)
error<-c(1.1,1.5,1.4,1.6,1.3,1.1,1.2)
self_data<-data.frame(days,inches,error)
View(self_data)

ggplot(data=self_data)+
  geom_point(mapping=aes(x=days,y=inches))+
  geom_errorbar(aes(x=days,y=inches,ymin=inches-error,ymax=inches+error),width=.5,color="black",linetype="solid")+
  geom_line(aes(x=days,y=inches))+
  ylab("Inches of Rain")+
  xlab("Days")+
  theme_classic()+
  scale_y_continuous(limits=c(0,25),breaks=c(0,5,10,15,20,25))+
  scale_x_continuous(limits=c(0,10),breaks=c(0,2,4,6,8,10))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

#read in iris data
iris_data<-read.csv("week4/iris_petal_length_cm.csv")
View(iris_data)

#plot iris data
ggplot(data=iris_data)+
  geom_boxplot(mapping=aes(x=species,y=petal_length_cm))+
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

#bar plot
iris_data2<-group_by(iris_data,species)
iris_data3<-summarize(iris_data2,length_mean=mean(petal_length_cm),
                      length_sd=sd(petal_length_cm),
                      length_n1=n(),length_n2=sum(!is.na(petal_length_cm)))
View(iris_data3)

#add standard error
iris_summary<-mutate(iris_data3,length_se=length_sd/sqrt(length_n2))
View(iris_summary)

#barplot of iris data
ggplot(data=iris_summary)+
  geom_col(mapping=aes(x=species,y=length_mean),color="black",fill="aliceblue")+
  geom_errorbar(aes(x=species,y=length_mean,ymin=length_mean-length_se,ymax=length_mean+length_se),width=.2,color="black",linetype="solid")+
  ylab("Petal length (cm)")+
  xlab("Species")+
  theme_classic()+
  scale_y_continuous(limits=c(0,6),breaks=c(0,2,4,6),expand=c(0,0))

#read in fluorescence data
fluorescence_data<-read.csv("week4/wp_vs_fluorescence.csv")
View(fluorescence_data)

#read in fluorescence
fluorescence_clean<-filter(fluorescence_data,!is.na(wp_mean),!is.na(fluor_mean))
View(fluorescence_clean)

#scatterplot
ggplot(data=fluorescence_clean)+
  geom_point(mapping=aes(x=wp_mean,y=fluor_mean))+
  geom_smooth(mapping=aes(x=wp_mean,y=fluor_mean),method="lm",se=FALSE,color="black")+
  ylab("Chlorophyll fluorescence (Fv/Fm)")+
  xlab("Water potential (MPa)")+
  theme_classic()+
  scale_y_continuous(limits=c(0,1.2),breaks=c(0,.2,.4,.6,.8,1,1.2))+
  scale_x_continuous(limits=c(-10,0),breaks=c(-10,-8,-6,-4,-2,0))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

#independent ggplot 2
month<-c(1,2,3,4,5,6,7,8,9,10)
people<-c(103,136,175,170,218,267,327,390,475,550)
my_data<-data.frame(month,people)
View(my_data)

#my ggplot
ggplot(data=my_data)+
  geom_point(mapping=aes(x=month,y=people))+
  geom_smooth(mapping=aes(x=month,y=people),method="lm",se=FALSE,color="black")+
  ylab("People")+
  xlab("Month")+
  theme_classic()+
  scale_y_continuous(limits=c(100,555),breaks=c(100,150,200,250,300,350,400,450,500,550))+
  scale_x_continuous(limits=c(0,10),breaks=c(0,2,4,6,8,10))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))