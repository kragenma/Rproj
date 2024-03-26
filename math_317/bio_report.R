library(RColorBrewer)
library(tidyverse)

viability<-read.csv("Book1.csv")
View(viability)

vmax_data2<-group_by(viability,treatment_mm)
vmax_data3<-summarize(vmax_data2,viability_mean=mean(normalized),
                      viability_sd=sd(normalized),
                      viability_n2=sum(!is.na(normalized)))
View(vmax_data3)

vmax_summary<-mutate(vmax_data3,viability_se=viability_sd/sqrt(viability_n2))
View(vmax_summary)

vmax_summary$treatment_mm<-factor(vmax_summary$treatment_mm,c("control","low","high"))

#barplot
ggplot(data=vmax_summary)+
  scale_fill_brewer(name="Treatment",labels=c("Control","Low (0.1 mM)","High (0.25 mM)"),palette="Set2")+
  geom_col(mapping=aes(x=treatment_mm,y=viability_mean,fill=treatment_mm),alpha=0.5)+
  geom_errorbar(aes(x=treatment_mm,y=viability_mean,ymin=viability_mean-viability_se,ymax=viability_mean+viability_se),width=.02,color="black",linetype="solid")+
  ylab("Cell Viability (mean)")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(limits=c(0,1.2),breaks=c(0,.2,.4,.6,.8,1,1.2),expand=c(0,0))+
  scale_x_discrete(labels=c("Control","Low (0.1 mM)","High (0.25 mM)"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.title=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))
