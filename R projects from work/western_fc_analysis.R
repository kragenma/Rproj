library(RColorBrewer)
library(tidyverse)

six_hr<-read.csv("six_hr.csv")
View(six_hr)

ctrl<-filter(six_hr, treatment=="control")
View(ctrl)

exatecan<-filter(six_hr, treatment=="exatecan")
View(exatecan)

belinostat<-filter(six_hr, treatment=="belinostat")
View(belinostat)

both<-filter(six_hr, treatment=="both")
View(both)

treatment_sum1<-group_by(six_hr,treatment)
trtmnt_sum2<-summarize(treatment_sum1,fc_mean=mean(fc),
                       fc_sd=sd(fc),
                       fc_n1=n())
View(trtmnt_sum2)

sixhr_sum<-mutate(trtmnt_sum2,fc_se=fc_sd/sqrt(fc_n1))
View(sixhr_sum)

control_data <- six_hr[six_hr$treatment == "control", "fc"]
both_data <- six_hr[six_hr$treatment == "both", "fc"]

# Perform a two-sample t-test
t.test(control_data, both_data)

# Print the t-test result
print(t_test_result)

six_analysis<-aov(fc~treatment, data = six_hr)
summary(six_analysis)

TukeyHSD(six_analysis)

sixhr_sum$treatment<-factor(sixhr_sum$treatment, levels=c("control","exatecan","belinostat","both"))

ggplot(data=sixhr_sum, aes(x=treatment,y=fc_mean, fill=treatment))+
  scale_fill_manual(name = "Treatment", labels = c("Control", "Exatecan", "Belinostat", "Both"), values = brewer.pal(4, "Set2"), limits=c("control","exatecan","belinostat","both"))+
  scale_color_brewer("Set2")+
  geom_col()+
  geom_errorbar(mapping=aes(x=treatment,y=fc_mean, ymin=fc_mean-fc_se, ymax=fc_mean+fc_se), width=.05, color="black", linetype="solid")+
  ylab("Fold Change")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(limits=c(0,1.5))+
  scale_x_discrete(labels=c("Control", "Exatecan", "Belinostat","Both"), limits=c("control","exatecan","belinostat","both"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))

#24 hour time point
day_hr<-read.csv("twentyfour_hr.csv")
View(day_hr)

treat_sum1<-group_by(day_hr,treatment)
treat_sum2<-summarize(treat_sum1,fc_mean=mean(fc),
                       fc_sd=sd(fc),
                       fc_n1=n())
View(treat_sum2)

tfhr_sum<-mutate(treat_sum2,fc_se=fc_sd/sqrt(fc_n1))
View(tfhr_sum)

ctrl_data <- day_hr[day_hr$treatment == "control", "fc"]
combination_data <- day_hr[day_hr$treatment == "both", "fc"]

# Perform a two-sample t-test
t.test(ctrl_data, combination_data)

# Print the t-test result
print(t_test_result)

six_analysis<-aov(fc~treatment, data = six_hr)
summary(six_analysis)

TukeyHSD(six_analysis)

tfhr_sum$treatment<-factor(tfhr_sum$treatment, levels=c("control","exatecan","belinostat","both"))

ggplot(data=tfhr_sum, aes(x=treatment,y=fc_mean, fill=treatment))+
  scale_fill_manual(name = "Treatment", labels = c("Control", "Exatecan", "Belinostat", "Both"), values = brewer.pal(4, "Set2"), limits=c("control","exatecan","belinostat","both"))+
  scale_color_brewer("Set2")+
  geom_col()+
  geom_errorbar(mapping=aes(x=treatment,y=fc_mean, ymin=fc_mean-fc_se, ymax=fc_mean+fc_se), width=.05, color="black", linetype="solid")+
  ylab("Fold Change")+
  xlab("")+
  theme_classic()+
  scale_y_continuous(limits=c(0,3))+
  scale_x_discrete(labels=c("Control", "Exatecan", "Belinostat","Both"), limits=c("control","exatecan","belinostat","both"))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.text.x=element_text(size=20,angle=60,hjust=1))+
  theme(axis.text.y=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(legend.text=element_text(size=20))







