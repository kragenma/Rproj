#only run this code once to install the RColorBrewer package
install.packages("RColorBrewer")

#every time
library(RColorBrewer)
library(tidyverse)


#first check out all the brewer colors
display.brewer.all()

#generally, it's best to only use the colorblind friendly palettes
display.brewer.all(colorblindFriendly = TRUE)

#you can use this code to pick a number of colors (n) and a palette ("Set2")
display.brewer.pal(n=3, name="Set2")

#hexidecimal codes can be used to directly input your colors
brewer.pal(n=3,name="Set2")


#make a dataframe with three groups of data
group1 <-c(13,14,15,15,15,16,16,16,17,18)
group2 <-c(3,3,4,4,4,4,5,5,5,6)
group3 <-c(7,8,9,9,9,10,10,10,10,12)
response <-c(group1,group2,group3)
group <-c(rep(c("group1","group2","group3"),each=10))
unevendata<-data.frame(group,response)
View(unevendata)


#make a graph colored with color mapped to group
#note the use of scale_fill_brewer and scale_color_brewer
ggplot(data=unevendata)+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  geom_boxplot(mapping=aes(x=group,y=response,fill=group),alpha=0.5)+
  geom_jitter(aes(x=group,y=response,color=group),height=0)+
  theme_classic()+
  xlab("")+
  theme(legend.position="none")

#note that you can also map shape (pch) to group!
ggplot(data=unevendata)+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  geom_boxplot(mapping=aes(x=group,y=response,fill=group),alpha=0.5)+
  geom_jitter(aes(x=group,y=response,color=group,pch=group),height=0)+
  theme_classic()+
  xlab("")+
  theme(legend.position="none")


#what if you don't want to use the color package?
#you can use scale_fill_manual and scale_color_manual to directly choose the colors
ggplot(data=unevendata)+
  scale_fill_manual(values=c("red","green","blue"))+
  scale_color_manual(values=c("red","green","blue"))+
  geom_boxplot(aes(x=group,y=response,fill=group),alpha=0.5)+
  geom_jitter(aes(x=group,y=response,color=group,pch=group),height=0)+
  theme_classic()+
  xlab("")+
  theme(legend.position="none")

#this also works with hexidecimal codes
ggplot(data=unevendata)+
  scale_fill_manual(values=c("#B2ABD2","#4DAC26","#E66101"))+
  scale_color_manual(values=c("#B2ABD2","#4DAC26","#E66101"))+
  geom_boxplot(aes(x=group,y=response,fill=group),alpha=0.5)+
  geom_jitter(aes(x=group,y=response,color=group,pch=group),height=0)+
  theme_classic()+
  xlab("")+
  theme(legend.position="none")

