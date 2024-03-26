install.packages("fmsb")
library(fmsb)

data<-as.data.frame(matrix(sample(1:3,4,replace=T),ncol=4))
colnames(data)<-c("Effector Increase","Cytotoxicity","T-Cell induced killing","MHC MFI")

data<-rbind(rep(4,1),rep(0,4),data)

radarchart(data,axistype=1,
           pcol=rgb(0.2,.5,.5,.9),pfcol=rgb(.2,.5,.5,.5),plwd=1,
           cglcol="grey",cglty = 1,axislabcol = "grey",caxislabel=seq(0,4),cglwd=.8,
           vlcex = .8)

View(data)

data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

data <- rbind(rep(20,10) , rep(0,10) , data)

radarchart( data  , axistype=1 ,
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            vlcex=0.8 )