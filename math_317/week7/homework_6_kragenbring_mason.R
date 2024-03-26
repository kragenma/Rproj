library(tidyverse)
distance<-read.csv("week7/golfers.csv")
View(distance)

golfers<-select(distance,-(driving_distance_yds))
View(golfers)

hist(golfers$height_m,breaks=12)
hist(golfers$weight_kg,breaks=12)

qqnorm(golfers$height_m)
qqline(golfers$height_m)

qqnorm(golfers$weight_kg)
qqline(golfers$weight_kg)

shapiro.test(golfers$height_m)
shapiro.test(golfers$weight_kg)

golf_log<-mutate(golfers,height_log=log(height_m))
View(golf_log)

golf_final<-mutate(golf_log,weight_log=log(weight_kg))
View(golf_final)

hist(golf_final$height_log,breaks=12)
hist(golf_final$weight_log,breaks=12)

qqnorm(golf_final$height_log)
qqline(golf_final$height_log)

qqnorm(golf_final$weight_log)
qqline(golf_final$weight_log)

shapiro.test(golf_final$height_log)
shapiro.test(golf_final$weight_log)

cor.test(golf_final$height_log,golf_final$weight_log,method="spearman")

cor.test(golf_final$height_log,golf_final$weight_log)

ggplot(data=golf_final)+
  geom_point(mapping=aes(x=height_log,y=weight_log))+
  ylab("Weight (log transformed)")+
  xlab("Height (log transformed)")











