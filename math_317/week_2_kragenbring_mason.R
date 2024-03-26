#question 3
4+4

#question 4
a<-6
a

#question 5
gas<-9
gas
gas*8

#question 6
poster<-"mason"
poster

#question 7
mode(a)
mode(poster)

#question 8
myvector<-c(2,4,6,8,10)
myvector
myvector*3

#question 9
organism<-c("Human","Mouse","Fruit Fly", "Roundworm","Yeast")
genome_size_bp<-c(3100000000,2700000000,135600000,97000000,12100000)
est_gene_count<-c(30000,30000,13061,19099,6034)
comparative_genome_size<-data.frame(organism,genome_size_bp,est_gene_count)
View(comparative_genome_size)
comparative_genome_size

#question 10
plot(est_gene_count~genome_size_bp)
plot(est_gene_count~genome_size_bp, xlab="Genome Size (BP)", ylab="Est. Gene Count")

#question 11
days<-c(1,3,5,6,8,9,10)
inches<-c(11,14,20,21,26,32,35)
plot(inches~days, xlab="Days", ylab="Inches")

#question 12
mydata<-read.csv("iris_petal_length_kragenbring_mason.csv")
View(mydata)

#question 13
boxplot(petal_length_cm~Species, data=mydata)

#question 14
library(tidyverse)
install.packages("tidyverse")
