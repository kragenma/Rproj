install.packages("writexl")

library(tidyverse)
library(RColorBrewer)
library(pzfx)
library(writexl)

plate()<-read.csv("plate_()_viability.csv")
View(plate())

#effector isolation
effector_count<-select(plate(),X:Lymphocytes.Single.Cells.Effectors.Viable...Count)

toDelete <- seq(1, nrow(effector_count), 2)

effector_count<-effector_count[-toDelete,]

#drug 1 sorting
viability()<-plate()[,-c(2,13)]

drug1_neff<-viability1[c(1,13,25,37,49,61,73,85),]

drug1_weff<-viability1[c(2,14,26,38,50,62,74,86),]

#drug 2
drug2_neff<-viability1[c(3,15,27,39,51,63,75,87),]

drug2_weff<-viability1[c(4,16,28,40,52,64,76,88),]

#drug 3
drug3_neff<-viability1[c(5,17,29,41,53,65,77,89),]

drug3_weff<-viability1[c(6,18,30,42,54,66,78,90),]

#drug 4
drug4_neff<-viability1[c(7,19,31,43,55,67,79,91),]

drug4_weff<-viability1[c(8,20,32,44,56,68,80,92),]

#drug 5
drug5_neff<-viability1[c(9,21,33,45,57,69,81,93),]

drug5_weff<-viability1[c(10,22,34,46,58,70,82,94),]

#drug 6
drug6_neff<-viability1[c(11,23,35,47,59,71,83,95),]

drug6_weff<-viability1[c(12,24,36,48,60,72,84,96),]

#excel export
neff_exports<-rbind(drug1_neff,drug2_neff,drug3_neff,drug4_neff,drug5_neff,drug6_neff)

weff_exports<-rbind(drug1_weff,drug2_weff,drug3_weff,drug4_weff,drug5_weff,drug6_weff)

plate()_controls<-viability()[1:12,]
colnames(plate1_controls)<-c("Concentration","DERL-2 Dead","DERL-2 Viable","SMZ Dead","SMZ Viable","T8ML Dead","T8ML Viable","HDMAR Dead","HDMAR Viable","OCI Dead","OCI Viable")
View(plate1_controls)

plate()_export<-rbind(neff_exports,weff_exports)
View(plate()_export)

as.numeric(plate()_export[1:80,2:11])

write_xlsx(plate()_export,"C:/Users/kragenma//plate()_viability_101122.xlsx")



