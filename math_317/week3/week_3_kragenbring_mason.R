library(tidyverse)
#read in the data
ferndata <-read.csv("week3/fern_seasonal_water_potential.csv")
View(ferndata)

#add a column with water potential in MPa
ferndata_2<-mutate(ferndata,water_potential_mpa=water_potential_bar/10)
View(ferndata_2)

#arrange by date
ferndata_3<-arrange(ferndata_2,year,month,day)
View(ferndata_3)
ferndata_3b<-arrange(ferndata_2,julian_date)
View(ferndata_3b)

#filter data to get evergreen only
evergreen_data<-filter(ferndata_3,phenology=="evergreen")
View(evergreen_data)

#evergreen november only
evergreen_data_nov<-filter(ferndata_3,phenology=="evergreen",month==11)
View(evergreen_data_nov)

#evergreen, November and December
evergreen_data_nov_dec<-filter(ferndata_3,phenology=="evergreen",month==11|month==12)
View(evergreen_data_nov_dec)

evergreen_data_july_thru_dec<-filter(ferndata_3,phenology=="evergreen",month%in%7:12)
View(evergreen_data_july_thru_dec)

#filter chaparral only
chaparral_data<-filter(ferndata_3,habitat=="chaparral")
View(chaparral_data)

#filter da out
no_da<-filter(ferndata_3,species!="da")
View(no_da)

#select only the date columns
dates_only<-select(ferndata_3,year:julian_date)
View(dates_only)

#select all but the date lines
no_dates<-select(ferndata_3,-(year:julian_date))
View(no_dates)

#no phenology of evergreen
evergreen_data_2<-select(evergreen_data,-(phenology))
View(evergreen_data_2)

#summary
evergreen_by_day<-group_by(evergreen_data_2,julian_date,species)
mean_wp<-summarize(evergreen_by_day,wp_mean=mean(water_potential_mpa,na.rm=TRUE))
View(mean_wp)

#better summary
evergreen_by_day<-group_by(evergreen_data_2,julian_date,species)
summary_wp<-summarize(evergreen_by_day,wp_mean=mean(water_potential_mpa,na.rm=TRUE),wp_sd=sd(water_potential_MPa,na.rm=TRUE),wp_n1=n(),wp_n2=sum(!is.na(water_potential_MPa)))
View(summary_wp)

#add standard error
summary_w_se<-mutate(summary_wp,wp_se=wp_sd/sqrt(wp_n2))
View(summary_w_se)
summary_final<-select(summary_wp,-(wp_n1))
View(summary_final)

#using pipes
piped_summary<-mutate(ferndata,water_potential_mpa=water_potential_bar/10)%>%
  arrange(year,month,day)%>%
  filter(phenology=="evergreen")%>%
  select(-phenology)%>%
  group_by(julian_date,species)%>%
  summarize(wp_mean=mean(water_potential_mpa,na.rm=TRUE),
            wp_sd=sd(water_potential_mpa,na.rm=TRUE),
            wp_n1=n(),wp_n2=sum(!is.na(water_potential_mpa)))%>%
  mutate(wp_se=wp_sd/sqrt(wp_n2))%>%
  select(!wp_n1)
View(piped_summary)