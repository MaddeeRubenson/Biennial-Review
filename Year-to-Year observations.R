setwd("~/R")
dta<-read.csv("Coos-Coquille_df.sub.csv")

##Libraries
library(dplyr)
library(reshape2)

##Create date range
dta$Sampled <- as.POSIXct(strptime(dta$Sampled, format = '%Y-%m-%d')) 

#Extract year of each sample event
Sampled<-as.Date(dta$Sampled)
dta$year<-as.numeric(format(Sampled, format="%Y"))


##Remove unnecessary columns
dta$Client<-NULL
dta$Station_Description<-NULL
dta$SampleType<-NULL
dta$MRL<-NULL
dta$Unit<-NULL
dta$Status<-NULL
dta$DATUM<-NULL
dta$DECIMAL_LAT<-NULL
dta$DECIMAL_LONG<-NULL
dta$StatusIdentifier<-NULL
dta$Comment<-NULL
dta$HUC<-NULL
dta$Database<-NULL
dta$SD<-NULL
dta$Detect<-NULL
dta$time.lim<-NULL

##Melt
dta2<-melt(dta, id=c("year", "Analyte", "Result"), na.rm=TRUE) #value = station_ID

#a<-aggregate(.~year, data=dta2)
aggdata<-aggregate(dta2$Analyte, list(dta2$value, dta2$Result), length)

table<-dta%>%
  group_by(Station_ID) %>%
  dplyr:::summarise("date"==sum(year==2000)) #2001==sum(year==2001), 2002==sum(year==2002), 2003==sum(year==2003))
  



