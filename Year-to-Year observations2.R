dta<-read.csv("//deqhq1/MRUBENS/R/Coos-Coquille_df.sub.csv")

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


WQtrend_pH<-dta%>%
  group_by(Station_ID) %>%
  filter(Analyte=="pH")%>%
  dplyr::summarise('2000' = sum(year == "2000"), 
                   '2001' = sum(year=="2001"),
                   '2002' = sum(year=="2002"),
                   '2003' = sum(year=="2003"),
                   '2004' = sum(year=="2004"),
                   '2005' = sum(year=="2005"),
                   '2006' = sum(year=="2006"),
                   '2007' = sum(year=="2007"),
                   '2008' = sum(year=="2008"),
                   '2009' = sum(year=="2009"),
                   '2010' = sum(year=="2010"),
                   '2011' = sum(year=="2011"),
                   '2012' = sum(year=="2012"),
                   '2013' = sum(year=="2013"),
                   '2014' = sum(year=="2014"),
                   '2015' = sum(year=="2015"),
                   '2016' = sum(year=="2016"))

  





