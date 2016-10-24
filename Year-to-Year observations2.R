setwd("~/R")
dta<-read.csv("Coos-Coquille_df.sub.csv")


##Libraries
detach(package:plyr)    
library(dplyr)
library(reshape2)

##Create date range
dta$Sampled <- as.POSIXct(strptime(dta$Sampled, format = '%Y-%m-%d')) 

#Extract year of each sample event, add as an extra column
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

lstoutput <- list()
for (i in 1:length(unique(dta$Analyte))) {
  sub_data <- dta[dta$Analyte == unique(dta$Analyte)[i],]
  summary_by_year_by_analyte<-sub_data%>%
    group_by(Station_ID, year, Analyte)%>%
    dplyr::summarise(n_years=length(unique(year)))
  
  
  if (nrow(summary_by_year_by_analyte) == 0) {
    lstoutput[[i]] <- "wah wah"
  } else {
    lstoutput[[i]] <- dcast(summary_by_year_by_analyte, Station_ID ~ year)
  }
  
  names(lstoutput)[i] <- unique(dta$Analyte)[i]
  
}

lstoutput[]


Summary_by_year<-dta%>%
  group_by(Station_ID, Analyte, year)%>%
  dplyr::summarise(n_years=length(unique(year)),
                   first_year=min(year),
                   last_year=max(year))


##pH
pH_summary<-dta%>%
  filter(Analyte=="pH") %>%
  dplyr::group_by(Station_ID, Analyte, year)
  
pH_summary<-dcast(pH_summary, Station_ID ~ year)
pH_summary[pH_summary == 0] <- NA

#pH_summary$trend<-ifelse(length(unique(pH_summary[,c(2:10)]))>8,1,0)
pH_summary$trend_2000_2010<-ifelse(rowSums(is.na(pH_summary[,c(2:12)]))>0, 0, 1)
pH_summary$trend_2001_2011<-ifelse(rowSums(is.na(pH_summary[,c(3:13)]))>0, 0, 1)
pH_summary$trend_2002_2012<-ifelse(rowSums(is.na(pH_summary[,c(4:14)]))>0, 0, 1)
pH_summary$trend_2003_2013<-ifelse(rowSums(is.na(pH_summary[,c(5:15)]))>0, 0, 1)
pH_summary$trend_2004_2014<-ifelse(rowSums(is.na(pH_summary[,c(6:16)]))>0, 0, 1)
pH_summary$trend_2005_2015<-ifelse(rowSums(is.na(pH_summary[,c(7:17)]))>0, 0, 1)
pH_summary$trend_2006_2016<-ifelse(rowSums(is.na(pH_summary[,c(8:18)]))>0, 0, 1)

pH_summary$row_sum<-rowSums(pH_summary[,c(19:25)])

pH_trend<-pH_summary%>%
  filter(row_sum>0)

#Temperature
temp_summary<-dta%>%
  filter(Analyte=="Temperature") %>%
  dplyr::group_by(Station_ID, Analyte, year)
temp_summary<-dcast(temp_summary, Station_ID ~ year)
temp_summary[temp_summary == 0] <- NA

temp_summary$trend_2000_2010<-ifelse(rowSums(is.na(temp_summary[,c(2:12)]))>0, 0, 1)
temp_summary$trend_2001_2011<-ifelse(rowSums(is.na(temp_summary[,c(3:13)]))>0, 0, 1)
temp_summary$trend_2002_2012<-ifelse(rowSums(is.na(temp_summary[,c(4:14)]))>0, 0, 1)
temp_summary$trend_2003_2013<-ifelse(rowSums(is.na(temp_summary[,c(5:15)]))>0, 0, 1)
temp_summary$trend_2004_2014<-ifelse(rowSums(is.na(temp_summary[,c(6:16)]))>0, 0, 1)
temp_summary$trend_2005_2015<-ifelse(rowSums(is.na(temp_summary[,c(7:17)]))>0, 0, 1)
temp_summary$trend_2006_2016<-ifelse(rowSums(is.na(temp_summary[,c(8:18)]))>0, 0, 1)

temp_summary$row_sum<-rowSums(temp_summary[,c(19:25)])

temp_trend<-temp_summary%>%
  filter(row_sum>0)
 
#E. coli
Ecoli_summary<-dta%>%
  filter(Analyte=="E. Coli") %>%
  dplyr::group_by(Station_ID, Analyte, year)
Ecoli_summary<-dcast(Ecoli_summary, Station_ID ~ year)
Ecoli_summary[Ecoli_summary == 0] <- NA

Ecoli_summary$trend_2000_2010<-ifelse(rowSums(is.na(Ecoli_summary[,c(2:12)]))>0, 0, 1)
Ecoli_summary$trend_2001_2011<-ifelse(rowSums(is.na(Ecoli_summary[,c(3:13)]))>0, 0, 1)
Ecoli_summary$trend_2002_2012<-ifelse(rowSums(is.na(Ecoli_summary[,c(4:14)]))>0, 0, 1)
Ecoli_summary$trend_2003_2013<-ifelse(rowSums(is.na(Ecoli_summary[,c(5:15)]))>0, 0, 1)
Ecoli_summary$trend_2004_2014<-ifelse(rowSums(is.na(Ecoli_summary[,c(6:16)]))>0, 0, 1)
Ecoli_summary$trend_2005_2015<-ifelse(rowSums(is.na(Ecoli_summary[,c(7:17)]))>0, 0, 1)
Ecoli_summary$trend_2006_2016<-ifelse(rowSums(is.na(Ecoli_summary[,c(8:18)]))>0, 0, 1)

Ecoli_summary$row_sum<-rowSums(Ecoli_summary[,c(19:25)])

Ecoli_trend<-Ecoli_summary%>%
  filter(row_sum>0)

#Enterococcus 
Entero_summary<-dta%>%
  filter(Analyte=="Enterococcus") %>%
  dplyr::group_by(Station_ID, Analyte, year)
Entero_summary<-dcast(Entero_summary, Station_ID ~ year)
Entero_summary[Entero_summary == 0] <- NA

Entero_summary$trend_2000_2010<-ifelse(rowSums(is.na(Entero_summary[,c(2:12)]))>0, 0, 1)
Entero_summary$trend_2001_2011<-ifelse(rowSums(is.na(Entero_summary[,c(3:13)]))>0, 0, 1)
Entero_summary$trend_2002_2012<-ifelse(rowSums(is.na(Entero_summary[,c(4:14)]))>0, 0, 1)
Entero_summary$trend_2003_2013<-ifelse(rowSums(is.na(Entero_summary[,c(5:15)]))>0, 0, 1)
Entero_summary$trend_2004_2014<-ifelse(rowSums(is.na(Entero_summary[,c(6:16)]))>0, 0, 1)
Entero_summary$trend_2005_2015<-ifelse(rowSums(is.na(Entero_summary[,c(7:17)]))>0, 0, 1)
Entero_summary$trend_2006_2016<-ifelse(rowSums(is.na(Entero_summary[,c(8:18)]))>0, 0, 1)

Entero_summary$row_sum<-rowSums(Entero_summary[,c(19:24)])

Entero_trend<-Entero_summary%>%
  filter(row_sum>0)

####Status####

#pH
pH_status<-pH_summary[,c(1, 16:18)] #2014-2016
pH_status[pH_status == 0] <- NA

pH_status$status<-ifelse(rowSums(is.na(pH_status)[,c(2:4)])>0, 0, 1)

pH_status<-pH_status%>%
  filter(status_2014_2016 > 0)

#Temperature
temp_status<-temp_summary[,c(1,16:18)]
temp_status[temp_status == 0] <- NA

temp_status$status<-ifelse(rowSums(is.na(temp_status)[,c(2:4)])>0, 0, 1)

temp_status<-temp_status %>%
  filter(status > 0)

#E. coli
Ecoli_status<-Ecoli_summary[,c(1,16:18)]
Ecoli_status[Ecoli_status == 0] <- NA

Ecoli_status$status<-ifelse(rowSums(is.na(Ecoli_status)[,c(2:4)])>0, 0, 1)

Ecoli_status<-Ecoli_status %>%
  filter(status > 0)

#Enterococcus
Entero_status<-Entero_summary[,c(1,15:17)]
Entero_status[Entero_status == 0] <- NA

Entero_status$status<-ifelse(rowSums(is.na(Entero_status)[,c(2:4)])>0, 0, 1)

Entero_status<-Entero_status %>%
  filter(status > 0)
