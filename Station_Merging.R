###Coos-Coquille AG WQ Biennial Review###
####Combine data for duplicate station IDs for Enterococcus: 
#Stations: 21ORBCH-29316, 21ORBCH-31450 and 21ORBCH-31561

d<-read.csv("//deqhq1/MRUBENS/R/Coos-Coquille_df.sub.csv")

library(dplyr)

Station_29316<-d%>%
  filter(Station_ID == "29316" | Station_ID == "21ORBCH-29316") %>%
  distinct()%>%
  filter(Station_ID == "29316")

source('DataAnalysis/StatusAndTrends/app/functions/funHelpers.R')
source('DataAnalysis/StatusAndTrends/app/functions/funSeaKen.R')
source('DataAnalysis/StatusAndTrends/app/functions/funPlots.R')


EvaluateEnteroWQS <- function(new_data) {
  new_data$exceed <- ifelse(new_data[, 'Result'] > 158, 1, 0)
  entero_gm_eval <- gm_mean_30_day(new_data, 
                                   unique(new_data$Analyte), 
                                   unique(new_data$Station_ID))
  entero_gm_eval$exceed <- ifelse(entero_gm_eval$gm > 35, 1, 0)
  ex_df <- data.frame("Station_ID" = rep(unique(new_data$Station_ID),2),
                      "Station_Description" = rep(unique(
                        new_data$Station_Description), 2),
                      "Sample" = c('Single sample', 'Geomean'),
                      "Obs" = c(nrow(new_data),
                                nrow(entero_gm_eval)),
                      "Exceedances" = c(sum(new_data$exceed),
                                        sum(entero_gm_eval$exceed))
  )
  attr(new_data, "entero_gm_eval") <- entero_gm_eval
  attr(new_data, "ex_df") <- ex_df
  return(new_data)
}

EvaluateEnteroWQS(Station_29316)
