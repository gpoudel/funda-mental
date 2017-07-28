#analyzing real estate data from funda.nl

#Clear the workspace - remove all variables
rm(list = ls())
gc()


library("tidyverse")
library("RJSONIO")
library("DT")


funda_ranking_utrecht <- "https://partnerapi.funda.nl/feeds/Aanbod.svc/json/271175433a7c4fe2a45750d385dd9bfd/?type=koop&zo=/amsterdam/&page=1&pagesize=25"



utrecht <- fromJSON(funda_ranking_utrecht)

utrecht_df <- c()


 for (i in 1:25) {
   
   utrecht_df <- rbind(utrecht_df,t(utrecht$Objects[[i]]))
   
 }