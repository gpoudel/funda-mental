#analyzing real estate data from funda.nl

#Clear the workspace - remove all variables
rm(list = ls())
gc()


library("tidyverse")
library("RJSONIO")
library("DT")
library("data.table")


results_per_page <- 25  #This (25) is the max possible and hard coaded


#function find toal number of houses in city and number of pages of web data 
total.houses <- function(location) {
  
  rankings.link <- paste0('https://partnerapi.funda.nl/feeds/Aanbod.svc/json/271175433a7c4fe2a45750d385dd9bfd/?type=koop&zo=/',location,'/&page=1&pagesize=25')
  
  ranking.db <- fromJSON(rankings.link)
  
  house.count <- c( "total.houses" = ranking.db$TotaalAantalObjecten, "total.pages" = ranking.db$Paging$AantalPaginas)
  
  return(house.count)
  

}



#function that returns a matrix 
city.db <- function(location,total.houses,total.pages) {
  
  city.df <- c()
  
  for (i in 1:total.pages)
  {
    city.link <- paste0('https://partnerapi.funda.nl/feeds/Aanbod.svc/json/271175433a7c4fe2a45750d385dd9bfd/?type=koop&zo=/',location,'/&page=',i,'&pagesize=25')    
    
    city <- fromJSON(city.link)
    
     for (j in 1:25)
     {
       try(city.df <- rbind(city.df,t(city$Objects[[j]])), silent = TRUE)
       
     }
    
  }
  
  return(city.df)
  
}




#-------------------Utrecht-----------------------------


utc.info <- total.houses('utrecht')
utc.df <- city.db('utrecht', utc.info[1], utc.info[2])
utc.df <- data.table(utc.df)

names(utc.df)[names(utc.df) == 'Soort-aanbod'] <- 'Soort_aanbod'


#checking if GlobalId is indeed the unique key - which it looks like
if (nrow(utc.df) == length(unique(utc.df$Id))) { print("True") }


#Also check if some fields have only 0 or one value (True) using unique(utrecht.df$<fieldname>) - if so tey may be removed from DF as well


#Identical columns can be removed as well - function "identical" 
#identical(utrecht.df$WoonOppervlakteTot, utrecht.df$WoonOppervlakte)
#identical(utrecht.df$KoopprijsTot, utrecht.df$KoopprijsTot)
#utrecht.df <- utrecht.df %>% select(-KoopprijsTot, -WoonOppervlakteTot, -Id)


#Reduce the DF, select more relevents columns only
utc.df <- utc.df %>% select(AangebodenSindsTekst,
                                    AantalKamers,
                                    Adres,
                                    GlobalId,                                 
                                    Koopprijs,
                                    MakelaarId,
                                    MakelaarNaam,
                                    MobileURL,
                                    Perceeloppervlakte,
                                    Postcode,
                                    Producten,
                                    URL,
                                    VerkoopStatus,
                                    WGS84_X,
                                    WGS84_Y,
                                    Woonoppervlakte,
                                    Woonplaats,
                                    Soort_aanbod)


#format the price to be displayed on normal integer format instead of exponential foramt
utc.df$Koopprijs <- format(utc.df$Koopprijs, scientific=FALSE)

#'Woonoppervlakte' has null values, below statemnt will assing NA  to them by coercion
utc.df$Woonoppervlakte <- as.numeric(as.character(utc.df$Woonoppervlakte))

#'Perceeloppervlakte' has null values, below statemnt will assing NA  to them by coercion
utc.df$Perceeloppervlakte <- as.numeric(as.character(utc.df$Perceeloppervlakte))


utc.df$Koopprijs <- as.numeric(utc.df$Koopprijs)
utc.df$Soort_aanbod <- as.character((utc.df$Soort_aanbod))


ggplot(utc.df, aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()
ggplot(utc.df[which(utc.df$Woonoppervlakte <= 400 & utc.df$Koopprijs <= 500000)], aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()
ggplot(utc.df[which(utc.df$Soort_aanbod == 'parkeergelegenheid' | utc.df$Soort_aanbod == 'bouwgrond')], aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()




#-------------------Amsterdam-----------------------------



ams.info <- total.houses('amsterdam')
ams.df <- city.db('amsterdam', ams.info[1], ams.info[2])
ams.df <- data.table(ams.df)

names(ams.df)[names(ams.df) == 'Soort-aanbod'] <- 'Soort_aanbod'

#Reduce the DF, select more relevents columns only
ams.df <- ams.df %>% select(AangebodenSindsTekst,
                                    AantalKamers,
                                    Adres,
                                    GlobalId,                                 
                                    Koopprijs,
                                    MakelaarId,
                                    MakelaarNaam,
                                    MobileURL,
                                    Perceeloppervlakte,
                                    Postcode,
                                    Producten,
                                    URL,
                                    VerkoopStatus,
                                    WGS84_X,
                                    WGS84_Y,
                                    Woonoppervlakte,
                                    Woonplaats,
                                    Soort_aanbod)


#format the price to be displayed on normal integer format instead of exponential foramt
ams.df$Koopprijs <- format(ams.df$Koopprijs, scientific=FALSE)

#'Woonoppervlakte' has null values, below statemnt will assing NA  to them by coercion
ams.df$Woonoppervlakte <- as.numeric(as.character(ams.df$Woonoppervlakte))

#'Perceeloppervlakte' has null values, below statemnt will assing NA  to them by coercion
ams.df$Perceeloppervlakte <- as.numeric(as.character(ams.df$Perceeloppervlakte))

ams.df$Koopprijs <- as.numeric(ams.df$Koopprijs)
ams.df$Soort_aanbod <- as.character((ams.df$Soort_aanbod))

ggplot(ams.df, aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()
ggplot(ams.df[which(ams.df$Woonoppervlakte <= 400 & ams.df$Koopprijs <= 500000)], aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()
ggplot(ams.df[which(ams.df$Soort_aanbod == 'parkeergelegenheid' | ams.df$Soort_aanbod == 'bouwgrond')], aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()




#-------------------Rotterdam-----------------------------



rtm.info <- total.houses('rotterdam')
rtm.df <- city.db('rotterdam', rtm.info[1], rtm.info[2])
rtm.df <- data.table(rtm.df)

names(rtm.df)[names(rtm.df) == 'Soort-aanbod'] <- 'Soort_aanbod'

#Reduce the DF, select more relevents columns only
rtm.df <- rtm.df %>% select(AangebodenSindsTekst,
                                    AantalKamers,
                                    Adres,
                                    GlobalId,                                 
                                    Koopprijs,
                                    MakelaarId,
                                    MakelaarNaam,
                                    MobileURL,
                                    Perceeloppervlakte,
                                    Postcode,
                                    Producten,
                                    URL,
                                    VerkoopStatus,
                                    WGS84_X,
                                    WGS84_Y,
                                    Woonoppervlakte,
                                    Woonplaats,
                                    Soort_aanbod)


#format the price to be displayed on normal integer format instead of exponential foramt
rtm.df$Koopprijs <- format(rtm.df$Koopprijs, scientific=FALSE)

#'Woonoppervlakte' has null values, below statemnt will assing NA  to them by coercion
rtm.df$Woonoppervlakte <- as.numeric(as.character(rtm.df$Woonoppervlakte))

#'Perceeloppervlakte' has null values, below statemnt will assing NA  to them by coercion
rtm.df$Perceeloppervlakte <- as.numeric(as.character(rtm.df$Perceeloppervlakte))


rtm.df$Koopprijs <- as.numeric(rtm.df$Koopprijs)
rtm.df$Soort_aanbod <- as.character((rtm.df$Soort_aanbod))


ggplot(rtm.df, aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()
ggplot(rtm.df[which(rtm.df$Woonoppervlakte <= 400 & rtm.df$Koopprijs <= 500000)], aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()
ggplot(rtm.df[which(rtm.df$Soort_aanbod == 'parkeergelegenheid' | rtm.df$Soort_aanbod == 'bouwgrond')], aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()




#-------------------Den Haag-----------------------------



hag.info <- total.houses('denhaag')
hag.df <- city.db('denhaag', hag.info[1], hag.info[2])
hag.df <- data.table(hag.df)

names(hag.df)[names(hag.df) == 'Soort-aanbod'] <- 'Soort_aanbod'

#Reduce the DF, select more relevents columns only
hag.df <- hag.df %>% select(AangebodenSindsTekst,
                                    AantalKamers,
                                    Adres,
                                    GlobalId,                                 
                                    Koopprijs,
                                    MakelaarId,
                                    MakelaarNaam,
                                    MobileURL,
                                    Perceeloppervlakte,
                                    Postcode,
                                    Producten,
                                    URL,
                                    VerkoopStatus,
                                    WGS84_X,
                                    WGS84_Y,
                                    Woonoppervlakte,
                                    Woonplaats,
                                    Soort_aanbod)


#format the price to be displayed on normal integer format instead of exponential foramt
hag.df$Koopprijs <- format(hag.df$Koopprijs, scientific=FALSE)

#'Woonoppervlakte' has null values, below statemnt will assing NA  to them by coercion
hag.df$Woonoppervlakte <- as.numeric(as.character(hag.df$Woonoppervlakte))

#'Perceeloppervlakte' has null values, below statemnt will assing NA  to them by coercion
hag.df$Perceeloppervlakte <- as.numeric(as.character(hag.df$Perceeloppervlakte))


hag.df$Koopprijs <- as.numeric(hag.df$Koopprijs)
hag.df$Soort_aanbod <- as.character((hag.df$Soort_aanbod))





ggplot(hag.df, aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()
ggplot(hag.df[which(hag.df$Woonoppervlakte <= 400 & hag.df$Koopprijs <= 500000)], aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()
ggplot(hag.df[which(hag.df$Soort_aanbod == 'parkeergelegenheid' | hag.df$Soort_aanbod == 'bouwgrond')], aes(Woonoppervlakte, Koopprijs,  colour = Soort_aanbod)) + geom_point()







