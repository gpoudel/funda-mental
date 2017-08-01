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



utrecht.info <- total.houses('utrecht')

utrecht.df <- city.db('utrecht', utrecht.info[1], utrecht.info[2])



utrecht.df <- data.table(utrecht.df)

names(utrecht.df)[names(utrecht.df) == 'Soort-aanbod'] <- 'Soort_aanbod'


#checking if GlobalId is indeed the unique key - which it looks like
if (nrow(utrecht.df) == length(unique(utrecht.df$Id))) { print("True") }


#Also check if some fields have only 0 or one value (True) using unique(utrecht.df$<fieldname>) - if so tey may be removed from DF as well

utrecht.df <- utrecht.df %>% select(AangebodenSindsTekst,
                                    AantalKamers,
                                    Adres,
                                    GlobalId,
                                    Id,                                    
                                    Koopprijs,
                                    KoopprijsTot,
                                    MakelaarId,
                                    MakelaarNaam,
                                    MobileURL,
                                    Oppervlakte,
                                    Perceeloppervlakte,
                                    Postcode,
                                    Producten,
                                    URL,
                                    VerkoopStatus,
                                    WGS84_X,
                                    WGS84_Y,
                                    WoonOppervlakteTot,
                                    Woonoppervlakte,
                                    Woonplaats,
                                    Soort_aanbod)










