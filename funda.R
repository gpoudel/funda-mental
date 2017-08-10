#analyzing real estate data from funda.nl

#Clear the workspace - remove all variables
rm(list = ls())
gc()


library("tidyverse")
library("RJSONIO")
library("DT")
library("data.table")
library("cowplot")

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



#-------------------All 4 Cities ----------------------

#Now lets combine all 4 DFs 
randstad <- rbind(ams.df, utc.df, hag.df, rtm.df)


#remove the individual city data
rm(ams.df, ams.info, utc.df, utc.info, rtm.df, rtm.info, hag.df, hag.info)



colnames(randstad) <- c("DaysSince", "NumberOfRooms", "Address", "GlobalId", "Price", "AgentId", 
                         "AgentName", "MobileURL", "PlotArea", "Postcode", "Products", "URL", 
                         "SalesStatus", "WGS84_X", "WGS84_Y", "LivingArea", "City", "PropertyType" )



#format the price to be displayed on normal integer format instead of exponential foramt
randstad$Price <- format(randstad$Price, scientific=FALSE)


#Format the text string of 'DaysSince' inofrmation for which the "on market period" is more than 6 months
randstad$DaysSince <- as.character(randstad$DaysSince)
randstad[which(randstad$DaysSince == "<span title=\"langer dan 6 maanden\">6+ maanden</span>")]$DaysSince <- "6+ maanden"


#the columns are in 'list' type so we need to change that
randstad$City <- as.character(randstad$City)
randstad$Price <- as.numeric(randstad$Price)
randstad$PropertyType <- as.character((randstad$PropertyType))


#'Woonoppervlakte' has null values, below statemnt will assing NA  to them by coercion
randstad$LivingArea <- as.numeric(as.character(randstad$LivingArea))
randstad$NumberOfRooms <- as.numeric(as.character(randstad$NumberOfRooms))


#'Perceeloppervlakte' has null values, below statemnt will assing NA  to them by coercion
randstad$PlotArea <- as.numeric(as.character(randstad$PlotArea))


#the data consists of properties like parking lots or just land, lets focus on house and apratements only for now
randstad <- data.table(randstad %>% filter(PropertyType == "woonhuis" | PropertyType == "appartement"))





#--------------------Vizs----------------------------

#turn off the scientific notations
options(scipen=999)


df <- randstad[which(randstad$LivingArea <= 400 & randstad$Price <= 1000000)]

ggplot(df, aes(LivingArea, Price,  colour = City)) + geom_point() + facet_grid(City~.)+ theme_bw()

ggplot(df, aes(LivingArea, Price,  colour = City)) + geom_point() + facet_grid(City~.) + coord_cartesian(xlim = c(0,50), ylim = c(0,500000)) + theme_bw()



print(ggplot(df,aes(LivingArea, colour = PropertyType)) + geom_histogram(binwidth = 10)) + theme_bw()
print(ggplot(df,aes(Price, colour = PropertyType)) + geom_histogram(binwidth = 50000)) + theme_bw()





amsArea <- ggplot(df %>% filter(City == "Amsterdam"),aes(LivingArea, colour = PropertyType)) + geom_histogram(binwidth = 10) + theme_bw()
rtmArea <- ggplot(df %>% filter(City == "Rotterdam"),aes(LivingArea, colour = PropertyType)) + geom_histogram(binwidth = 10) + theme_bw()
hagArea <- ggplot(df %>% filter(City == "Den Haag"),aes(LivingArea, colour = PropertyType)) + geom_histogram(binwidth = 10) + theme_bw()
utcArea <- ggplot(df %>% filter(City == "Utrecht"),aes(LivingArea, colour = PropertyType)) + geom_histogram(binwidth = 10) + theme_bw()


plot_grid(amsArea, rtmArea, hagArea, utcArea, 
          labels = c("Ams", "Rtm", "Hag", "Utc"),
          ncol = 2, nrow = 2)




amsPrice <- ggplot(df %>% filter(City == "Amsterdam"),aes(Price, colour = PropertyType)) + geom_histogram(binwidth = 50000) + theme_bw()
rtmPrice <- ggplot(df %>% filter(City == "Rotterdam"),aes(Price, colour = PropertyType)) + geom_histogram(binwidth = 50000) + theme_bw()
hagPrice <- ggplot(df %>% filter(City == "Den Haag"),aes(Price, colour = PropertyType)) + geom_histogram(binwidth = 50000) + theme_bw()
utcPrice <- ggplot(df %>% filter(City == "Utrecht"),aes(Price, colour = PropertyType)) + geom_histogram(binwidth = 50000) + theme_bw()

plot_grid(amsPrice, rtmPrice, hagPrice, utcPrice, 
          labels = c("Ams", "Rtm", "Hag", "Utc"),
          ncol = 2, nrow = 2)



print(ggplot(df,aes(LivingArea, colour = City)) + geom_histogram(binwidth = 10)) + theme_bw()
print(ggplot(df,aes(Price, colour = City)) + geom_histogram(binwidth = 50000))
print(ggplot(df,aes(NumberOfRooms, colour = City)) + geom_histogram(binwidth = 1))


utcPC <- ggplot(df %>% filter(City == "Utrecht"), aes(substr(Postcode,1,4), Price,  colour = PropertyType)) + geom_point() + facet_grid(City~.)+ theme_bw()
amsPC <- ggplot(df %>% filter(City == "Amsterdam"), aes(substr(Postcode,1,4), Price,  colour = PropertyType)) + geom_point() + facet_grid(City~.)+ theme_bw()
rtmPC <- ggplot(df %>% filter(City == "Rotterdam"), aes(substr(Postcode,1,4), Price,  colour = PropertyType)) + geom_point() + facet_grid(City~.)+ theme_bw()
hagPC <- ggplot(df %>% filter(City == "Den Haag"), aes(substr(Postcode,1,4), Price,  colour = PropertyType)) + geom_point() + facet_grid(City~.)+ theme_bw()



plot_grid(amsPC, rtmPC, hagPC, utcPC, 
          labels = c("Ams", "Rtm", "Hag", "Utc"),
          ncol = 1, nrow = 4)


ggplot(df %>% filter(City == "Utrecht"), aes(substr(Postcode,1,4), Price,  colour = PropertyType)) + geom_boxplot() + facet_grid(City~.)+ theme_bw()




