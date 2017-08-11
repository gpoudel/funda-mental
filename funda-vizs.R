#Visualize -  real estate data from funda.nl


randstad <- read.csv("randstad.csv", sep = ";", header = T)


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




