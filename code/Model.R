business=read.csv("Attributes_pizza.csv")
business=business[,c("business_id","stars","RestaurantsAttire",
                     "RestaurantsTakeOut","BusinessAcceptsCreditCards","NoiseLevel",
                     "GoodForKids","RestaurantsReservations","RestaurantsGoodForGroups",
                     "RestaurantsPriceRange2","HasTV","Alcohol","BikeParking",
                     "RestaurantsDelivery","OutdoorSeating","Caters","WiFi",
                     "dessert","latenight","lunch","dinner","brunch","breakfast","garage",
                     "street","validated","lot","valet","romantic","intimate","classy",
                     "hipster","divey","touristy","trendy","upscale","casual")]
for(i in c(4,5,7,8,9,11,13:16,18:37)){business[,i]=as.logical(business[,i])}

words=read.csv("WordEmbedding_selected.csv",row.names = 1)
correlation=cor(words$stars_x,words[,3:109])[1,]
freq=matrix(rep(NA,2039*106),nrow=2039)
for(i in 4:109){
  freq[,(i-3)]=tapply(words[,i], as.factor(words$business_id), sum)
}
colnames(freq)=colnames(words[,4:109])
freq=data.frame(freq)
freq$business_id=rownames(tapply(words[,i], as.factor(words$business_id), sum))
dat=merge(business,freq,by="business_id")
lm=lm(stars~.,dat[,2:143])
summary(lm)
