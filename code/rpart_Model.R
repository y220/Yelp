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
freq=matrix(rep(NA,2039*106),nrow=2039)
for(i in 4:109){
  freq[,(i-3)]=tapply(words[,i], as.factor(words$business_id), sum)
}
freq=data.frame(freq)
freq$business_id=rownames(tapply(words[,i], as.factor(words$business_id), sum))
colnames(freq)=c(colnames(words[,4:109]),"business_id")
freq$attitude_positive_1=apply(freq[,c("wonderful","perfectly","excellent",
                                       "fantastic","delicious","awesome",
                                       "favorite","perfect","amazing","loved")],1,sum)
freq$attitude_positive_2=apply(freq[,c("fun","happy")],1,sum)
freq$attitude_negative_1=freq$terrible
freq$attitude_negative_2=freq$bad
#freq=freq[,c("business_id","attitude_positive_1","attitude_positive_2",
             "attitude_negative_1","attitude_negative_2","super","hoagie","table","friendly","delivery",
             "pie","drink","dessert","mozzarella")]
freq[,c("wonderful","perfectly","excellent",
         "fantastic","delicious","awesome",
         "favorite","perfect","amazing","loved","fun","happy","terrible","bad","good","great")]=NULL
dat=merge(business,freq,by="business_id")

rownames(dat)=dat$business_id
dat$business_id=NULL
tree=rpart(stars~.,dat)
tree$variable.importance
sum((predict(tree)-dat$stars)^2)/2039
hist(predict(tree)-dat$stars)
predict(tree)
qqnorm(predict(tree)-dat$stars)
plot((predict(tree)-dat$stars))

rpart.plot(tree)
