library("rpart")
library("rpart.plot")
business=read.csv("Attributes_pizza.csv")
business=business[,c("business_id","stars",
                     "GoodForKids",
                     "RestaurantsPriceRange2",
                     "RestaurantsDelivery","Caters","WiFi")]

words=read.csv("WordEmbedding_selected.csv",row.names = 1)
freq=matrix(rep(NA,2039*106),nrow=2039)
for(i in 4:109){
  freq[,(i-3)]=tapply(words[,i], as.factor(words$business_id), sum)
}
freq=data.frame(freq)
freq$business_id=rownames(tapply(words[,i], as.factor(words$business_id), sum))
colnames(freq)=c(colnames(words[,4:109]),"business_id")
freq=freq[,c("business_id", "hoagie", "table", "friendly", 
             "delivery", "pie", "drink", "dessert", "mozzarella", 
             "fresh",  "crispy", "greasy", "spicy","italian")]

dat=merge(business,freq,by="business_id")

rownames(dat)=dat$business_id
dat$business_id=NULL
tree=rpart(stars~.,dat,cp=0.005)
tree$variable.importance

#### MSE ####
sum((predict(tree)-dat$stars)^2)/2039
hist(predict(tree)-dat$stars)
qqnorm(predict(tree)-dat$stars)
plot((predict(tree)-dat$stars),pch=20)

#### Create tree plot ####
jpeg("rpart.jpg")
rpart.plot(tree,type=1,fallen.leaves = FALSE, tweak = 1.4, space = 1, gap=0.5)
dev.off()
