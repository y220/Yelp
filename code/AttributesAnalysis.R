rm(list = ls())
# load required package for json and read data

library(jsonlite)
library(plyr)
# review = stream_in(file("Data/review_city.json"))
business = stream_in(file("data/business_city.json"))
unique(business$state)
# select pets and pet service
pizza = business[grep("Pizza", business$categories), ]


attribute = pizza$attributes
pizza_new <- cbind(pizza,attribute)
pizza <- subset(pizza_new, select = -c(attributes))

for(i in 1:length(pizza$GoodForMeal)){
  if(is.na(pizza$GoodForMeal)==T){
    next;
  }
  else{
    pizza$GoodForMeal[i]=gsub("\'","\"",pizza$GoodForMeal[i])
    pizza$GoodForMeal[i]=gsub("False","\"False\"",pizza$GoodForMeal[i])
    pizza$GoodForMeal[i]=gsub("True","\"True\"",pizza$GoodForMeal[i])
    pizza$GoodForMeal[i]=gsub("None","\"False\"",pizza$GoodForMeal[i])
  }
}

GoodForMeal = as.data.frame(fromJSON(pizza$GoodForMeal[1]))
for(i in 2:length(pizza$GoodForMeal)){
  if(is.na(pizza$GoodForMeal[i])==T){
    GoodForMeal[i,]="NA"
  }
  else if(length(as.data.frame(fromJSON(pizza$GoodForMeal[i])))==1){
    GoodForMeal[i,]=as.data.frame(fromJSON(pizza$GoodForMeal[i]))[1,1]
  }
  else{
    GoodForMeal = rbind(GoodForMeal,as.data.frame(fromJSON(pizza$GoodForMeal[i])))
  }
}

for(i in 1:length(pizza$BusinessParking)){
  if(is.na(pizza$BusinessParking)==T){
    next;
  }
  else{
    pizza$BusinessParking[i]=gsub("\'","\"",pizza$BusinessParking[i])
    pizza$BusinessParking[i]=gsub("False","\"False\"",pizza$BusinessParking[i])
    pizza$BusinessParking[i]=gsub("True","\"True\"",pizza$BusinessParking[i])
    pizza$BusinessParking[i]=gsub("None","\"False\"",pizza$BusinessParking[i])
  }
}


BusinessParking = as.data.frame(fromJSON(pizza$BusinessParking[1]))
for(i in 2:length(pizza$BusinessParking)){
  if(is.na(pizza$BusinessParking[i])==T){
    BusinessParking[i,]="NA"
  }
  else if(length(as.data.frame(fromJSON(pizza$BusinessParking[i])))==1){
    BusinessParking[i,]=as.data.frame(fromJSON(pizza$BusinessParking[i]))[1,1]
  }
  else{
    BusinessParking = rbind(BusinessParking,as.data.frame(fromJSON(pizza$BusinessParking[i])))
  }
}


#Ambience
for(i in 1:length(pizza$Ambience)){
  if(is.na(pizza$Ambience)==T){
    next;
  }
  else{
    pizza$Ambience[i]=gsub("\'","\"",pizza$Ambience[i])
    pizza$Ambience[i]=gsub("False","\"False\"",pizza$Ambience[i])
    pizza$Ambience[i]=gsub("True","\"True\"",pizza$Ambience[i])
    pizza$Ambience[i]=gsub("None","\"False\"",pizza$Ambience[i])
  }
}


Ambience = as.data.frame(fromJSON(pizza$Ambience[1]))
for(i in 2:length(pizza$Ambience)){
  if(is.na(pizza$Ambience[i])==T){
    Ambience[i,]="NA"
  }
  else if(length(as.data.frame(fromJSON(pizza$Ambience[i])))==1){
    Ambience[i,]=as.data.frame(fromJSON(pizza$Ambience[i]))[1,1]
  }
  else{
    Ambience = rbind.fill(Ambience,as.data.frame(fromJSON(pizza$Ambience[i])))
  }
}

x=c()
for(i in 1:ncol(pizza)){
  x[i]=sum(is.na(pizza[,i])==F)
}
x

pizza_new <- pizza[,-c(27,30,33,35:52)]
pizza_new <- cbind(pizza_new,GoodForMeal,BusinessParking,Ambience)
hours = pizza$hours
pizza_new <- cbind(pizza_new,hours)
pizza_new <- subset(pizza_new, select = -c(GoodForMeal,BusinessParking,Ambience,hours,postal_code,review_count,
                                           categories))

x=c()
for(i in 1:ncol(pizza_new)){
  x[i]=sum(is.na(pizza_new[,i])==F)
}
x


#casual=1;formal=2;dressy=3
for(i in 1:nrow(pizza_new)){
  if(is.na(pizza_new$RestaurantsAttire[i])==T){
    next
  }
  if(pizza_new$RestaurantsAttire[i]=="'casual'"| pizza_new$RestaurantsAttire[i]=="u'casual'"){
    pizza_new$RestaurantsAttire[i]=1
  }
  if(pizza_new$RestaurantsAttire[i]=="'formal'"| pizza_new$RestaurantsAttire[i]=="u'formal'"){
    pizza_new$RestaurantsAttire[i]=2
  }
  if(pizza_new$RestaurantsAttire[i]=="'dressy'"| pizza_new$RestaurantsAttire[i]=="u'dressy'"){
    pizza_new$RestaurantsAttire[i]=3
  }
  if(pizza_new$RestaurantsAttire[i]=="None"){
    pizza_new$RestaurantsAttire[i]=NA
  }
}


#quiet=1;average=2;loud=3;very_loud=4
for(i in 1:nrow(pizza_new)){
  if(is.na(pizza_new$NoiseLevel[i])==T){
    next
  }
  if(pizza_new$NoiseLevel[i]=="'quiet'"| pizza_new$NoiseLevel[i]=="u'quiet'"){
    pizza_new$NoiseLevel[i]=1
  }
  if(pizza_new$NoiseLevel[i]=="'average'"| pizza_new$NoiseLevel[i]=="u'average'" ){
    pizza_new$NoiseLevel[i]=2
  }
  if(pizza_new$NoiseLevel[i]=="'loud'"| pizza_new$NoiseLevel[i]=="u'loud'"){
    pizza_new$NoiseLevel[i]=3
  }
  if(pizza_new$NoiseLevel[i]=="'very_loud'"| pizza_new$NoiseLevel[i]=="u'very_loud'"){
    pizza_new$NoiseLevel[i]=4
  }
  if(pizza_new$NoiseLevel[i]=="None"){
    pizza_new$NoiseLevel[i]=NA
  }
}

#no=1;beer_and_wine=2;full_bar=3
for(i in 1:nrow(pizza_new)){
  if(is.na(pizza_new$Alcohol[i])==T){
    next
  }
  if(pizza_new$Alcohol[i]=="'none'"| pizza_new$Alcohol[i]=="u'none'"){
    pizza_new$Alcohol[i]=1
  }
  if(pizza_new$Alcohol[i]=="'beer_and_wine'"| pizza_new$Alcohol[i]=="u'beer_and_wine'" ){
    pizza_new$Alcohol[i]=2
  }
  if(pizza_new$Alcohol[i]=="'full_bar'"| pizza_new$Alcohol[i]=="u'full_bar'"){
    pizza_new$Alcohol[i]=3
  }
  if(pizza_new$Alcohol[i]=="None"){
    pizza_new$Alcohol[i]=NA
  }
}

#no=1;free=2;paid=3
for(i in 1:nrow(pizza_new)){
  if(is.na(pizza_new$WiFi[i])==T){
    next
  }
  if(pizza_new$WiFi[i]=="'no'"| pizza_new$WiFi[i]=="u'no'"){
    pizza_new$WiFi[i]=1
  }
  if(pizza_new$WiFi[i]=="'free'"| pizza_new$WiFi[i]=="u'free'" ){
    pizza_new$WiFi[i]=2
  }
  if(pizza_new$WiFi[i]=="'paid'"| pizza_new$WiFi[i]=="u'paid'"){
    pizza_new$WiFi[i]=3
  }
  if(pizza_new$WiFi[i]=="None"){
    pizza_new$WiFi[i]=NA
  }
}
write.csv(pizza_new,"data/Attributes_pizza.csv")

library(rpart)
library(maptree)


pizza <- read.csv("data/Attributes_pizza.csv")
pizza <- pizza[,-c(1,4:8,46:52)]
pizza <- pizza[,-c(1,2,4)]

head(pizza)
colnames(pizza)
for(i in 1:nrow(pizza)){
  for(j in 1:ncol(pizza)){
    if(is.na(pizza[i,j])==T){
      next
    }
    else if(pizza[i,j]=="False"){
      pizza[i,j]=0
    }
    else if(pizza[i,j]=="True"){
      pizza[i,j]=1
    }
    else{ next }
  }
}
# set.seed(1234) 
# 
# train<-sample(nrow(pizza),0.7*nrow(pizza)) 
# tdata<-pizza[train,] 
# vdata<-pizza[-train,] 
# fit <- rpart(formula = stars ~ ., method='anova', data = tdata)
# fit
# printcp(fit)
# library(rpart.plot)
# rpart.plot(fit,branch=1,type=2, fallen.leaves=T,cex=0.8, sub="Tree")
# predtree<-predict(fit,newdata=vdata,type="vector") 
# vdata$stars-predtree

x=c()
y=c()
for(i in 1:nrow(pizza)){
  if(is.na(pizza$RestaurantsDelivery[i])==T){
    next
  }
  else if(pizza$RestaurantsDelivery[i]==1){
    x=c(x,pizza$stars[i])
  }
  else{
    y=c(y,pizza$stars[i])
  }
}
t.test(x,y,paired = F)
hist(x)
hist(y)

unique(pizza$RestaurantsAttire)# no deliver>deliver

r = lm(pizza$stars~as.factor(pizza$RestaurantsAttire),na.action=na.omit )
summary(r)#Attire casual best

unique(pizza$RestaurantsTakeOut)
x=c()
y=c()
for(i in 1:nrow(pizza)){
  if(is.na(pizza$RestaurantsTakeOut[i])==T){
    next
  }
  else if(pizza$RestaurantsTakeOut[i]==1){
    x=c(x,pizza$stars[i])
  }
  else{
    y=c(y,pizza$stars[i])
  }
}
t.test(x,y,paired = F)
hist(x)
hist(y)#no takeout>takeout

unique(pizza$BusinessAcceptsCreditCards)
x=c()
y=c()
for(i in 1:nrow(pizza)){
  if(is.na(pizza$BusinessAcceptsCreditCards[i])==T){
    next
  }
  else if(pizza$BusinessAcceptsCreditCards[i]==0){
    x=c(x,pizza$stars[i])
  }
  else{
    y=c(y,pizza$stars[i])
  }
}
t.test(x,y,paired = F)
hist(x)
hist(y)#credit cards > no credit cards

unique(pizza$NoiseLevel)
r = lm(pizza$stars~as.factor(pizza$NoiseLevel),na.action=na.omit )
summary(r)#1,2>3,4 quiet=1;average=2;loud=3;very_loud=4

unique(pizza$GoodForKids)
x=c()
y=c()
for(i in 1:nrow(pizza)){
  if(is.na(pizza$GoodForKids[i])==T){
    next
  }
  else if(pizza$GoodForKids[i]==0){
    x=c(x,pizza$stars[i])
  }
  else{
    y=c(y,pizza$stars[i])
  }
}
t.test(x,y,paired = F)
hist(x)
hist(y)# good for kids > bad for kids


x=c()
y=c()
for(i in 1:nrow(pizza)){
  if(is.na(pizza$RestaurantsReservations[i])==T){
    next
  }
  else if(pizza$RestaurantsReservations[i]==0){
    x=c(x,pizza$stars[i])
  }
  else{
    y=c(y,pizza$stars[i])
  }
}
t.test(x,y,paired = F)
hist(x)
hist(y)# reservation > no reservation

x=c()
y=c()
for(i in 1:nrow(pizza)){
  if(is.na(pizza$RestaurantsReservations[i])==T){
    next
  }
  else if(pizza$RestaurantsReservations[i]==0){
    x=c(x,pizza$stars[i])
  }
  else{
    y=c(y,pizza$stars[i])
  }
}
t.test(x,y,paired = F)
hist(x)
hist(y)#good for groups > no

unique(pizza$RestaurantsPriceRange2)
r = lm(pizza$stars~as.factor(pizza$RestaurantsPriceRange2),na.action=na.omit )
summary(r)# price 2 best

unique(pizza$HasTV)
x=c()
y=c()
for(i in 1:nrow(pizza)){
  if(is.na(pizza$HasTV[i])==T){
    next
  }
  else if(pizza$HasTV[i]==0){
    x=c(x,pizza$stars[i])
  }
  else{
    y=c(y,pizza$stars[i])
  }
}
t.test(x,y,paired = F)
hist(x)
hist(y)# no tv >tv no difference

unique(pizza$Alcohol)
r = lm(pizza$stars~as.factor(pizza$Alcohol),na.action=na.omit )
summary(r)# all alcohol is best

unique(pizza$BikeParking)
x=c()
y=c()
for(i in 1:nrow(pizza)){
  if(is.na(pizza$BikeParking[i])==T){
    next
  }
  else if(pizza$BikeParking[i]==0){
    x=c(x,pizza$stars[i])
  }
  else{
    y=c(y,pizza$stars[i])
  }
}
t.test(x,y,paired = F)
hist(x)
hist(y)#bike parking >no

unique(pizza$WiFi)
r = lm(pizza$stars~as.factor(pizza$WiFi),na.action=na.omit )
summary(r)# free wifi best

library(wordcloud2)
pizza <- read.csv("data/Attributes_pizza.csv", header = TRUE)
pizza <- pizza[,-c(1:10)]
cou<-c()
for(i in 1:ncol(pizza)){
  x <- sum(is.na(pizza[,i])==F)
  cou<-c(cou,x)
}
demopizza<-as.data.frame(colnames(pizza))
demopizza$count<-cou
colnames(demopizza)<-c('Attributes','Frequency')
demopizza$Attributes <- gsub("Restaurants","",demopizza$Attributes)
demopizza$Attributes <- gsub("Business","",demopizza$Attributes)
wordcloud2(demopizza , size = 0.2,color = "random-light", backgroundColor = "white")

pizza <- read.csv("data/Attributes_pizza.csv", header = TRUE)
pizza <- pizza[,-c(1,4:8,46:52)]
pizza <- pizza[,-c(1,2,4)]

datafun <- function(x,y=pizza$stars){
  t<-c();f<-c()
  for(i in 1:nrow(pizza)){
    if(is.na(x[i])==T){
      next
    }
    else if(x[i]=="True"){
      t<-c(t,y[i])
    }
    else {
      f<-c(f,y[i])
    }
  }
  mydata <- data.frame( 
    Group = c(rep("TRUE",length(t)), rep("FALSE",length(f))),
    Frequency = c(t, f)
  )
  return(mydata)
}

ggboxplot(datafun(pizza$GoodForKids), x="Group", y = "Frequency", color = "Group", palette = "jco",add = "jitter",short.panel.labs = FALSE)+
  ylim(0,6)+
  stat_compare_means(method = "t.test",label.y=100)
ggboxplot(datafun(pizza$RestaurantsDelivery), x="RestaurantsDelivery", y = "Frequency", color = "Group", palette = "jco",add = "jitter",short.panel.labs = FALSE)+
  ylim(0,6)+
  stat_compare_means(method = "t.test",label.y=100)
t<-c();f<-c();g<-c();h<-c()
for(i in 1:nrow(pizza)){
  if(is.na(pizza$RestaurantsPriceRange2[i])==T){
    next
  }
  else if(pizza$RestaurantsPriceRange2[i]==1){
    t<-c(t,pizza$stars[i])
  }
  else if(pizza$RestaurantsPriceRange2[i]==2){
    f<-c(f,pizza$stars[i])
  }
  else if(pizza$RestaurantsPriceRange2[i]==3){
    g<-c(g,pizza$stars[i])
  }
  else {
    h<-c(h,pizza$stars[i])
  }
}
mydata <- data.frame( 
  Price = c(rep("Level_1",length(t)), rep("Level_2",length(f)),rep("Level_3",length(g)),rep("Level_4",length(h))),
  Frequency = c(t, f, g, h)
)
ggboxplot(mydata, x="Price", y = "Frequency", color = "Price", palette = "jco",add = "jitter",short.panel.labs = FALSE)+
  ylim(0.5,5.5)














