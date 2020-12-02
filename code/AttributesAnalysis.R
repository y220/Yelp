rm(list = ls())
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














