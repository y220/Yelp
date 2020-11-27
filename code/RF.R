rm(list = ls())
library(rpart)
library(maptree)
pizza <- read.csv("data/Attributes_pizza.csv")
pizza <- pizza[,-c(1,4:8,46:52)]
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
set.seed(1234) 
pizza <- pizza[,-c(1,2,4)]
train<-sample(nrow(pizza),0.7*nrow(pizza)) 
tdata<-pizza[train,] 
vdata<-pizza[-train,] 
fit <- rpart(formula = stars ~ ., method='anova', data = tdata)
fit
printcp(fit)
library(rpart.plot)
rpart.plot(fit,branch=1,type=2, fallen.leaves=T,cex=0.8, sub="Tree")
predtree<-predict(fit,newdata=vdata,type="vector") 
vdata$stars-predtree

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
cbind(pizza$stars,pizza$RestaurantsAttire)
unique(pizza$RestaurantsAttire)

r = lm(pizza$stars~as.factor(pizza$RestaurantsAttire),na.action=na.omit )
summary(r)

