rm(list = ls())
# load required package for json and read data

library(jsonlite)
library(plyr)
# review = stream_in(file("Data/review_city.json"))
business = stream_in(file("data/business_city.json"))

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

for(i in 1:nrow(pizza_new)){
  if(is.na(pizza_new$RestaurantsAttire[i])==T){
    next
  }
  if(pizza_new$RestaurantsAttire[i]=="'casual'"| pizza_new$RestaurantsAttire[i]=="u'casual'"){
    pizza_new$RestaurantsAttire[i]="causal"
  }
  if(pizza_new$RestaurantsAttire[i]=="'formal'"| pizza_new$RestaurantsAttire[i]=="u'formal'"){
    pizza_new$RestaurantsAttire[i]="formal"
  }
  if(pizza_new$RestaurantsAttire[i]=="'dressy'"| pizza_new$RestaurantsAttire[i]=="u'dressy'"){
    pizza_new$RestaurantsAttire[i]="dressy"
  }
  if(pizza_new$RestaurantsAttire[i]=="None"){
    pizza_new$RestaurantsAttire[i]=NA
  }
}

for(i in 1:nrow(pizza_new)){
  if(is.na(pizza_new$NoiseLevel[i])==T){
    next
  }
  if(pizza_new$NoiseLevel[i]=="'quiet'"| pizza_new$NoiseLevel[i]=="u'quiet'"){
    pizza_new$NoiseLevel[i]="quiet"
  }
  if(pizza_new$NoiseLevel[i]=="'average'"| pizza_new$NoiseLevel[i]=="u'average'" ){
    pizza_new$NoiseLevel[i]="average"
  }
  if(pizza_new$NoiseLevel[i]=="'loud'"| pizza_new$NoiseLevel[i]=="u'loud'"){
    pizza_new$NoiseLevel[i]="loud"
  }
  if(pizza_new$NoiseLevel[i]=="'very_loud'"| pizza_new$NoiseLevel[i]=="u'very_loud'"){
    pizza_new$NoiseLevel[i]="very_loud"
  }
  if(pizza_new$NoiseLevel[i]=="None"){
    pizza_new$NoiseLevel[i]=NA
  }
}

for(i in 1:nrow(pizza_new)){
  if(is.na(pizza_new$Alcohol[i])==T){
    next
  }
  if(pizza_new$Alcohol[i]=="'none'"| pizza_new$Alcohol[i]=="u'none'"){
    pizza_new$Alcohol[i]="no"
  }
  if(pizza_new$Alcohol[i]=="'beer_and_wine'"| pizza_new$Alcohol[i]=="u'beer_and_wine'" ){
    pizza_new$Alcohol[i]="beer_and_wine"
  }
  if(pizza_new$Alcohol[i]=="'full_bar'"| pizza_new$Alcohol[i]=="u'full_bar'"){
    pizza_new$Alcohol[i]="full_bar"
  }
  if(pizza_new$Alcohol[i]=="None"){
    pizza_new$Alcohol[i]=NA
  }
}

for(i in 1:nrow(pizza_new)){
  if(is.na(pizza_new$WiFi[i])==T){
    next
  }
  if(pizza_new$WiFi[i]=="'no'"| pizza_new$WiFi[i]=="u'no'"){
    pizza_new$WiFi[i]="no"
  }
  if(pizza_new$WiFi[i]=="'free'"| pizza_new$WiFi[i]=="u'free'" ){
    pizza_new$WiFi[i]="free"
  }
  if(pizza_new$WiFi[i]=="'paid'"| pizza_new$WiFi[i]=="u'paid'"){
    pizza_new$WiFi[i]="paid"
  }
  if(pizza_new$WiFi[i]=="None"){
    pizza_new$WiFi[i]=NA
  }
}
write.csv(pizza_new,"data/Attributes_pizza.csv")
pizza <- read.csv("data/Attributes_pizza.csv")
pizza <- pizza[,-c(1,4:8,46:52)]
















