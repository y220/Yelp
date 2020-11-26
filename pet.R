rm(list = ls())
# load required package for json and read data

library(jsonlite)
library(plyr)
# review = stream_in(file("Data/review_city.json"))
business = stream_in(file("Data/business_city.json"))

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

Ambience

