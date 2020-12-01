# Descriptive Analysis
# Step1: Word Cloud
words = read.csv("word_frequency.csv",header = T)
words = words[-which(words$words %in% c('pizza','good','great','like','recommend','amazing','perfect','favorite','happy','nice','special','awesome','enjoyed','delicious','love','tasty','enjoy','cool','fantastic','fun','bad','excellent','disappointed','perfectly','terrible','wonderful','loved')),]
words = words[order(-abs(words$avgStar_from_grandmean)),]
positiveWords = words[which(words$type=='Positive'),1:2]
negativeWords = words[which(words$type=='Negative'),1:2]
totalWords = words[,1:2]
library('devtools')
devtools::install_github('lchiffon/wordcloud2')
library("wordcloud2")
# js_color_fun = "function (word, weight) {
#   return (weight > 5000) ? '#f02222' : '#c09292';
# }"
wordcloud2(totalWords, size = 2, minSize = 0, gridSize =  0,  
           fontFamily = 'Arial', fontWeight = 'normal',  
           color = 'random-dark', backgroundColor = "white",  
           minRotation = -pi/2, maxRotation = -pi/2, rotateRatio = 0.4,  
           shape = 'circle', ellipticity = 0.65, widgetsize = NULL)
wordcloud2(positiveWords, size = 2, minSize = 0, gridSize =  0,  
           fontFamily = NULL, fontWeight = 'normal',  
           color = 'random-light', backgroundColor = "white",  
           minRotation = -pi/2, maxRotation = -pi/2, rotateRatio = 0.4,  
           shape = 'circle', ellipticity = 0.65, widgetsize = NULL)
wordcloud2(negativeWords, size = 1.3, minSize = 0, gridSize =  0,  
           fontFamily = NULL, fontWeight = 'normal',  
           color = 'random-dark', backgroundColor = "white",  
           minRotation = -pi/2, maxRotation = -pi/2, rotateRatio = 0.4,  
           shape = 'circle', ellipticity = 0.65, widgetsize = NULL)
# wordcloud2(words, figPath = pizza_pic, size = 1,color = "random-light") 
letterCloud(totalWords, word = "Pizza", wordSize = 1.5,color = 'random-dark',backgroundColor = "white") 

# Step2: Map plot
library("maps")
library(ggmap)
library(sp)
library(maptools)
library(maps)
business = read.csv("business.csv",header = T)
states = map_data("state")
lon = business$longitude
lat = business$latitude
# name of states
fourstates = data.frame(region = c("Wisconsin","Ohio","Pennsylvania","Illinois"),lat = c(44.5597,40.10187,40.87924,39.37624),lon = c(-89.7,-82.49665,-77.60149,-89.43427) )
# state borders
mapstate<-borders(database = "state",regions = c("wisconsin","ohio","pennsylvania","illinois"),colour = "gray50",fill="coral",alpha = 0.05)
# county borders
mapcounty<-borders(database = "county",regions = c("wisconsin","ohio","pennsylvania","illinois"),colour = "gray88",fill="white")
mp <- ggplot()+  
  mapcounty+ 
  mapstate+
  xlim(-93,-74.5)+ylim(37,47) +
  geom_text(data = fourstates, aes(x = lon , y = lat, label = region),color='black', family = "Arial")+
  geom_point(aes(x = lon,y = lat,color=business$stars),alpha = 0.4)+
  # scale_fill_gradient2()+
  scale_colour_gradient(low ="yellow",high="red")+
   scale_size(range=c(1,1))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
      axis.text.y=element_blank(),axis.ticks=element_blank(), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(),legend.position="none", 
      # panel.background=element_blank(),
      # panel.border=element_blank(),
      # panel.grid.major=element_blank(), 
      # panel.grid.minor=element_blank(),
      # plot.background=element_blank()
      ) 
mp

