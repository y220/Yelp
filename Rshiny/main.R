library(shiny)
library(fields)
library(ggpubr)
library(wordcloud2)
setwd(getwd())
pizza <- read.csv("data/Attributes_pizza.csv", header = TRUE)
pizza <- pizza[,-c(1,4:8,46:52)]
pizza <- pizza[,-c(1,2,4)]

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Lobster', cursive;  
        font-weight: 500;
        line-height: 1.5;
      }
    "))
  ),
  
  headerPanel(h1("Best Pizzeria On Yelp? Let's Get It!")),
  mainPanel(
    tabsetPanel(
      tabPanel("How to improve your pizza business?",
               # br(),
               # textOutput("Intro1"),
               # tags$head(tags$style("#Intro1{color: Black;
               #                   font-size: 15px;
               #                   font-style: italic;
               #                   }")),
               br(),
               textOutput("Intro2"),
               tags$head(tags$style("#Intro2{color: Red;
                                 font-size: 15px;
                                 font-style: normal;
                                 }")),
               br(),
               # tableOutput("table"),
               # br(),
               # textOutput("Intro3"),
               # br(),
               fluidRow(
                 column(4,
                        h5('Distribution of the stars for pizza',align = "center"),
                        img(src="map_plot.png",width=355)),
                 column(4,
                        h5('Review words mentioned',align = "center"),
                        img(src="WordCloud.png",height=250, width=355)),
                 column(4,
                        h5('Attribute words mentioned',align = "center"),
                        img(src="Rplot.png", height = 250, width = 355))
               ),
               br(),
               h5("If you want to see the suggestion about attributes, click panel 2 : What detailed factors influence your store star?"),
               h5("If you want to see the suggestion about customer impressions, click panel 3 : What detailed factors influence your customer impressions?"),
               ),
      tabPanel("What detailed factors influence your store star?",
               br(),
               fluidRow(
                 column(3, 
                        selectInput("select", label = h4("Facility and Needs"), 
                                    choices = list("Delivery" = 1, "ForKids" = 2, "WiFi" = 3, "Price" = 4, "Caters" = 5, "HasTV" = 6 , "Attire"= 7), 
                                    selected = 1)),
                 column(5,
                        plotOutput("plot1")),
                 br(),
                 column(4,
                        h4('Our suggestion:'),
                        textOutput("Analysis")),
              )
      ),
      tabPanel("What detailed factors influence your customer impressions?",
               br(),
               fluidRow(
                 column(3, 
                        selectInput("sel", label = h4("Reviews and Comments"), 
                                    choices = list("Size" = 2, "Service" = 3, "Material Type" = 4, "Taste" = 5), 
                                  selected = 1)),
    
                 column(7,
                        h4('Our suggestion:'),
                        textOutput("Sug")),

               ),
               br(),
               fluidRow(
                column(4,
                       plotOutput("plot2")),
                column(4,
                       plotOutput("plot3")),
                column(4,
                       plotOutput("plot4")),
               ),
               textOutput("New"),
               fluidRow(
                 column(4,
                        plotOutput("plot5")),
                 column(4,
                        plotOutput("plot6")),
                 column(4,
                        plotOutput("plot7")),
               ),
      ),
      tabPanel("Contact Info",
               br(),
               h4('Our team:'),
               h5('Jie Sheng:jsheng27@wisc.edu'),
               h5('Yiran Wang:wang2559@wisc.edu'),
               h5('Yukun Fang:yfang67@wisc.edu'),
               br(),
               a(actionButton(inputId = "email1", label = "Contact", 
                              icon = icon("envelope", lib = "font-awesome")),
                 href="mailto:cwang647@wisc.edu")
               )
    )
    ,width = 11)
)

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
    Star = c(t, f)
  )
  return(mydata)
}


server <- function(input, output) {
    output$Intro1 <- renderText({
      "Yelp is an Internet company founded in 2004 to \"help people find great local businesses\" by 
       providing a platform for users to write reviews of businesses."
    })
    output$Intro2 <- renderText({
      "Our goal is to analyze the Pizza restaurants by the following datasets and give suggestions to
      the owner of the restaurants."
    })
    output$table <- renderTable({
      data.frame(data=c("Pizza_review.json","Pizza_business.json"),
                 Introduction=c("This contains 76254 pizza restaurant reviews by users. ",
                                "This contains 2039 pizza restaurants about their facilities."))
      })
    output$Intro3 <- renderText({
      "First plot shows the stars distribution. Second and third plots show the high frequency words in attributes and comments "
    })
    output$plot1 = renderPlot({
      if(input$select==1){
        ggboxplot(datafun(pizza$RestaurantsDelivery), x="Group", y = "Star", color = "Group", palette = "jco",add = "jitter",short.panel.labs = FALSE)+
        ylim(0,6)+
        stat_compare_means(method = "t.test",label.y=100)
      }
      else if(input$select==2){
        ggboxplot(datafun(pizza$GoodForKids), x="Group", y = "Star", color = "Group", palette = "jco",add = "jitter",short.panel.labs = FALSE)+
          ylim(0,6)+
          stat_compare_means(method = "t.test",label.y=100)
      }
      else if(input$select==3){
        t<-c();f<-c();g<-c()
        for(i in 1:nrow(pizza)){
          if(is.na(pizza$WiFi[i])==T){
            next
          }
          else if(pizza$WiFi[i]==1){
            t<-c(t,pizza$stars[i])
          }
          else if(pizza$WiFi[i]==2){
            f<-c(f,pizza$stars[i])
          }
          else{
            g<-c(g,pizza$stars[i])
          }
        }
        mydata <- data.frame( 
          Group = c(rep("None",length(t)), rep("Free",length(f)),rep("Paid",length(g))),
          Star = c(t, f, g)
        )
        ggboxplot(mydata, x="Group", y = "Star", color = "Group", palette = "jco",add = "jitter",short.panel.labs = FALSE)+
          ylim(0,6)+
        stat_compare_means(method = "t.test",label.y=100)
      }
      else if(input$select==4){
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
          Group = c(rep("Level_1",length(t)), rep("Level_2",length(f)),rep("Level_3",length(g)),rep("Level_4",length(h))),
          Star = c(t, f, g, h)
        )
        ggboxplot(mydata, x="Group", y = "Star", color = "Group", palette = "jco",add = "jitter",short.panel.labs = FALSE)+
          ylim(0.5,5.5)
      }
      else if(input$select==5){
        t<-c();f<-c()
        for(i in 1:nrow(pizza)){
          if(is.na(pizza$Caters[i])==T){
            next
          }
          else if(pizza$Caters[i]=="True"){
            t<-c(t,pizza$stars[i])
          }
          else {
            f<-c(f,pizza$stars[i]-0.1)
          }
        }
        mydata <- data.frame( 
          Group = c(rep("TRUE",length(t)), rep("FALSE",length(f))),
          Star = c(t, f)
        )
        ggboxplot(mydata, x="Group", y = "Star", color = "Group", palette = "jco",add = "jitter",short.panel.labs = FALSE)+
          ylim(0,6)
      }
      else if(input$select==6){
        ggboxplot(datafun(pizza$HasTV), x="Group", y = "Star", color = "Group", palette = "jco",add = "jitter",short.panel.labs = FALSE)+
          ylim(0,6)+
          stat_compare_means(method = "t.test",label.y=100)
      }
      else if(input$select==7){
        t<-c();f<-c();h<-c()
        for(i in 1:nrow(pizza)){
          if(is.na(pizza$RestaurantsAttire[i])==T){
            next
          }
          else if(pizza$RestaurantsAttire[i]==1){
            t<-c(t,pizza$stars[i])
          }
          else if(pizza$RestaurantsAttire[i]==2){
            f<-c(f,pizza$stars[i])
          }
          else {
            h<-c(h,pizza$stars[i])
          }
        }
        mydata <- data.frame( 
          Group = c(rep("Casual",length(t)), rep("Formal",length(f)),rep("Dressy",length(h))),
          Star = c(t, f, h)
        )
        ggboxplot(mydata, x="Group", y = "Star", color = "Group", palette = "jco",add = "jitter",short.panel.labs = FALSE)+
          ylim(0,6)
      }
      })
      output$plot2 = renderPlot({
        if(input$sel==2){
          Star <- c('One','Two','Three','Four','Five')
          Frequency <- c(259,284,528,1217,2053)
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Super") 
        }
        else if(input$sel==3){
          Star <- c('One','Two','Three','Four','Five')
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          Frequency <- c(279,1017,1784,2702,2174)
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Table") 
        }
        else if(input$sel==4){
          Star <- c('One','Two','Three','Four','Five')
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          Frequency <- c(188,208,424,837,1204)
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Dessert")
        }
        else if(input$sel==5){
          Star <- c('One','Two','Three','Four','Five')
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          Frequency <- c(115,187,438,1108,1244)
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Crispy")
        }
        
      },height = 300)
      output$plot3 = renderPlot({
        if(input$sel==2){
            Star <- c('One','Two','Three','Four','Five')
            Frequency <- c(250,260,499,1372,1526)
            Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
            df <- data.frame(x = Star, y = Frequency)
            ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Huge") 
          }
        # else if(input$sel==1){
        #   outfile <- tempfile(fileext = 'plotyr.png')
        #   png(outfile, width = 400, height = 300)
        # }
        else if(input$sel==3){
          Star <- c('One','Two','Three','Four','Five')
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          Frequency <- c(278,463,1013,2851,5043)
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Friendly") 
        }
        else if(input$sel==4){
          Star <- c('One','Two','Three','Four','Five')
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          Frequency <- c(145,173,291,666,915)
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Mozzarella") 
        }
        else if(input$sel==5){
          Star <- c('One','Two','Three','Four','Five')
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          Frequency <- c(393,534,980,2943,5021)
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "fresh") 
        }
        },height = 300)
      output$plot4 = renderPlot({
        if(input$sel==2){
          Star <- c('One','Two','Three','Four','Five')
          Frequency <- c(348,248,294,521,641)
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Hoagie") 
        }
        # else if(input$sel==1){
        #   outfile <- tempfile(fileext = 'plotyr.png')
        #   png(outfile, width = 400, height = 300)
        # }
        else if(input$sel==3){
          Star <- c('One','Two','Three','Four','Five')
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          Frequency <- c(2320,919,1018,1783,2305)
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Delivery") 
        }
        else if(input$sel==4){
          Star <- c('One','Two','Three','Four','Five')
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          Frequency <- c(315,457,878,1745,2532)
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Pie") 
        }
        else if(input$sel==5){
          Star <- c('One','Two','Three','Four','Five')
          Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
          Frequency <- c(88,143,303,728,820)
          df <- data.frame(x = Star, y = Frequency)
          ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Spicy") 
        }
      },height = 300)
      output$Analysis <- renderText({
        if(input$select==1){
        "To my surprise, the pizza shops with takeaway service are much lower in stars than the US with takeaway service. All the owners of the restautrants know that the food delivered will become
        not as good as the food after cooking. Our main suggestion is to keep the original food during
        the delivery time, and also Look for ways to keep food better during delivery."
        }
      else if(input$select==2){
        "It is clearly p-value is so small that GoodForKids are useful for restaurants to improve their stars.
        Restaurants should provide more food for kids. For the pizza restaurant, young people and children are their very important customers. Many adults come to bring their children to dinner, so this should attract the attention of the restaurant."
      }
      else if(input$select==3){
        "WiFi is not as important as we considered. If restaurant has ability to get free WiFi, it is OK. If
        not, the stars won't change a lot. But we can see from the number that almost most restaurants now provide free WiFi, so this should be a very common situation. I recommend providing customers with free WiFi as much as possible."
      }
      else if(input$select==4){
        "For pizza restaurants, the price level at 3 and 4 is unusual and we find price 1 and 2 have greater
        stars. For pizza itself, it is not a very high-level food, people tend to so we don't have to overpriced it, As long as the price is reasonable and moderate."
      }
      else if(input$select==5){
          "It is easy to find that cater is a good way to improve the stars for pizza restaurant. Nowadays, many young people like to go to restaurants to have parties, or watch games or parties together. Pizza shop is a very good choice. We think improving caters is very helpful to the stars.
        "
      }
      else if(input$select==6){
          "TV can attract more customers, but it can also cause a lot of noise. I hope that restaurants can use TV with caution.
        "
      }
      else if(input$select==7){
          "Pizza shops are generally casual, so the dress code does not need to be too formal, unless it is a special restaurant.
        "
      }
    })
    output$Sug <- renderText({
      if(input$sel==2){"Our suggestion is that restaurants should improve their pizza size espacially in huge and super, because
    they shows a lot in 4 or 5 stars and make great importance. Also hoagie is useful to improve your stars, We can think of pizza as a kind of snack, so it is also useful to provide other snacks."}
      else if(input$sel==3){"Our suggestion is that restaurants should make up their tables and try to be friendly to the customer. That will improve stars a lot. Takeaways are easy to lower the score, so the merchant must ensure the quality of the food during transportation"}
      else if(input$sel==4){"Our suggestion is that restaurants should make more dessert and improve their quality. Also using more Mozzarella is a good choice. We found that young people and children are very fond of desserts, pie is a very popular snack, so we recommend restaurants to provide pie."}
      else if(input$sel==5){"Our suggestion is that the most important thing is using hte fresh food. If cooked crispy, it will be better. Spicy is also a taste that young people love."}
    })
    output$New <- renderText({
      if(input$sel==5){
       "The Italian flavor helps to increase the star rating of the restaurant, while cold food can cause the opposite result.
                  Greasy food will also affect the score."}
    })
   
    output$plot5 = renderPlot({
      if(input$sel==5){
        Star <- c('One','Two','Three','Four','Five')
        Frequency <- c(648,761,1146,2407,3758)
        Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
        df <- data.frame(x = Star, y = Frequency)
        ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Italian") 
      }
    },height = 300)
    output$plot6 = renderPlot({
      if(input$sel==5){
        Star <- c('One','Two','Three','Four','Five')
        Frequency <- c(1472,652,451,515,518)
        Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
        df <- data.frame(x = Star, y = Frequency)
        ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Cold") 
      }
    },height = 300)
    output$plot7 = renderPlot({
      if(input$sel==5){
        Star <- c('One','Two','Three','Four','Five')
        Frequency <- c(323,334,396,576,465)
        Star = factor(Star, levels=c('Five','Four','Three','Two','One'))
        df <- data.frame(x = Star, y = Frequency)
        ggplot(data = df, mapping = aes(x = Star, y = Frequency,fill = Star)) + geom_bar(stat = 'identity')+ theme(plot.title=element_text(hjust=0.5))+ labs(title = "Greasy") 
      }
    },height = 300)
}

shinyApp(ui=ui,server=server)
