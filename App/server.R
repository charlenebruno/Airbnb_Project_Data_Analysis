# Define server logic required to draw a histogram ----
library(shiny)
library(leaflet)
source("prepare_data.R")

server <- function(session,input, output) {
  
  # output$distPlot <- renderPlot({
  # 
  #   x    <- faithful$waiting
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #   hist(x, breaks = bins, col = "#75AADB", border = "white",
  #        xlab = "Waiting time to next eruption (in mins)",
  #        main = "Histogram of waiting times")
  # 
  # })
  
  
  output$my_table <- renderTable({
    if (input$dimension == "None"){
      if (input$aggreg == "Average") {
        mydata %>% 
          group_by(city) %>%
          summarise(avg = mean(switch(input$features,
                                      availability_30 = availability_30,
                                      revenue_30 = revenue_30,
                                      price = price)))%>%
          filter(city %in% str_split(paste0(input$checkGroup_city, collapse = ","),",", simplify = TRUE))
      }
      else if(input$aggreg == "Median"){
        mydata %>% 
          group_by(city) %>%
          summarise(Median = median(switch(input$features,
                                           availability_30 = availability_30,
                                           revenue_30 = revenue_30,
                                           price = price)))%>%
          filter(city %in% str_split(paste0(input$checkGroup_city, collapse = ","),",", simplify = TRUE))
      }
    }
    
    #IF another dimension was selected
    else{
      if (input$aggreg == "Average") {
        mydata %>% 
          filter(!is.na(bedrooms)) %>%
          group_by(city, dimension =switch(input$dimension, 
                               Room_Type = room_type, 
                               nb_Bedrooms = bedrooms,
                               Neighborhood = neighbourhood_cleansed)) %>%
          summarise(avg = mean(switch(input$features,
                                      availability_30 = availability_30,
                                      revenue_30 = revenue_30,
                                      price = price)))%>%
          filter(city %in% str_split(paste0(input$checkGroup_city, collapse = ","),",", simplify = TRUE))
      }
      else if(input$aggreg == "Median"){
        mydata %>% 
          filter(!is.na(bedrooms)) %>%
          group_by(city,dimension =switch(input$dimension,
                               Room_Type = room_type, 
                               nb_Bedrooms = bedrooms,
                               Neighborhood = neighbourhood_cleansed)) %>%
          summarise(Median = median(switch(input$features,
                                           availability_30 = availability_30,
                                           revenue_30 = revenue_30,
                                           price = price)))%>%
          filter(city %in% str_split(paste0(input$checkGroup_city, collapse = ","),",", simplify = TRUE))
      }
    }
    
   
    })
  
  output$text_feature_selected <- renderText({
    paste("You have selected the feature :",input$features)
  })
  
  output$text1 <- renderText({
    paste("You have selected the cities :",paste(input$checkGroup_city, collapse = ", "))
    })
  
  output$plot_tab1 <- renderPlot({
    DATA <- mydata %>% 
      filter(city %in% str_split(paste0(input$checkGroup_city, collapse = ","),",", simplify = TRUE), !is.na(bedrooms))
    
    if(input$aggreg == "Histogram"){ 
      qplot(switch(input$features,
                   availability_30 = availability_30,
                   revenue_30 = revenue_30,
                   price = price),data=DATA,geom="histogram", xlab=input$features, 
            fill=switch(input$dimension,
                        None = NULL, 
                        Room_Type = room_type, 
                        nb_Bedrooms = bedrooms,
                        Neighborhood = neighbourhood_cleansed))+ 
        facet_grid(.~city) + guides(fill=guide_legend(title=input$dimension))
    }
    
    else if(input$aggreg == "Density"){
      if (input$dimension == "None"){
        qplot(switch(input$features,
                     availability_30 = availability_30,
                     revenue_30 = revenue_30,
                     price = price),
              data=DATA,geom="density", xlab=input$features, 
              color=city)
      }
      else{
        qplot(switch(input$features,
                     availability_30 = availability_30,
                     revenue_30 = revenue_30,
                     price = price),
              data=DATA,geom="density", xlab=input$features, 
              color=switch(input$dimension,
                           None = NULL, 
                           Room_Type = room_type, 
                           nb_Bedrooms = bedrooms,
                           Neighborhood = neighbourhood_cleansed))+ 
          facet_grid(.~city) + guides(color=guide_legend(title=input$dimension))
      }
     
    }
    
    else if(input$aggreg == "Boxplot"){
      if (input$dimension == "None"){
        p <- ggplot(DATA, aes(city, switch(input$features,
                                           availability_30 = availability_30,
                                           revenue_30 = revenue_30,
                                           price = price)))
        p + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
          scale_y_continuous(limits = quantile(switch(input$features,
                                                      availability_30 = DATA$availability_30,
                                                      revenue_30 = DATA$revenue_30,
                                                      price = DATA$price), c(0.1, 0.9), na.rm = T))+
          ylab(input$features)
      }
      else{
        p <- ggplot(DATA, aes(switch(input$dimension,
                                     None = NULL, 
                                     Room_Type = room_type, 
                                     nb_Bedrooms = bedrooms,
                                     Neighborhood = neighbourhood_cleansed),
                              switch(input$features,
                                           availability_30 = availability_30,
                                           revenue_30 = revenue_30,
                                           price = price)))
        p + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
          scale_y_continuous(limits = quantile(switch(input$features,
                                                      availability_30 = DATA$availability_30,
                                                      revenue_30 = DATA$revenue_30,
                                                      price = DATA$price), c(0.1, 0.9), na.rm = T))+
          ylab(input$features) + xlab(input$dimension)+ facet_wrap(~ city)+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
      }
      
      }
  })
  
  output$text2 <- renderText(paste("You have selected",input$select_city))

  output$mymap <- renderLeaflet({
    CITY <- input$select_city
    DATE <- input$select_date2
    my_data <- filter(mydata, city == CITY, date == DATE)
    my_data %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions(),popup=
                   paste0(
                          "<b>Price: </b>"
                          , paste0("$",as.character(my_data$price))
                          , "<br>"
                          , "<b>Bed(s): </b>"
                          , my_data$beds
                          , "<br>"
                          , "<a href='https://www.airbnb.com/rooms/"
                          , my_data$id
                          , "'>"
                          , "Go to Airbnb</a>"
                        ))
  })
  
  
  output$plot_tab2 <- renderPlot({
    DATA <- mydata %>% 
      filter(city == input$select_city, !is.na(bedrooms), date == input$select_date2)
    
    p<-ggplot(DATA, aes(x=switch(input$dimension_tab2, 
                                 Room_Type = room_type, 
                                 nb_Bedrooms = bedrooms,
                                 Neighborhood = neighbourhood_cleansed)))
    p + geom_bar() + xlab(input$dimension_tab2)+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      ggtitle(paste("The proportion of each",input$dimension_tab2," for the city",input$select_city, "in", input$select_date2 ))
  })
  
  observeEvent(
    input$select_city,
    updateSelectInput(session, "select_date2","Select a date", 
                      choices = mydata$date[mydata$city==input$select_city]))
  
  observeEvent(
    input$select_city,
    updateDateRangeInput(session, "date_range","Date range", 
                      start  = min(as.vector((mydata %>% filter(city %in% str_split(paste0(input$checkGroup_city, collapse = ","),",", simplify = TRUE))%>% select(date))[,1])),
                      end    = max(mydata$date),
                      min    = min(mydata$date),
                      max    = max(mydata$date)
                      )
    )
  
}