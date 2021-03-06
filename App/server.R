# Define server logic required to draw a histogram ----
library(shiny)
library(leaflet)
suppressPackageStartupMessages(library(googleVis))
source("../Scripts/prepare_data.R")

server <- function(session,input, output) {
  
  #OUTPUT FOR TAB1
  
  output$text_feature_selected <- renderText({
    paste("You have selected the feature :",input$features)
  })
  
  output$text1 <- renderText({
    paste("You have selected the cities :",paste(input$checkGroup_city, collapse = ", "))
  })
  
  
  
  mydata_cities_selected_tab1 <- reactive({#the data is filtered by the cities selected by the user
    mydata %>% filter(city %in% str_split(paste0(input$checkGroup_city, collapse = ","),",", simplify = TRUE),!is.na(bedrooms))
  })
  
  mydata_filtered_by_date_tab1 <- reactive({ 
    mydata_cities_selected_tab1() %>% #the data is filtered by date and cities selected by the user
      filter(date >= input$date_range[1], date <= input$date_range[2])
  })
  
  
  min_date <- reactive({
    min(as.vector((mydata_cities_selected_tab1()%>% select(date))[,1]))
  })
  
  max_date <- reactive({
    max(as.vector((mydata_cities_selected_tab1()%>% select(date))[,1]))
  })
  
  observeEvent(
    input$checkGroup_city,
    updateDateRangeInput(session, "date_range","Date range", 
                         start  = min_date(),
                         end    = max_date(),
                         min    = min_date(),
                         max    = max_date()
    )
  )
  
  observeEvent(
    input$features,
    switch(
      input$features,
      availability_30 = updateSliderInput(session, "slider", 
                                          min = min(mydata_filtered_by_date_tab1()$availability_30), 
                                          max = max(mydata_filtered_by_date_tab1()$availability_30),
                                          value=c(min(mydata_filtered_by_date_tab1()$availability_30),max(mydata_filtered_by_date_tab1()$availability_30))
                                          ), 
      revenue_30 = updateSliderInput(session, "slider", 
                                     min = min(mydata_filtered_by_date_tab1()$revenue_30), 
                                     max = max(mydata_filtered_by_date_tab1()$revenue_30),
                                     value=c(min(mydata_filtered_by_date_tab1()$revenue_30),max(mydata_filtered_by_date_tab1()$revenue_30))
                                     ),
      price = updateSliderInput(session, "slider", 
                                min = min(mydata_filtered_by_date_tab1()$price), 
                                max = max(mydata_filtered_by_date_tab1()$price),
                                value=c(min(mydata_filtered_by_date_tab1()$price),max(mydata_filtered_by_date_tab1()$price))
                                )
      )
    )
  
  
  output$my_table <- renderTable({
    if (input$dimension == "None"){
      if (input$aggreg == "Average") {
        mydata_filtered_by_date_tab1()%>%
          group_by(city) %>%
          summarise(avg = mean(switch(input$features,
                                      availability_30 = availability_30,
                                      revenue_30 = revenue_30,
                                      price = price)))
      }
      else if(input$aggreg == "Median"){
        mydata_filtered_by_date_tab1()%>% 
          group_by(city) %>%
          summarise(Median = median(switch(input$features,
                                           availability_30 = availability_30,
                                           revenue_30 = revenue_30,
                                           price = price)))
      }
    }
    
    #IF another dimension was selected
    else{
      if (input$aggreg == "Average") {
        mydata_filtered_by_date_tab1()%>%
          group_by(city, dimension =switch(input$dimension, 
                               Room_Type = room_type, 
                               nb_Bedrooms = bedrooms,
                               Neighborhood = neighbourhood_cleansed)) %>%
          summarise(avg = mean(switch(input$features,
                                      availability_30 = availability_30,
                                      revenue_30 = revenue_30,
                                      price = price)))
      }
      else if(input$aggreg == "Median"){
        mydata_filtered_by_date_tab1()%>%
          group_by(city,dimension =switch(input$dimension,
                               Room_Type = room_type, 
                               nb_Bedrooms = bedrooms,
                               Neighborhood = neighbourhood_cleansed)) %>%
          summarise(Median = median(switch(input$features,
                                           availability_30 = availability_30,
                                           revenue_30 = revenue_30,
                                           price = price)))
      }
    }
    
   
    })#END OF TABLE
  
  output$plot_tab1 <- renderPlot({
    if(input$aggreg == "Histogram"){ 
      qplot(switch(input$features,
                   availability_30 = availability_30,
                   revenue_30 = revenue_30,
                   price = price),data=mydata_filtered_by_date_tab1(),geom="histogram", xlab=input$features, 
            fill=switch(input$dimension,
                        None = NULL, 
                        Room_Type = room_type, 
                        nb_Bedrooms = bedrooms,
                        Neighborhood = neighbourhood_cleansed))+ 
        facet_grid(.~city) + guides(fill=guide_legend(title=input$dimension)) +xlim(c(input$slider[1]-5,input$slider[2]+5))
    }
    
    else if(input$aggreg == "Density"){
      if (input$dimension == "None"){
        qplot(switch(input$features,
                     availability_30 = availability_30,
                     revenue_30 = revenue_30,
                     price = price),
              data=mydata_filtered_by_date_tab1(),geom="density", xlab=input$features, 
              color=city) +coord_cartesian(xlim=c(input$slider[1],input$slider[2]))
      }
      else{
        qplot(switch(input$features,
                     availability_30 = availability_30,
                     revenue_30 = revenue_30,
                     price = price),
              data=mydata_filtered_by_date_tab1(),geom="density", xlab=input$features, 
              color=switch(input$dimension,
                           None = NULL, 
                           Room_Type = room_type, 
                           nb_Bedrooms = bedrooms,
                           Neighborhood = neighbourhood_cleansed))+ 
          facet_grid(.~city) + guides(color=guide_legend(title=input$dimension))+
          coord_cartesian(xlim=c(input$slider[1],input$slider[2]))
      }
     
    }
    
    else if(input$aggreg == "Boxplot"){
      if (input$dimension == "None"){
        p <- ggplot(mydata_filtered_by_date_tab1(), aes(city, switch(input$features,
                                           availability_30 = availability_30,
                                           revenue_30 = revenue_30,
                                           price = price)))
        p + geom_boxplot(aes(colour = city), outlier.shape = NA) +
          scale_y_continuous(limits = quantile(switch(input$features,
                                                      availability_30 = mydata_filtered_by_date_tab1()$availability_30,
                                                      revenue_30 = mydata_filtered_by_date_tab1()$revenue_30,
                                                      price = mydata_filtered_by_date_tab1()$price), c(0.1, 0.9), na.rm = T))+
          ylab(input$features)
      }
      else{
        p <- ggplot(mydata_filtered_by_date_tab1(), aes(switch(input$dimension,
                                     None = NULL, 
                                     Room_Type = room_type, 
                                     nb_Bedrooms = bedrooms,
                                     Neighborhood = neighbourhood_cleansed),
                              switch(input$features,
                                           availability_30 = availability_30,
                                           revenue_30 = revenue_30,
                                           price = price)))
        p + geom_boxplot(aes(colour = city), outlier.shape = NA) +
          scale_y_continuous(limits = quantile(switch(input$features,
                                                      availability_30 = mydata_filtered_by_date_tab1()$availability_30,
                                                      revenue_30 = mydata_filtered_by_date_tab1()$revenue_30,
                                                      price = mydata_filtered_by_date_tab1()$price), c(0.1, 0.9), na.rm = T))+
          ylab(input$features) + xlab(input$dimension)+ 
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
      }
      
      }
  })#END OF PLOT_TAB1
  
  
  
  output$imgTab1 <- renderImage({
    return(list(src = "img/tab1.PNG",height = 400, width = 800,
         filetype = "image/png",align = "center",
         alt = "tab1"))
  }, deleteFile = FALSE)
  
  output$imgTab2 <- renderImage({
    return(list(src = "img/tab2.png",height = 400, width = 800,
                filetype = "image/png",align = "center",
                alt = "tab1"))
  }, deleteFile = FALSE)
  
  output$imgTab2Map <- renderImage({
    return(list(src = "img/tab2_map.png",height = 400, width = 800,
                filetype = "image/png",align = "center",
                alt = "tab1"))
  }, deleteFile = FALSE)
  
  
  
  
  
  
  
  ###############################################################################################################################
  #OUTPUT FOR TAB2
  
  mydata_cities_selected_tab2 <- reactive({
    #the data is filtered by the cities selected by the user
    mydata %>% 
      filter( city == input$select_city, date == input$select_date2)
  })
  
  output$mymap <- renderLeaflet({
    my_data <- mydata_cities_selected_tab2()
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
    DATA <- mydata_cities_selected_tab2() %>% 
      filter(!is.na(bedrooms))
    
    p<-ggplot(DATA, aes(x=switch(input$dimension_tab2, 
                                 Room_Type = room_type,
                                 Property_Type = property_type,
                                 nb_Bedrooms = bedrooms,
                                 Neighborhood = neighbourhood_cleansed)))
    p + geom_bar() + xlab(input$dimension_tab2)+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      ggtitle(paste("The proportion of each",input$dimension_tab2," for the city",input$select_city, "in", input$select_date2 ))
  })
  
  
  
  output$Gvis_columnChart_tab2 <- renderGvis({
    DATA <- mydata_cities_selected_tab2() %>%
      filter(!is.na(bedrooms)) %>% 
      group_by(switch(input$dimension_tab2,
                      Room_Type = room_type,
                      Property_Type = property_type,
                      nb_Bedrooms = bedrooms,
                      Neighborhood = neighbourhood_cleansed)) %>% 
                      count(name = "Total")
    gvisColumnChart(DATA, options=list(height="550px")) 
  })
  
  output$Gvis_pieChart_tab2 <- renderGvis({
    DATA <- mydata_cities_selected_tab2() %>% 
      filter(!is.na(bedrooms)) %>% 
      group_by(switch(input$dimension_tab2, 
                      Room_Type = room_type,
                      Property_Type = property_type, 
                      nb_Bedrooms = bedrooms,
                      Neighborhood = neighbourhood_cleansed)) %>% count(name = "Total")
   P<-  gvisPieChart(DATA, options=list(height="550px") )
   
  })
  
  observeEvent(
    input$select_city,
    updateSelectInput(session, "select_date2","Select a date", 
                      choices = mydata$date[mydata$city==input$select_city]))
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Documentation", tabName="doc", icon = icon("list-alt"),selected = TRUE),
      menuItem("Analysis 1 - Comparing cities", tabName="tab1", icon = icon("bar-chart-o")),
      menuItem("Analysis 2 - Deep dive into city", tabName="tab2", icon = icon("bar-chart-o"))
    )
  })
  
}