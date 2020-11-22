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
  output$my_table <- renderTable({mydata %>% 
    group_by(city) %>%
    summarise(avg = mean(switch(input$features,
                                availability_30 = availability_30,
                                revenue_30 = revenue_30,
                                price = price)))%>%
    filter(city %in% str_split(paste0(input$checkGroup_city, collapse = ","),",", simplify = TRUE))
    })
  
  output$text_feature_selected <- renderText({
    paste("You have selected :",input$features)
  })
  
  output$text1 <- renderText({
    paste("You have selected :",paste(input$checkGroup_city, collapse = ", "))
    })
  
  output$histogram_price <- renderPlot({
    city <- mydata %>% group_by(city) %>%filter(city=="barcelona")
    x <- city$price
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #breaks = bins
    hist(x, col = "#75AADB", border = "white",
         xlab = "price",
         main = "Histogram price")
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
  
  observeEvent(
    input$select_city,
    updateSelectInput(session, "select_date2","Select a date", 
                      choices = mydata$date[mydata$city==input$select_city]))
}