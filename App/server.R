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
    summarise(avg = mean(availability_30))%>%
    filter(city %in% str_split(paste0(input$checkGroup_city, collapse = ","),",", simplify = TRUE))
    })
  
  output$text1 <- renderText({
    # cities <- input$checkGroup_country
    # cities <- paste(input$cities, collapse = ", ")
    # paste("You have selected :",z)
    #print(str_split(paste0(input$checkGroup_city, collapse = ","),",", simplify = TRUE))
    paste("You have selected :",paste(input$checkGroup_city, collapse = ", "))
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