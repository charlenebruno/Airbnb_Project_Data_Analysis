library(shiny)
library(leaflet)

source("prepare_data.R")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Hello Shiny!"),
  
  tabsetPanel(type = "tabs",
              tabPanel("Tab 1  (Analysis 1 - Comparing cities)",
                       # Sidebar layout with input and output definitions ----
                       sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           
                           # Input: Slider for the number of bins ----
                           # sliderInput(inputId = "bins",
                           #             label = "Number of bins:",
                           #             min = 1,
                           #             max = 50,
                           #             value = 30),
                           
                           checkboxGroupInput("checkGroup_city",
                                              h3("Choose a city"),
                                              inline = TRUE,
                                              choices = unique(mydata$city),
                                              selected = 1)
                           
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                           h3("Hello Wolrd !"),br(),
                           textOutput("text1"),
                           # Output: Histogram ----
                           # plotOutput(outputId = "distPlot"),
                           # 
                           # textOutput("urls_data"),
                           # DT::dataTableOutput("mytable")
                           tableOutput("my_table")
                         )
                       )
              ),#End of Tab1
              tabPanel("Tab 2  (Analysis 2 - Deep dive into a city)", 
                       #DT::dataTableOutput("mytable"),
                       sidebarLayout(
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           selectInput("select_city",
                                      h3("Select a city"),
                                      choices = unique(mydata$city),
                                      selected = 1),
                           selectInput("select_date2",
                                       "Select a date",choices ="", selected = "")
                         ),
                           
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                           textOutput("text2"),
                           # 
                           leafletOutput("mymap")
                         )
                       )
                       
              )#End of Tab2
  )
  
)