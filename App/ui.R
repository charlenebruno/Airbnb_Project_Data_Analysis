library(shiny)
library(leaflet)

source("prepare_data.R")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Airbnb Analysis"),
  
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
                           
                           fluidRow(
                             
                             column(4,
                                    checkboxGroupInput("checkGroup_city",
                                                       h3("Choose a city"),
                                                       choices = unique(mydata$city),
                                                       selected = "bordeaux")
                                    ),

                             column(4,

                                    radioButtons("aggreg", h3("Aggregation Type"),
                                                 choices = c("Average", "Median",
                                                              "Histogram","Density",
                                                             "Boxplot"),selected = "Average")
                                    )
                        
                           ),
                           
                           
                           fluidRow(
                             column(4,
                                    radioButtons("features", h3("Features"),
                                                 choices = c("availability_30", "revenue_30",
                                                                "price"),selected = "availability_30")
                             ),
                             column(4,
                                    radioButtons("dimension",
                                                       h3("Add a dimension"),
                                                       choices = c("None", "Room_Type", "nb_Bedrooms", "Neighborhood"),
                                                       selected = "None")
                             ),

                           ),
                           fluidRow(
                             column(10,
                                    dateRangeInput("date_range", h3("Date range"), start  = min(mydata$date),
                                                   end    = max(mydata$date),
                                                   min    = min(mydata$date),
                                                   max    = max(mydata$date))),
                             column(10,
                                    sliderInput("slider2", "",
                                                min = 0, max = 100, value = c(25, 75)))
                             )
                           
                           
                           

                           
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                           
                           
                           h3("Hello Wolrd !"),br(),
                           textOutput("text1"),
                           textOutput("text_feature_selected"),
                           tableOutput("my_table"),
                           plotOutput(outputId = "plot_tab1")
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
                                       "Select a date",choices ="", selected = ""),
                           radioButtons("dimension_tab2",
                                        h3("Select a dimension for the histogram"),
                                        choices = c("Room_Type", "nb_Bedrooms", "Neighborhood"),
                                        selected = "Room_Type")
                         ),
                           
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                           textOutput("text2"),
                            
                           leafletOutput("mymap"),
                           plotOutput(outputId = "plot_tab2")
                         )
                       )
                       
              )#End of Tab2
  )
  
)