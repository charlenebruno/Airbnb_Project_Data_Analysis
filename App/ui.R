library(shiny)
library(leaflet)
library(shinydashboard)

source("prepare_data.R")

ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title = "Airbnb Analysis"),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="doc",
              h1("Documentation"),
              br(),br(),
              p("Here is a tutorial to explain how to use the Rshiny app. 
                It is composed of two principal tabs. 
                The first one is dedicated to the comparison between cities on different features. 
                The second one deeps dive in more details about one specific city."),
              br(),br(),
              
              ),
      tabItem(tabName = "tab1",  # Sidebar layout with input and output definitions ----
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
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
                           dateRangeInput("date_range", h3("Date range"))),
                    conditionalPanel(
                      condition = "input.aggreg == 'Histogram' | input.aggreg == 'Density' ",
                      column(10,
                             sliderInput("slider", "Zoom in/out the graph",
                                         min = 0, max = 100, value = c(0, 75))))
                  )
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  h3("Comparing cities"),br(),
                  textOutput("text1"),
                  textOutput("text_feature_selected"),
                  tableOutput("my_table"),
                  conditionalPanel(
                    condition = "input.aggreg == 'Histogram' | input.aggreg == 'Density' | input.aggreg == 'Boxplot'",
                  box(
                    title = "input.aggreg",
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput(outputId = "plot_tab1", width = 450)
                    )
                  )
                )
              ) ),
      tabItem(tabName = "tab2",
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
                               h3("Select a dimension"),
                               choices = c("Room_Type", "Property_Type", "nb_Bedrooms", "Neighborhood"),
                               selected = "Room_Type")
                ),
                # Main panel for displaying outputs ----
                mainPanel(
                  #textOutput("text2"),
                  leafletOutput("mymap"),
                  br(),br(),br()
                )
              ),
              splitLayout(
                htmlOutput( "Gvis_columnChart_tab2"),
                htmlOutput( "Gvis_pieChart_tab2")
              )
              
              
      ) )
    )
  )