library(shiny)
library(leaflet)
library(shinydashboard)

source("../Scripts/prepare_data.R")

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
              br(),
              h4("Here is a tutorial to explain you how to use the Rshiny app. 
                It is composed of two principal tabs. 
                The first one is dedicated to the comparison between cities on different features. 
                The second one deeps dive in more details about one specific city."),
              br(),br(),
              h2("Analysis 1 - Comparing cities"),
              imageOutput("imgTab1", inline = TRUE ),
              h3("1. Choose your aggregation"),
                h4(em("Average")," and ", em("Median"), " will display a table. It will compute the average or the median of the feature you chose for each city. Whereas" ,
                  em("Histogram, Density and Boxplot"), "will display the corresponded plot."),
              h4("In ", em("Histogram and Density"), " you can zoom in and zoom out, thanks to the slider on the axe X."),
              br(),
              h3("2. Choose your cities"),
                h4("In the checkbox, choose as many as cities you want to compare between them."),
              br(),
              h3("3. Choose your feature and dimension"),
                h4("Choose an additional dimension to analysis the data of your cities"),
              br(),
              h3("4. Filter the date"),
                h4("The date range you choose will filter the data and display what you want."),
              br(),
              h2("Analysis 2 - Deep dive into city"),
              imageOutput("imgTab2", inline = TRUE ),
              div(h4("Following the same principal as before, you will only be able to select one city and one date to analyse here 
                     (You can only choose one date because it will slow down the latency to load the data).")),
              br(),
              imageOutput("imgTab2Map", inline = TRUE ),
              h4("In the map, there are many housings represented by a blue marker. 
                  For more details about the housing, click on the link. It will redirect you to the official Airbnb website. ")
              
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
                  # box(
                  #   title = "input.aggreg",
                  #   status = "primary", solidHeader = TRUE,
                  #   collapsible = TRUE,
                    plotOutput(outputId = "plot_tab1")
                    # )
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