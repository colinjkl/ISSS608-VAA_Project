#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

pacman::p_load(
  "tidyverse"
)

df <- read_csv("data/time_series_data.csv")
station_names <- unique(df$Station)
target_vars <- grep("_", colnames(df), value = T)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Navigation panel on left
    navlistPanel(
      id = "tabset",
      widths = c(2,10),
      "EDA",
      tabPanel("Distribution"),
      tabPanel("Country by Country Comparison"),
      "Explanatory Model",
      tabPanel("Linear Regression"),
      "Time Series Forecast",
      tabPanel("Holt Winters",
               
               # Left column
               column(
                 2,
                 
                 # Model Initialization
                 fluidRow(
                   selectInput("station",
                               "Choose Station:",
                               choices = station_names,
                               multiple = F),
                   selectInput("target_var",
                               "Choose Weather Variable:",
                               choices = target_vars,
                               multiple = F),
                   div(actionButton("initButton", "Initialize"), style = "float:right")
                 ),
                 
                 # Model Tuning
                 fluidRow(
                   sliderInput("periodRange", 
                               "Select Period:", 
                               min = as.Date("2016-02-01"), max = as.Date("2020-02-01"), 
                               value = c(as.Date("2016-02-01"), as.Date("2020-02-01")),
                               timeFormat = "%Y-%m"
                   ),
                   
                   tags$head(tags$style(HTML("div#inline label { width: 32%; }
                               div#inline input { display: inline-block; width: 68%;}"))),
                   tags$head(
                     tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; }
                                   #inline .form-group { display: table-row;}")),
                   
                   tags$div(
                     id = "inline",
                     style = "width:130%;",
                     #class = "inline",
                     numericInput("trainTestSplitInput",
                                "Alpha :",
                                value = 0.8,
                                min = 0.5,
                                max = 1.0,
                                step = 0.05)),
                   numericInput("alphaInput",
                                "Alpha: (between 0.0 - 1.0)",
                                value = 0.8,
                                min = 0.0,
                                max = 1.0,
                                step = 0.05),
                   numericInput("betaInput",
                                "Beta: (between 0.0 - 1.0)",
                                value = 0.8,
                                min = 0.0,
                                max = 1.0,
                                step = 0.05),
                   numericInput("gammaInput",
                                "Gamma: (between 0.0 - 1.0)",
                                value = 0.8,
                                min = 0.0,
                                max = 1.0,
                                step = 0.05)
                   
                   
                 ),
                 fluidRow(
                   h4("Parameters")
                   
                 )
                   
                 
               ),
               
               # Show a plot of the generated distribution
               column(
                 5,
                 plotOutput("distPlot")
               ),
               
               # Show a plot of the generated distribution
               column(
                 5,
                 "Hello World"
               )
      )
    ),


)
