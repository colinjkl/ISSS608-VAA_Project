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
  shiny,
  shinyjs,
  tidyverse,
  fable,
  tsibble,
  feasts,
  patchwork,
  plotly
)

df <- read_csv("data/dengue_climate_joined_by_week_transformed.csv")

df$Date <- lubridate::ymd(lubridate::parse_date_time(paste(df$Year, df$WkNo, 1, sep="/"),'Y/W/w'))

arima_ts <-  df %>% dplyr::select(Date, Cases)
arima_tbl <- arima_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)

start_date <- arima_tbl$Date[length(arima_tbl)]
end_date <- arima_tbl$Date[nrow(arima_tbl)]


# Define UI for application that draws a histogram
fluidPage(
  
  # load shinyjs
  useShinyjs(),
  
  # Application title
  titlePanel("Climate and Dengue"),
  
  # Navigation panel on left
  navlistPanel(
    id = "tabset",
    widths = c(2,10),
    "EDA",
    tabPanel("Distribution"),
    tabPanel("Country by Country Comparison"),
    "Variable Selection",
    tabPanel("Correlation"),
    tabPanel("Feature Importance"),
    "Univariate Time Series",
    tabPanel("ARIMA",
             
             # Left column
             column(
               3,
               
               # Model Initialization
               strong("Initialization"),
               wellPanel(
                 fluidRow(
                   sliderInput(
                     "periodRange", 
                     "Select Period:", 
                     min = as.Date(start_date), max = as.Date(end_date), 
                     value = c(as.Date(start_date), as.Date(end_date)),
                     timeFormat = "%Y-%m-%d",
                     width = "90%"
                   ),
                   div(actionButton("initButton", "Begin"), style = "float:right")
                 )
               ),
               
               # Model Tuning
               conditionalPanel(
                 condition = ("input.initButton > 0"),
                 strong("Parameter Tuning"),
                 wellPanel(
                   fluidRow(
                     tags$table(
                       width = "100%",
                       tags$tr(
                         tags$td(
                           width = "50%",
                           strong("p"),
                           tags$br(),
                           tags$p("Autoregression", style = "font-size:10px"),
                           align = "left"
                         ),
                         tags$td(
                           numericInput(inputId = "pInput",
                                        label = NULL,
                                        value = 0,
                                        min = 0,
                                        max = 9,
                                        step = 1),
                           width = "50%"
                         )
                       ),
                       tags$tr(
                         tags$td(
                           width = "50%",
                           strong("d"),
                           tags$br(),
                           tags$p("Differencing", style = "font-size:10px"),
                           align = "left"
                         ),
                         tags$td(
                           numericInput(inputId = "dInput",
                                        label = NULL,
                                        value = 0,
                                        min = 0,
                                        max = 9,
                                        step = 1),
                           width = "50%"
                         )
                       ),
                       tags$tr(
                         tags$td(
                           width = "50%",
                           strong("q"),
                           tags$br(),
                           tags$p("Moving Average", style = "font-size:10px"),
                           align = "left"
                         ),
                         tags$td(
                           numericInput(inputId = "qInput",
                                        label = NULL,
                                        value = 0,
                                        min = 0,
                                        max = 9,
                                        step = 1),
                           width = "50%"
                         )
                       )
                     )
                   )
                 )
               ),
               
               # Model Forecast
               conditionalPanel(
                 condition = ("input.initButton > 0"),
                 strong("Forecast"),
                 wellPanel(
                   fluidRow(
                     tags$table(
                       width = "100%",
                       tags$tr(
                         tags$td(
                           width = "50%",
                           strong("n ahead"),
                           tags$br(),
                           tags$p("Periods to forecast", style = "font-size:10px"),
                           align = "left"
                         ),
                         tags$td(
                           numericInput(inputId = "nInput",
                                        label = NULL,
                                        value = 13,
                                        min = 1,
                                        max = 208,
                                        step = 1),
                           width = "50%"
                         )
                       )
                     ),
                     div(actionButton("forecastButton", "Go"), style = "float:right")
                   )
                 )
               )
             ),
             
             # Diagnostics Panel
             column(
               4,
               conditionalPanel(
                 condition = ("input.initButton > 0"),
                 tabsetPanel(
                   tabPanel(
                     "Actual vs Fit",
                     fluidRow(
                       uiOutput("avp_plot")
                     )
                   ),
                   tabPanel(
                     "Residuals",
                     fluidRow(
                       uiOutput("residual_plot")
                     )
                   )
                 ),
                 fluidRow(
                   uiOutput("metric_table")
                 )
               )
             ),
             
             # Forecast Panel
             column(
               5,
               conditionalPanel(
                 condition = ("input.forecastButton > 0"),
                 tabsetPanel(
                   tabPanel(
                     "Forecast",
                     fluidRow(
                       uiOutput("forecast_plot")
                     )
                   ),
                   tabPanel(
                     "Data table",
                     fluidRow(
                       style = "padding:5%;",
                       uiOutput("results_table")
                     )
                   )
                 )
               )
             )
             
    ),
    tabPanel("ETS",

             # Left column
             column(
               3,

               # Model Initialization
               strong("Initialization"),
               wellPanel(
                 fluidRow(
                   sliderInput(
                     "periodRangeEts",
                     "Select Period:",
                     min = as.Date(start_date), max = as.Date(end_date),
                     value = c(as.Date(start_date), as.Date(end_date)),
                     timeFormat = "%Y-%m-%d",
                     width = "90%"
                   ),
                   div(actionButton("initEtsButton", "Begin"), style = "float:right")
                 )
               ),

               # Model Tuning
               conditionalPanel(
                 condition = ("input.initEtsButton > 0"),
                 strong("Parameter Tuning"),
                 wellPanel(
                   fluidRow(
                     tags$table(
                       width = "100%",
                       tags$tr(
                         tags$td(
                           width = "50%",
                           strong("Error"),
                           tags$br(),
                           tags$p("Form of error term", style = "font-size:10px"),
                           align = "left"
                         ),
                         tags$td(
                           selectInput(inputId = "eInput",
                                        label = NULL,
                                        choices = c("Additive" = "A",
                                                    "Multiplicative" = "M"),
                                        selected = "Additive"),
                           width = "50%"
                         )
                       ),
                       tags$tr(
                         tags$td(
                           width = "50%",
                           strong("Trend"),
                           tags$br(),
                           tags$p("Form of trend term", style = "font-size:10px"),
                           align = "left"
                         ),
                         tags$td(
                           selectInput(inputId = "tInput",
                                        label = NULL,
                                        choices = c("Additive" = "A",
                                                    "Multiplicative" = "M",
                                                    "None" = "N"),
                                        selected = "N"),
                           width = "50%"
                         )
                       ),
                       tags$tr(
                         tags$td(
                           width = "50%",
                           strong("Season"),
                           tags$br(),
                           tags$p("Form of season", style = "font-size:10px"),
                           align = "left"
                         ),
                         tags$td(
                           selectInput(inputId = "sInput",
                                        label = NULL,
                                        choices = c("Additive" = "A",
                                                    "Multiplicative" = "M",
                                                    "None" = "N"),
                                        selected = "N"),
                           width = "50%"
                         )
                       )
                     )
                   )
                 )
               ),

               conditionalPanel(
                 condition = ("input.initEtsButton > 0"),
                 strong("Trend parameters"),
                 wellPanel(
                   tabsetPanel(
                     tabPanel("Alpha",
                              tags$table(
                                width = "100%",
                                tags$tr(
                                  tags$td(
                                    width = "50%",
                                    strong("Alpha"),
                                    tags$br(),
                                    tags$p("Smoothing parameter for level", style = "font-size:10px"),
                                    align = "left"
                                  ),
                                  tags$td(
                                    sliderInput(
                                      "trendAlphaEts",
                                      label = NULL,
                                      min = 0.0, max = 1.0,
                                      value = 0
                                    ),
                                    width = "50%"
                                  )
                                )
                              )
                     ),
                     tabPanel("Beta",
                              conditionalPanel(
                                condition = "input.tInput != 'N'",
                                tags$table(
                                  width = "100%",
                                  tags$tr(
                                    tags$td(
                                      width = "50%",
                                      strong("Beta"),
                                      tags$br(),
                                      tags$p("Smoothing parameter for slope", style = "font-size:10px"),
                                      align = "left"
                                    ),
                                    tags$td(
                                      sliderInput(
                                        "trendBetaEts",
                                        label = NULL,
                                        min = 0.0, max = 1.0,
                                        value = 0
                                      ),
                                      width = "50%"
                                    )
                                  )
                                )
                              )
                     ),
                     tabPanel("Gamma",
                              conditionalPanel(
                                condition = "input.sInput != 'N'",
                                tags$table(
                                  width = "100%",
                                  tags$tr(
                                    tags$td(
                                      width = "50%",
                                      strong("Gamma"),
                                      tags$br(),
                                      tags$p("Smoothing parameter for season", style = "font-size:10px"),
                                      align = "left"
                                    ),
                                    tags$td(
                                      sliderInput(
                                        "seasonGammaEts",
                                        label = NULL,
                                        min = 0.0, max = 1.0,
                                        value = 0
                                      ),
                                      width = "50%"
                                    )
                                  )
                                )
                              )
                     )
                   )
                 )
               ),
               

               # Model Forecast
               conditionalPanel(
                 condition = ("input.initEtsButton > 0"),
                 strong("Forecast"),
                 wellPanel(
                   fluidRow(
                     tags$table(
                       width = "100%",
                       tags$tr(
                         tags$td(
                           width = "50%",
                           strong("n ahead"),
                           tags$br(),
                           tags$p("Periods to forecast", style = "font-size:10px"),
                           align = "left"
                         ),
                         tags$td(
                           numericInput(inputId = "nEtsInput",
                                        label = NULL,
                                        value = 13,
                                        min = 1,
                                        max = 208,
                                        step = 1),
                           width = "50%"
                         )
                       )
                     ),
                     div(actionButton("forecastEtsButton", "Go"), style = "float:right")
                   )
                 )
               )
               
             
             ),

             # Diagnostics Panel
             column(
               4,
               conditionalPanel(
                 condition = ("input.initEtsButton > 0"),
                 tabsetPanel(
                   tabPanel(
                     "Actual vs Fit",
                     fluidRow(
                       uiOutput("ets_avp_plot")
                     )
                   ),
                   tabPanel(
                     "Components",
                     fluidRow(
                       uiOutput("ets_component_plot")
                     )
                   ),
                   tabPanel(
                     "Report",
                     fluidRow(
                       uiOutput("ets_mdl_report")
                     )
                   )
                 ),
                 fluidRow(
                   uiOutput("ets_metric_table")
                 )
               )
             ),
    

             # Forecast Panel
             column(
               5,
               conditionalPanel(
                 condition = ("input.forecastEtsButton > 0"),
                 tabsetPanel(
                   tabPanel(
                     "Forecast",
                     fluidRow(
                       uiOutput("ets_forecast_plot")
                     )
                   ),
                   tabPanel(
                     "Data table",
                     fluidRow(
                       style = "padding:5%;",
                       uiOutput("ets_results_table")
                     )
                   )
                 )
               )
             )
  )
  
  
))
