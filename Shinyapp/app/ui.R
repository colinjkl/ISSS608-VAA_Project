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

df <- read_csv("data/dengue_climate_joined_by_week_transformed_diff.csv")

df$Date <- lubridate::ymd(lubridate::parse_date_time(paste(df$Year, df$WkNo, 1, sep="/"),'Y/W/w'))

varList <- colnames(df[!grepl("^z_|^mm_|^log_|^diff|Year|WkNo|Cases|Date", colnames(df))])


arima_ts <-  df %>% dplyr::select(Date, Cases)
arima_tbl <- arima_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)

start_date <- arima_tbl$Date[length(arima_tbl)]
end_date <- arima_tbl$Date[nrow(arima_tbl)]




# Define UI for application that draws a histogram
fluidPage(
  
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
    
    # ARIMA Module
    tabPanel("ARIMA",
             
             # ARIMA Initialization column ----
             column(
               3,
               
               # ARIMA choose dates ----
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
               
               # ARIMA parameter tuning ----
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
               
               # ARIMA forecast panel ----
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
             # End column ----
             
             # ARIMA Diagnostics column ----
             column(
               4,
               conditionalPanel(
                 condition = ("input.initButton > 0"),
                 tabsetPanel(
                   # ARIMA avp plot ----
                   tabPanel(
                     "Actual vs Fit",
                     fluidRow(
                       uiOutput("avp_plot")
                     )
                   ),
                   # ARIMA residual plot ----
                   tabPanel(
                     "Residuals",
                     fluidRow(
                       uiOutput("residual_plot")
                     )
                   )
                 ),
                 # ARIMA metric table ----
                 fluidRow(
                   uiOutput("metric_table")
                 )
               )
             ),
             # End column ----
             
             # ARIMA Forecast column ----
             column(
               5,
               conditionalPanel(
                 condition = ("input.forecastButton > 0"),
                 tabsetPanel(
                   # ARIMA forecast plot ----
                   tabPanel(
                     "Forecast",
                     fluidRow(
                       uiOutput("forecast_plot")
                     )
                   ),
                   # ARIMA forecast data table ----
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
             # End Column ----
    ),
    # End Module ----
    
    # ETS Module
    tabPanel("ETS",
             
             # ETS initialization column ----
             column(
               3,
               
               # ETS choose dates ----
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
               
               # ETS parameter tuning 1 ----
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
               
               # ETS parameter tuning 2 ----
               conditionalPanel(
                 condition = ("input.initEtsButton > 0"),
                 strong("Trend parameters"),
                 wellPanel(
                   tabsetPanel(
                     # Alpha ----
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
                     # Beta ----
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
                     # Gamma ----
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
               
               # Model Forecast panel----
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
             # End column ----
             
             # ETS Diagnostics column ----
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
             # End column ----
             
             # ETS Forecast column ----
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
             # End column ----
    ),
    # End Module ----
    
  # VAR Module
  tabPanel("VAR",
           tabsetPanel(
             
             # VAR Tuning tab ----
             tabPanel(
               "Model Tuning",
               
               # VAR initialization column ----
               column(
                 3,
                 
                 # VAR choose dates ----
                 strong("Initialization"),
                 wellPanel(
                   fluidRow(
                     sliderInput(
                       "periodRangeVar",
                       "Select Period:",
                       min = as.Date(start_date), max = as.Date(end_date),
                       value = c(as.Date(start_date), as.Date(end_date)),
                       timeFormat = "%Y-%m-%d",
                       width = "90%"
                     ),
                     div(actionButton("initVarButton", "Begin"), style = "float:right")
                   )
                 ),
                 # VAR choose variables ----
                 conditionalPanel(
                   condition = ("input.initVarButton > 0"),
                   strong("Parameter Tuning"),
                   wellPanel(
                     fluidRow(
                       checkboxGroupInput("checkBoxVar", "Choose variables:",
                                          choiceNames = varList,
                                          choiceValues = varList,
                                          inline = FALSE
                       )
                     )
                   )
                 )
               ),
               # End column ----
               
               # VAR Added Variable column ----
               column(
                 3,
                 
                 # Avg Rainfall ----
                 conditionalPanel(
                   condition = ("input.checkBoxVar.includes('avg_rainfall')"),
                   strong("Average Rainfall"),
                   wellPanel(
                     fluidRow(
                       radioButtons(
                         "radioAvgRainfallInput",
                         "Choose transformation:",
                         choiceNames = list("None", "Log", "MinMax", "Z"),
                         choiceValues = list("None", "Log", "MinMax", "Z"),
                         inline = TRUE
                       ),
                       tags$table(
                         width = "100%",
                         tags$tr(
                           tags$td(
                             width = "50%",
                             strong("Differencing"),
                             tags$br(),
                             tags$p("Number of differencing:", style = "font-size:10px"),
                             align = "left"
                           ),
                           tags$td(
                             numericInput(inputId = "diffAvgRainfallInput",
                                          label = NULL,
                                          value = 0,
                                          min = 0,
                                          max = 3,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       )
                     )
                   )
                 ),
                 
                 # Tot Rainfall ----
                 conditionalPanel(
                   condition = ("input.checkBoxVar.includes('tot_rainfall')"),
                   strong("Total Rainfall"),
                   wellPanel(
                     fluidRow(
                       radioButtons(
                         "radioTotRainfallInput",
                         "Choose transformation:",
                         choiceNames = list("None", "Log", "MinMax", "Z"),
                         choiceValues = list("None", "Log", "MinMax", "Z"),
                         inline = TRUE
                       ),
                       tags$table(
                         width = "100%",
                         tags$tr(
                           tags$td(
                             width = "50%",
                             strong("Differencing"),
                             tags$br(),
                             tags$p("Number of differencing:", style = "font-size:10px"),
                             align = "left"
                           ),
                           tags$td(
                             numericInput(inputId = "diffTotRainfallInput",
                                          label = NULL,
                                          value = 0,
                                          min = 0,
                                          max = 3,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       )
                     )
                   )
                 ),
                 
                 # Max 30m Rainfall ----
                 conditionalPanel(
                   condition = ("input.checkBoxVar.includes('max_30m_rainfall')"),
                   strong("Max 30m Rainfall"),
                   wellPanel(
                     fluidRow(
                       radioButtons(
                         "radioMax30mRainfallInput",
                         "Choose transformation:",
                         choiceNames = list("None", "Log", "MinMax", "Z"),
                         choiceValues = list("None", "Log", "MinMax", "Z"),
                         inline = TRUE
                       ),
                       tags$table(
                         width = "100%",
                         tags$tr(
                           tags$td(
                             width = "50%",
                             strong("Differencing"),
                             tags$br(),
                             tags$p("Number of differencing:", style = "font-size:10px"),
                             align = "left"
                           ),
                           tags$td(
                             numericInput(inputId = "diffMax30mRainfallInput",
                                          label = NULL,
                                          value = 0,
                                          min = 0,
                                          max = 3,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       )
                     )
                   )
                 ),
                 
                 # Max 60m Rainfall ----
                 conditionalPanel(
                   condition = ("input.checkBoxVar.includes('max_60m_rainfall')"),
                   strong("Max 60m Rainfall"),
                   wellPanel(
                     fluidRow(
                       radioButtons(
                         "radioMax60mRainfallInput",
                         "Choose transformation:",
                         choiceNames = list("None", "Log", "MinMax", "Z"),
                         choiceValues = list("None", "Log", "MinMax", "Z"),
                         inline = TRUE
                       ),
                       tags$table(
                         width = "100%",
                         tags$tr(
                           tags$td(
                             width = "50%",
                             strong("Differencing"),
                             tags$br(),
                             tags$p("Number of differencing:", style = "font-size:10px"),
                             align = "left"
                           ),
                           tags$td(
                             numericInput(inputId = "diffMax60mRainfallInput",
                                          label = NULL,
                                          value = 0,
                                          min = 0,
                                          max = 3,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       )
                     )
                   )
                 ),
                 
                 # Max 120m Rainfall ----
                 conditionalPanel(
                   condition = ("input.checkBoxVar.includes('max_120m_rainfall')"),
                   strong("Max 120m Rainfall"),
                   wellPanel(
                     fluidRow(
                       radioButtons(
                         "radioMax120mRainfallInput",
                         "Choose transformation:",
                         choiceNames = list("None", "Log", "MinMax", "Z"),
                         choiceValues = list("None", "Log", "MinMax", "Z"),
                         inline = TRUE
                       ),
                       tags$table(
                         width = "100%",
                         tags$tr(
                           tags$td(
                             width = "50%",
                             strong("Differencing"),
                             tags$br(),
                             tags$p("Number of differencing:", style = "font-size:10px"),
                             align = "left"
                           ),
                           tags$td(
                             numericInput(inputId = "diffMax120mRainfallInput",
                                          label = NULL,
                                          value = 0,
                                          min = 0,
                                          max = 3,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       )
                     )
                   )
                 ),
                 
                 # Avg Temp ----
                 conditionalPanel(
                   condition = ("input.checkBoxVar.includes('avg_temp')"),
                   strong("Average Temperature"),
                   wellPanel(
                     fluidRow(
                       radioButtons(
                         "radioAvgTempInput",
                         "Choose transformation:",
                         choiceNames = list("None", "Log", "MinMax", "Z"),
                         choiceValues = list("None", "Log", "MinMax", "Z"),
                         inline = TRUE
                       ),
                       tags$table(
                         width = "100%",
                         tags$tr(
                           tags$td(
                             width = "50%",
                             strong("Differencing"),
                             tags$br(),
                             tags$p("Number of differencing:", style = "font-size:10px"),
                             align = "left"
                           ),
                           tags$td(
                             numericInput(inputId = "diffAvgTempInput",
                                          label = NULL,
                                          value = 0,
                                          min = 0,
                                          max = 3,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       )
                     )
                   )
                 ),
                 
                 # Max Temp ----
                 conditionalPanel(
                   condition = ("input.checkBoxVar.includes('max_temp')"),
                   strong("Max Temperature"),
                   wellPanel(
                     fluidRow(
                       radioButtons(
                         "radioMaxTempInput",
                         "Choose transformation:",
                         choiceNames = list("None", "Log", "MinMax", "Z"),
                         choiceValues = list("None", "Log", "MinMax", "Z"),
                         inline = TRUE
                       ),
                       tags$table(
                         width = "100%",
                         tags$tr(
                           tags$td(
                             width = "50%",
                             strong("Differencing"),
                             tags$br(),
                             tags$p("Number of differencing:", style = "font-size:10px"),
                             align = "left"
                           ),
                           tags$td(
                             numericInput(inputId = "diffMaxTempInput",
                                          label = NULL,
                                          value = 0,
                                          min = 0,
                                          max = 3,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       )
                     )
                   )
                 ),
                 
                 # Min Temp ----
                 conditionalPanel(
                   condition = ("input.checkBoxVar.includes('min_temp')"),
                   strong("Min Temperature"),
                   wellPanel(
                     fluidRow(
                       radioButtons(
                         "radioMinTempInput",
                         "Choose transformation:",
                         choiceNames = list("None", "Log", "MinMax", "Z"),
                         choiceValues = list("None", "Log", "MinMax", "Z"),
                         inline = TRUE
                       ),
                       tags$table(
                         width = "100%",
                         tags$tr(
                           tags$td(
                             width = "50%",
                             strong("Differencing"),
                             tags$br(),
                             tags$p("Number of differencing:", style = "font-size:10px"),
                             align = "left"
                           ),
                           tags$td(
                             numericInput(inputId = "diffMinTempInput",
                                          label = NULL,
                                          value = 0,
                                          min = 0,
                                          max = 3,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       )
                     )
                   )
                 ), 
                 
                 # Avg Wind ----
                 conditionalPanel(
                   condition = ("input.checkBoxVar.includes('avg_wind')"),
                   strong("Average Wind Speed"),
                   wellPanel(
                     fluidRow(
                       radioButtons(
                         "radioAvgWindInput",
                         "Choose transformation:",
                         choiceNames = list("None", "Log", "MinMax", "Z"),
                         choiceValues = list("None", "Log", "MinMax", "Z"),
                         inline = TRUE
                       ),
                       tags$table(
                         width = "100%",
                         tags$tr(
                           tags$td(
                             width = "50%",
                             strong("Differencing"),
                             tags$br(),
                             tags$p("Number of differencing:", style = "font-size:10px"),
                             align = "left"
                           ),
                           tags$td(
                             numericInput(inputId = "diffAvgWindInput",
                                          label = NULL,
                                          value = 0,
                                          min = 0,
                                          max = 3,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       )
                     )
                   )
                 ),
                 
                 # Max Wind ----
                 conditionalPanel(
                   condition = ("input.checkBoxVar.includes('max_wind')"),
                   strong("Max Wind Speed"),
                   wellPanel(
                     fluidRow(
                       radioButtons(
                         "radioMaxWindInput",
                         "Choose transformation:",
                         choiceNames = list("None", "Log", "MinMax", "Z"),
                         choiceValues = list("None", "Log", "MinMax", "Z"),
                         inline = TRUE
                       ),
                       tags$table(
                         width = "100%",
                         tags$tr(
                           tags$td(
                             width = "50%",
                             strong("Differencing"),
                             tags$br(),
                             tags$p("Number of differencing:", style = "font-size:10px"),
                             align = "left"
                           ),
                           tags$td(
                             numericInput(inputId = "diffMaxWindInput",
                                          label = NULL,
                                          value = 0,
                                          min = 0,
                                          max = 3,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       )
                     )
                   )
                 ),
               ),
               # End column ----
               
               # VAR Model Diagnostics column ----
               column(
                 6,
                 conditionalPanel(
                   condition = ("input.initVarButton > 0"),
                   tabsetPanel(
                     
                     
                     # VAR avp plot ----
                     tabPanel(
                       "Actual vs Fit",
                       fluidRow(
                         uiOutput("var_avp_plot")
                       ),
                       fluidRow(
                         uiOutput("var_met_table")
                       )
                     ),
                     # VAR acf plot ----
                     tabPanel(
                       "ACF",
                       uiOutput("var_acf_plot")
                     )
                   )
                 )
               ),
               # End column ----
             ),
             # End tab ----
             
             # VAR Forecast tab ----
             tabPanel(
               "Forecast",
               
               # VAR forecast panel ----
               fluidRow(
                 column(
                   3,
                   strong("VAR Model Forecast"),
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
                             numericInput(inputId = "nVarInput",
                                          label = NULL,
                                          value = 13,
                                          min = 1,
                                          max = 208,
                                          step = 1),
                             width = "50%"
                           )
                         )
                       ),
                       div(actionButton("forecastVarButton", "Go"), style = "float:right")
                     )
                   )
                 )
               ),
               
               # VAR forecast plot ----
               conditionalPanel(
                 condition = "input.forecastVarButton > 0",
                 fluidRow(
                   (div(style='overflow-x: scroll;',
                        uiOutput("var_forecast_plot")))
                 )
               )
             )
           )
  )
  
  # End Module ----
  
))
