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
  tidyverse,
  fable,
  tsibble,
  feasts,
  patchwork,
  plotly,
  ggstatsplot,
  MLmetrics,
  performance,
  caret,
  ggstatsplot
)

df <- read_csv("data/dengue_climate_joined_by_week_transformed_diff.csv")

df$Date <- lubridate::ymd(lubridate::parse_date_time(paste(df$Year, df$WkNo, 1, sep="/"),'Y/W/w'))

varList <- colnames(df[!grepl("^z_|^mm_|^log_|^diff|Year|WkNo|Cases|Date", colnames(df))])


arima_ts <-  df %>% dplyr::select(Date, Cases)
arima_tbl <- arima_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)

start_date <- arima_tbl$Date[34]
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
    "Explanatory Model",
    
    # Lm module ---- 
    tabPanel("Linear Regression",

             # LM initialization columns ----
             column(
               3,

               # LM choose dates ----
               strong("Choose Dates"),
               wellPanel(
                 fluidRow(
                   sliderInput(
                     "periodRangeLm",
                     "Select Period:",
                     min = as.Date(start_date), max = as.Date(end_date),
                     value = c(as.Date(start_date), as.Date(end_date)),
                     timeFormat = "%Y-%m-%d",
                     width = "90%"
                   )
                 )
               ),
               # Lm choose variables ----
               strong("Parameter Tuning"),
               wellPanel(
                 fluidRow(
                   checkboxGroupInput("checkBoxLm", "Choose variables:",
                                      choiceNames = varList,
                                      choiceValues = varList,
                                      inline = FALSE
                   )
                 )
               ),
               
               # Lm stepwise ----
               strong("Stepwise Regression"),
               wellPanel(
                 fluidRow(
                   radioButtons(
                     "lmRadioStepwise",
                     "Choose method:",
                     choiceNames = list("None", "Forward", "Backward"),
                     choiceValues = list("None", "Forward", "Backward"),
                     inline = TRUE
                   ),
                   div(actionButton("lmTuneButton", "Tune"), style = "float:right")
                 )
               )
             ),
             # End column ----

             # LM added variable column ----
             column(
               3,

               # Avg Rainfall ----
               conditionalPanel(
                 condition = ("input.checkBoxLm.includes('avg_rainfall')"),
                 strong("Average Rainfall"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "lmRadioAvgRainfallInput",
                       "Choose transformation:",
                       choiceNames = list("None", "Log", "MinMax", "Z"),
                       choiceValues = list("None", "Log", "MinMax", "Z"),
                       inline = TRUE
                     )
                   )
                 )
               ),

               # Tot Rainfall ----
               conditionalPanel(
                 condition = ("input.checkBoxLm.includes('tot_rainfall')"),
                 strong("Total Rainfall"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "lmRadioTotRainfallInput",
                       "Choose transformation:",
                       choiceNames = list("None", "Log", "MinMax", "Z"),
                       choiceValues = list("None", "Log", "MinMax", "Z"),
                       inline = TRUE
                     )
                   )
                 )
               ),

               # Max 30m Rainfall ----
               conditionalPanel(
                 condition = ("input.checkBoxLm.includes('max_30m_rainfall')"),
                 strong("Max 30m Rainfall"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "lmRadioMax30mRainfallInput",
                       "Choose transformation:",
                       choiceNames = list("None", "Log", "MinMax", "Z"),
                       choiceValues = list("None", "Log", "MinMax", "Z"),
                       inline = TRUE
                     )
                   )
                 )
               ),

               # Max 60m Rainfall ----
               conditionalPanel(
                 condition = ("input.checkBoxLm.includes('max_60m_rainfall')"),
                 strong("Max 60m Rainfall"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "lmRadioMax60mRainfallInput",
                       "Choose transformation:",
                       choiceNames = list("None", "Log", "MinMax", "Z"),
                       choiceValues = list("None", "Log", "MinMax", "Z"),
                       inline = TRUE
                     )
                   )
                 )
               ),

               # Max 120m Rainfall ----
               conditionalPanel(
                 condition = ("input.checkBoxLm.includes('max_120m_rainfall')"),
                 strong("Max 120m Rainfall"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "lmRadioMax120mRainfallInput",
                       "Choose transformation:",
                       choiceNames = list("None", "Log", "MinMax", "Z"),
                       choiceValues = list("None", "Log", "MinMax", "Z"),
                       inline = TRUE
                     )
                   )
                 )
               ),

               # Avg Temp ----
               conditionalPanel(
                 condition = ("input.checkBoxLm.includes('avg_temp')"),
                 strong("Average Temperature"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "lmRadioAvgTempInput",
                       "Choose transformation:",
                       choiceNames = list("None", "Log", "MinMax", "Z"),
                       choiceValues = list("None", "Log", "MinMax", "Z"),
                       inline = TRUE
                     )
                   )
                 )
               ),

               # Max Temp ----
               conditionalPanel(
                 condition = ("input.checkBoxLm.includes('max_temp')"),
                 strong("Max Temperature"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "lmRadioMaxTempInput",
                       "Choose transformation:",
                       choiceNames = list("None", "Log", "MinMax", "Z"),
                       choiceValues = list("None", "Log", "MinMax", "Z"),
                       inline = TRUE
                     )
                   )
                 )
               ),

               # Min Temp ----
               conditionalPanel(
                 condition = ("input.checkBoxLm.includes('min_temp')"),
                 strong("Min Temperature"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "lmRadioMinTempInput",
                       "Choose transformation:",
                       choiceNames = list("None", "Log", "MinMax", "Z"),
                       choiceValues = list("None", "Log", "MinMax", "Z"),
                       inline = TRUE
                     )
                   )
                 )
               ),

               # Avg Wind ----
               conditionalPanel(
                 condition = ("input.checkBoxLm.includes('avg_wind')"),
                 strong("Average Wind Speed"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "lmRadioAvgWindInput",
                       "Choose transformation:",
                       choiceNames = list("None", "Log", "MinMax", "Z"),
                       choiceValues = list("None", "Log", "MinMax", "Z"),
                       inline = TRUE
                     )
                   )
                 )
               ),

               # Max Wind ----
               conditionalPanel(
                 condition = ("input.checkBoxLm.includes('max_wind')"),
                 strong("Max Wind Speed"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "lmRadioMaxWindInput",
                       "Choose transformation:",
                       choiceNames = list("None", "Log", "MinMax", "Z"),
                       choiceValues = list("None", "Log", "MinMax", "Z"),
                       inline = TRUE
                     )
                   )
                 )
               ),
             ),
             # End column ----

             # LM diagnostic column ----
             column(
               6,
                 tabsetPanel(
                   tabPanel(
                     "Observed vs Fitted",
                     fluidRow(
                       plotOutput("lm_avp")
                     ),
                     fluidRow(
                       tableOutput("lm_met_1")
                     ),
                     fluidRow(
                       tableOutput("lm_met_2")
                     )
                   ),
                   tabPanel(
                     "Coefficients",
                     fluidRow(
                       plotOutput("lm_coeff", height = 600)
                     )
                   ),
                   tabPanel(
                     "Diagnostics",
                     fluidRow(
                       selectInput("lmDiagnosticInput",
                                   "Select Model Checks:",
                                   choices = c("Posterior Predictive",
                                               "Linearity",
                                               "Homogeneity of Variance",
                                               "Influential Observations",
                                               "Collinearity",
                                               "Normality of Residuals"),
                                   selected = "Posterior Predictive")
                     ),
                     fluidRow(
                       plotOutput("lm_diagnostic", height = 600)
                     )
                   ),
                   tabPanel(
                     "Raw Variables",
                     fluidRow(
                       uiOutput("lm_ts_plot")
                     )
                   )
                 )
             )
             # End column ----
    ),
    # End Module ----
    
    # Tslm module ----
    tabPanel("Time Series Linear Regression",
             
             # Tslm initialization columns ----
             column(
               3,
               
               # Tslm choose dates ----
               strong("Choose Dates"),
               wellPanel(
                 fluidRow(
                   sliderInput(
                     "periodRangeTslm",
                     "Select Period:",
                     min = as.Date(start_date), max = as.Date(end_date),
                     value = c(as.Date(start_date), as.Date(end_date)),
                     timeFormat = "%Y-%m-%d",
                     width = "90%"
                   )
                 )
               ),
               # Tslm choose variables ----
               strong("Parameter Tuning"),
               wellPanel(
                 fluidRow(
                   checkboxGroupInput("checkBoxTslm", "Choose variables:",
                                      choiceNames = varList,
                                      choiceValues = varList,
                                      inline = FALSE
                   )
                 ),
                 fluidRow(
                  div(actionButton("tslmTuneButton", "Tune"), style = "float:right")
                 )
               )
             ),
             # End column ----
             
             # Tslm added variable column ----
             column(
               3,
               
               # Cases ----
               wellPanel(
                 fluidRow(
                   radioButtons(
                     "tslmRadioCasesInput",
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
                         numericInput(inputId = "tslmDiffCasesInput",
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
               ),
               # AvgRainfall ----
               conditionalPanel(
                 condition = ("input.checkBoxTslm.includes('avg_rainfall')"),
                 strong("Average Rainfall"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "tslmRadioAvgRainfallInput",
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
                           numericInput(inputId = "tslmDiffAvgRainfallInput",
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
                 condition = ("input.checkBoxTslm.includes('tot_rainfall')"),
                 strong("Total Rainfall"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "tslmRadioTotRainfallInput",
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
                           numericInput(inputId = "tslmDiffTotRainfallInput",
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
                 condition = ("input.checkBoxTslm.includes('max_30m_rainfall')"),
                 strong("Max 30m Rainfall"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "tslmRadioMax30mRainfallInput",
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
                           numericInput(inputId = "tslmDiffMax30mInput",
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
                 condition = ("input.checkBoxTslm.includes('max_60m_rainfall')"),
                 strong("Max 60m Rainfall"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "tslmRadioMax60mRainfallInput",
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
                           numericInput(inputId = "tslmDiffMax60mInput",
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
                 condition = ("input.checkBoxTslm.includes('max_120m_rainfall')"),
                 strong("Max 120m Rainfall"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "tslmRadioMax120mRainfallInput",
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
                           numericInput(inputId = "tslmDiffMax120mInput",
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
                 condition = ("input.checkBoxTslm.includes('avg_temp')"),
                 strong("Average Temperature"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "tslmRadioAvgTempInput",
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
                           numericInput(inputId = "tslmDiffAvgTempInput",
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
                 condition = ("input.checkBoxTslm.includes('max_temp')"),
                 strong("Max Temperature"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "tslmRadioMaxTempInput",
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
                           numericInput(inputId = "tslmDiffMaxTempInput",
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
                 condition = ("input.checkBoxTslm.includes('min_temp')"),
                 strong("Min Temperature"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "tslmRadioMinTempInput",
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
                           numericInput(inputId = "tslmDiffMinTempInput",
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
                 condition = ("input.checkBoxTslm.includes('avg_wind')"),
                 strong("Average Wind Speed"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "tslmRadioAvgWindInput",
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
                           numericInput(inputId = "tslmDiffAvgWindInput",
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
                 condition = ("input.checkBoxTslm.includes('max_wind')"),
                 strong("Max Wind Speed"),
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "tslmRadioMaxWindInput",
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
                           numericInput(inputId = "tslmDiffMaxWindInput",
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
             
             # Tslm Diagnostics column ----
             column(
               6,
               tabsetPanel(   
                 
                 # Tslm avp plot ----
                 tabPanel(
                   "Actual vs Fit",
                   fluidRow(
                     plotOutput("tslm_avp")
                   ),
                   fluidRow(
                     tableOutput("tslm_met")
                   )
                 ),
                 # Tslm rdl plot ----
                 tabPanel(
                   "Residuals",
                   fluidRow(
                     plotOutput("tslm_rdl")
                   )
                 ),
                 # Tslm coeff plot ----
                 tabPanel(
                   "Coefficients",
                   fluidRow(
                     plotOutput("tslm_coeff")
                   ),
                   fluidRow(
                     tableOutput("tslm_coeff_error")
                   )
                 )
               )
             ),
             
             
    ),
    # End Module ----
             
    # New Section ----
    "Univariate Time Series",
    
    # ARIMA Module ----
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
                   )
                 )
               ),
               
               # ARIMA parameter tuning ----
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
                   ),
                   div(actionButton("arimaTuneButton", "Tune"), style = "float:right")
                 )
               ),
               
               # ARIMA forecast panel ----
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
                   div(actionButton("forecastArimaButton", "Go"), style = "float:right")
                 )
               )
             ),
             # End column ----
             
             # ARIMA Diagnostics column ----
             column(
               4,
               tabsetPanel(
                 # ARIMA avp plot ----
                 tabPanel(
                   "Actual vs Fit",
                   fluidRow(
                     plotOutput("arima_avp")
                   )
                 ),
                 # ARIMA residual plot ----
                 tabPanel(
                   "Residuals",
                   fluidRow(
                     plotOutput("arima_rdl")
                   )
                 )
               ),
               # ARIMA metric table ----
               fluidRow(
                 tableOutput("arima_met")
               )
             ),
             # End column ----
             
             # ARIMA Forecast column ----
             column(
               5,
               tabsetPanel(
                 # ARIMA forecast plot ----
                 tabPanel(
                   "Forecast",
                   fluidRow(
                     plotOutput("arima_forecast")
                   )
                 ),
                 # ARIMA forecast data table ----
                 tabPanel(
                   "Data table",
                   fluidRow(
                     style = "padding:5%;",
                     dataTableOutput("arima_results")
                   )
                 )
               )
             )
             # End Column ----
    ),
    # End Module ----
    
    # ETS Module ----
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
                   )
                 )
               ),
               
               # ETS parameter tuning 1 ----
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
               ),
               
               # ETS parameter tuning 2 ----
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
                   ),
                   # Gamma ----
                   tabPanel("Gamma",
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
                 ),
                 fluidRow(
                   div(actionButton("etsTuneButton", "Tune"), style = "float:right")
                 )
               ),
               
               # Model Forecast panel----
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
             ),
             # End column ----
             
             # ETS Diagnostics column ----
             column(
               4,
               tabsetPanel(
                 tabPanel(
                   "Actual vs Fit",
                   fluidRow(
                     plotOutput("ets_avp")
                   )
                 ),
                 tabPanel(
                   "Components",
                   fluidRow(
                     plotOutput("ets_component")
                   )
                 ),
                 tabPanel(
                   "Report",
                   fluidRow(
                     htmlOutput("ets_report")
                   )
                 )
               ),
               fluidRow(
                 tableOutput("ets_met")
               )
             ),
             # End column ----
             
             # ETS Forecast column ----
             column(
               5,
               tabsetPanel(
                 tabPanel(
                   "Forecast",
                   fluidRow(
                     plotOutput("ets_forecast")
                   )
                 ),
                 tabPanel(
                   "Data table",
                   fluidRow(
                     style = "padding:5%;",
                     dataTableOutput("ets_results")
                   )
                 )
               )
             )
             # End column ----
    ),
    # End Module ----
    
  "Multi-Variate Time Series",
    
  # VAR Module ----
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
                     )
                   )
                 ),
                 # VAR choose variables ----
                 strong("Parameter Tuning"),
                 wellPanel(
                   fluidRow(
                     checkboxGroupInput("checkBoxVar", "Choose variables:",
                                        choiceNames = varList,
                                        choiceValues = varList,
                                        inline = FALSE
                     ),
                     div(actionButton("varTuneButton", "Tune"), style = "float:right")
                   )
                 )
               ),
               # End column ----
               
               # VAR Added Variable column ----
               column(
                 3,
                 
                 # Cases Panel ----
                 wellPanel(
                   fluidRow(
                     radioButtons(
                       "radioCasesInput",
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
                           numericInput(inputId = "diffCasesInput",
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
                 ),
                 
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
                 tabsetPanel(   
                   
                   # VAR avp plot ----
                   tabPanel(
                     "Actual vs Fit",
                     fluidRow(
                       plotOutput("var_avp")
                     ),
                     fluidRow(
                       tableOutput("var_met")
                     )
                   ),
                   # VAR acf plot ----
                   tabPanel(
                     "ACF",
                     (div(style='height:550px; overflow-y: scroll;',
                          uiOutput("var_acf_plot")))
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
                 column(
                   6,
                   fluidRow(
                      plotOutput("var_forecast", height = 600)
                   )
                 ),
                 column(
                   6,
                   tabsetPanel(
                     tabPanel(
                       "Other Regressors",
                       (div(style='height:550px; overflow-y: scroll;',
                            uiOutput("var_other_plot")))
                     ),
                     tabPanel(
                       "Data Table",
                       (div(style='height:550px; overflow-y: scroll;',
                            dataTableOutput("var_results")))
                     ),
                     tabPanel(
                       "Report",
                       (div(style='height:550px; overflow-y: scroll;',
                            tableOutput("var_report")))
                     )
                   )
                 )
               )
             )
           )
  )
  
  # End Module ----
  
))
