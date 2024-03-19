#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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

p <- reactiveVal()
d <- reactiveVal()
q <- reactiveVal()
n <- reactiveVal()

# Define server logic required to draw a histogram
function(input, output, session) {

  observeEvent(input$initButton, {
    output$avp_plot <- renderUI({
      plotOutput('tuned_avp')
    })
  })
    
  observeEvent(input$initButton, {
    output$residual_plot <- renderUI({
      plotOutput('tuned_rdl')
    })
  })
  
  observeEvent(input$initButton, {
    output$metric_table <- renderUI({
      tableOutput('tuned_met')
    })
  })
  
  observeEvent(input$forecastButton, {
    p(input$pInput)
    d(input$dInput)
    q(input$qInput)
    n(input$nInput)
    output$forecast_plot <- renderUI({
      plotOutput('forecast')
    })
  })
  
  observeEvent(input$forecastButton, {
    p(input$pInput)
    d(input$dInput)
    q(input$qInput)
    n(input$nInput)
    output$results_table <- renderUI({
      dataTableOutput('results')
    })
  })
  
  output$tuned_rdl <- renderPlot({
    
    # tuned arima model
    arima_mdl <- arima_tbl %>% 
      model(ARIMA(Cases ~ pdq(input$pInput,
                              input$dInput,
                              input$qInput)))
    
    # generate residual plot
    arima_mdl %>% gg_tsresiduals()
    
  })
  
  output$tuned_avp <- renderPlot({
    
    # tuned arima model
    arima_mdl <- arima_tbl %>% 
      model(ARIMA(Cases ~ pdq(input$pInput,
                              input$dInput,
                              input$qInput)))
    
    # find fit
    arima_fitted <- fitted(arima_mdl)[,2:3] %>% 
      as_tibble() %>% 
      rename(Cases=.fitted)
    
    # define types
    arima_fitted$Type <- "Fit"
    arima_tbl$Type <- "Observed"
    arima_avp <- dplyr::bind_rows(arima_fitted, arima_tbl)
    
    # plot
    ggplot(data = arima_avp) +
      geom_line(aes(x = Date, y = Cases, colour = Type)) +
      ggtitle("Observed vs Fitted")
    
    
  })
  
  output$tuned_met <- renderTable({
    
    # generate cross validation metrics
    arima_cv <- arima_tbl %>%
      stretch_tsibble(.init = 200, .step = 10) 
    
    arima_cv_metrics <- arima_cv %>%
      model(ARIMA(Cases ~ pdq(input$pInput,
                              input$dInput,
                              input$qInput))) %>%
      forecast(h = 1) %>%
      accuracy(arima_tbl)
    
    # generate table
    arima_cv_metrics[c("RMSE","MAE","MAPE")]
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Cross Validation Metrics</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$forecast <- renderPlot({
    
    # tuned arima model
    arima_mdl <- arima_tbl %>% 
      model(ARIMA(Cases ~ pdq(p(),
                              d(),
                              q())))
    
    # generate forecast plot
    arima_mdl %>%
      forecast(h = n()) %>%
      autoplot(arima_tbl)
    
  })
  
  output$results <- renderDataTable({
    
    # tuned arima model
    arima_mdl <- arima_tbl %>% 
      model(ARIMA(Cases ~ pdq(p(),
                              d(),
                              q())))
    
    # generate forecast results
    results <- arima_mdl %>%
      forecast(h = 26)
    
    results <- results[-c(1,3)]
    colnames(results) <- c("Date", "Forecast")
    
    results
  },
  options = list(
    pageLength = 10
  )
  )
  
}
