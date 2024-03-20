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

ets_ts <-  df %>% dplyr::select(Date, Cases)
ets_tbl <- ets_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)

p <- reactiveVal()
d <- reactiveVal()
q <- reactiveVal()
n <- reactiveVal()
sd <- reactiveVal()
ed <- reactiveVal()

ets_e <- reactiveVal()
ets_t <- reactiveVal()
ets_s <- reactiveVal()
ets_a <- reactiveVal()
ets_b <- reactiveVal()
ets_g <- reactiveVal()
ets_sd <- reactiveVal()
ets_ed <- reactiveVal()

# Define server logic required to draw a histogram
function(input, output, session) {

  observeEvent(input$initButton, {
    output$avp_plot <- renderUI({
      plotOutput('arima_avp')
    })
  })
    
  observeEvent(input$initButton, {
    output$residual_plot <- renderUI({
      plotOutput('arima_rdl')
    })
  })
  
  observeEvent(input$initButton, {
    output$metric_table <- renderUI({
      tableOutput('arima_met')
    })
  })
  
  observeEvent(input$forecastButton, {
    p(input$pInput)
    d(input$dInput)
    q(input$qInput)
    n(input$nInput)
    sd(input$periodRange[1])
    ed(input$periodRange[2])
    output$forecast_plot <- renderUI({
      plotOutput('arima_forecast')
    })
  })
  
  observeEvent(input$forecastButton, {
    p(input$pInput)
    d(input$dInput)
    q(input$qInput)
    n(input$nInput)
    sd(input$periodRange[1])
    ed(input$periodRange[2])
    output$results_table <- renderUI({
      dataTableOutput('arima_results')
    })
  })
  
  observeEvent(input$initEtsButton, {
    output$ets_avp_plot <- renderUI({
      plotOutput('ets_avp')
    })
  })
  
  observeEvent(input$initEtsButton, {
    output$ets_mdl_report <- renderUI({
      htmlOutput('ets_report')
    })
  })
  
  observeEvent(input$initEtsButton, {
    output$ets_component_plot <- renderUI({
      plotOutput('ets_component')
    })
  })
  
  observeEvent(input$initEtsButton, {
    output$ets_metric_table <- renderUI({
      tableOutput('ets_met')
    })
  })
  
  observeEvent(input$forecastEtsButton, {
    ets_e(input$eInput)
    ets_t(input$tInput)
    ets_s(input$sInput)
    ets_a(input$trendAlphaEts)
    ets_b(input$trendBetaEts)
    ets_g(input$seasonGammaEts)
    ets_n(input$nEtsInput)
    ets_sd(input$periodRangeEts[1])
    ets_ed(input$periodRangeEts[2])
    output$ets_forecast_plot <- renderUI({
      plotOutput('ets_forecast')
    })
  })
  
  observeEvent(input$forecastEtsButton, {
    ets_e(input$eInput)
    ets_t(input$tInput)
    ets_s(input$sInput)
    ets_a(input$trendAlphaEts)
    ets_b(input$trendBetaEts)
    ets_g(input$seasonGammaEts)
    ets_n(input$nEtsInput)
    ets_sd(input$periodRangeEts[1])
    ets_ed(input$periodRangeEts[2])
    output$ets_results_table <- renderUI({
      dataTableOutput('ets_results')
    })
  })
  
  output$arima_rdl <- renderPlot({
    
    # slice dates
    arima_slice <- arima_tbl %>% dplyr::filter(Date >= input$periodRange[1] &
                                                 Date <= input$periodRange[2])
    
    # tuned arima model
    arima_mdl <- arima_slice %>% 
      model(ARIMA(Cases ~ pdq(input$pInput,
                              input$dInput,
                              input$qInput)))
    
    # generate residual plot
    arima_mdl %>% gg_tsresiduals()
    
  })
  
  output$arima_avp <- renderPlot({
    
    # slice dates
    arima_slice <- arima_tbl %>% dplyr::filter(Date >= input$periodRange[1] &
                                                 Date <= input$periodRange[2])
    
    # tuned arima model
    arima_mdl <- arima_slice %>% 
      model(ARIMA(Cases ~ pdq(input$pInput,
                              input$dInput,
                              input$qInput)))
    
    # find fit
    arima_fitted <- fitted(arima_mdl)[,2:3] %>% 
      as_tibble() %>% 
      rename(Cases=.fitted)
    
    # define types
    arima_fitted$Type <- "Fit"
    arima_slice$Type <- "Observed"
    arima_avp <- dplyr::bind_rows(arima_fitted, arima_slice)
    
    # plot
    ggplot(data = arima_avp) +
      geom_line(aes(x = Date, y = Cases, colour = Type)) +
      ggtitle("Observed vs Fitted")
    
    
  })
  
  output$arima_met <- renderTable({
    
    # slice dates
    arima_slice <- arima_tbl %>% dplyr::filter(Date >= input$periodRange[1] &
                                                 Date <= input$periodRange[2])
    
    # generate cross validation metrics
    arima_cv <- arima_slice %>%
      stretch_tsibble(.init = round(0.5*nrow(arima_slice)), .step = 10) 
    
    arima_cv_metrics <- arima_cv %>%
      model(ARIMA(Cases ~ pdq(input$pInput,
                              input$dInput,
                              input$qInput))) %>%
      forecast(h = 1) %>%
      accuracy(arima_slice)
    
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
  
  output$arima_forecast <- renderPlot({
    
    # slice dates
    arima_slice <- arima_tbl %>% dplyr::filter(Date >= sd() &
                                                 Date <= ed())
    
    # tuned arima model
    arima_mdl <- arima_slice %>% 
      model(ARIMA(Cases ~ pdq(p(),
                              d(),
                              q())))
    
    # generate forecast plot
    arima_mdl %>%
      forecast(h = n()) %>%
      autoplot(arima_slice)
    
  })
  
  output$arima_results <- renderDataTable({
    
    # slice dates
    arima_slice <- arima_tbl %>% dplyr::filter(Date >= sd() &
                                                 Date <= ed())
    
    # tuned arima model
    arima_mdl <- arima_slice %>% 
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
  
  output$ets_avp <- renderPlot({
    
    # slice dates
    ets_slice <- ets_tbl %>% dplyr::filter(Date >= input$periodRangeEts[1] &
                                             Date <= input$periodRangeEts[2])
    

    # tuned ets model
    ets_mdl <- ets_slice %>%
      model(ETS(Cases ~ error(input$eInput) + 
                  trend(input$tInput, alpha = input$trendAlphaEts, beta = input$trendBetaEts) + 
                  season(input$sInput, gamma = input$seasonGammaEts)))

    # find fit
    ets_fitted <- fitted(ets_mdl)[,2:3] %>%
      as_tibble() %>%
      rename(Cases=.fitted)

    # define types
    ets_fitted$Type <- "Fit"
    ets_slice$Type <- "Observed"
    ets_avp <- dplyr::bind_rows(ets_fitted, ets_slice)

    # plot
    ggplot(data = ets_avp) +
      geom_line(aes(x = Date, y = Cases, colour = Type)) +
      ggtitle("Observed vs Fitted")
  
  })
  
  output$ets_component <- renderPlot({
    
    # slice dates
    ets_slice <- ets_tbl %>% dplyr::filter(Date >= input$periodRangeEts[1] &
                                             Date <= input$periodRangeEts[2])
    
    
    # tuned ets model
    ets_mdl <- ets_slice %>%
      model(ETS(Cases ~ error(input$eInput) + 
                  trend(input$tInput, alpha = input$trendAlphaEts, beta = input$trendBetaEts) + 
                  season(input$sInput, gamma = input$seasonGammaEts)))
    
    # generate residual plot
    components(ets_mdl) %>%
      autoplot() +
      labs(title = "Exponential Smoothing Components")
    
  })
  
  output$ets_report <- renderText({
    
    # slice dates
    ets_slice <- ets_tbl %>% dplyr::filter(Date >= input$periodRangeEts[1] &
                                             Date <= input$periodRangeEts[2])
    
    
    # tuned ets model
    ets_mdl <- ets_slice %>%
      model(ETS(Cases ~ error(input$eInput) + 
                  trend(input$tInput, alpha = input$trendAlphaEts, beta = input$trendBetaEts) + 
                  season(input$sInput, gamma = input$seasonGammaEts)))
    
    # report
    report <- capture.output(report(ets_mdl))
    msg <- ""
    for (l in report) {
      msg <- paste0(msg,l,"<br>")
    }
    
    if (grepl("NULL model", msg) == TRUE) {
      return(paste(msg,
                   "<br><br>",
                   "<div, style = 'color:red'>Unable to construct model with chosen parameters.",
                   "<br><br>",
                   "Please try again with a different set of parameters.</div>",
                   "<div, style = 'color:black'></div>"))
    } else {
      return(msg)
    }
    
  })
  
  output$ets_met <- renderTable({
    
    # slice dates
    ets_slice <- ets_tbl %>% dplyr::filter(Date >= input$periodRangeEts[1] &
                                             Date <= input$periodRangeEts[2])
    
    # generate cross validation metrics
    ets_cv <- ets_slice %>%
      stretch_tsibble(.init = round(0.5*nrow(ets_slice)), .step = 10) 
    
    ets_cv_metrics <- ets_cv %>%
      model(ETS(Cases ~ error(input$eInput) + 
                  trend(input$tInput, alpha = input$trendAlphaEts, beta = input$trendBetaEts) + 
                  season(input$sInput, gamma = input$seasonGammaEts))) %>%
      forecast(h = 1) %>%
      accuracy(ets_slice)
    
    # generate table
    ets_cv_metrics[c("RMSE","MAE","MAPE")]
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Cross Validation Metrics</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$ets_forecast <- renderPlot({
    
    # slice dates
    ets_slice <- ets_tbl %>% dplyr::filter(Date >= input$periodRangeEts[1] &
                                             Date <= input$periodRangeEts[2])
    
    
    # tuned ets model
    ets_mdl <- ets_slice %>%
      model(ETS(Cases ~ error(input$eInput) + 
                  trend(input$tInput, alpha = input$trendAlphaEts, beta = input$trendBetaEts) + 
                  season(input$sInput, gamma = input$seasonGammaEts)))
    
    # generate forecast plot
    ets_mdl %>%
      forecast(h = input$nEtsInput) %>%
      autoplot(ets_slice)
    
  })
  
  output$arima_results <- renderDataTable({
    
    # slice dates
    arima_slice <- arima_tbl %>% dplyr::filter(Date >= sd() &
                                                 Date <= ed())
    
    # tuned arima model
    arima_mdl <- arima_slice %>% 
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
