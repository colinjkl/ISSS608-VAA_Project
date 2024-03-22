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

df <- read_csv("data/dengue_climate_joined_by_week_transformed_diff.csv")

df$Date <- lubridate::ymd(lubridate::parse_date_time(paste(df$Year, df$WkNo, 1, sep="/"),'Y/W/w'))

arima_ts <-  df %>% dplyr::select(Date, Cases)
arima_tbl <- arima_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)

ets_ts <-  df %>% dplyr::select(Date, Cases)
ets_tbl <- ets_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)

var_ts <-  df %>% dplyr::select(-c("Year", "WkNo"))
var_tbl <- var_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(colnames(var_ts))

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
ets_n <- reactiveVal()
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
  
  observeEvent(input$initVarButton, {
    output$var_avp_plot <- renderUI({
      plotOutput('var_avp')
    })
  })
  
  observeEvent(input$initVarButton, {
    output$var_acf_plot <- renderUI({
      plotOutput('var_acf', height = 800)
    })
  })
  
  observeEvent(input$initVarButton, {
    output$var_met_table <- renderUI({
      tableOutput('var_met')
    })
  })
  
  observeEvent(input$forecastVarButton, {
    output$var_forecast_plot <- renderUI({
      plotOutput('var_forecast', width = 600*(length(input$checkBoxVar)+1))
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
      forecast(h = n())
    
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
    ets_slice <- ets_tbl %>% dplyr::filter(Date >= ets_sd() &
                                             Date <= ets_ed())
    
    
    # tuned ets model
    ets_mdl <- ets_slice %>%
      model(ETS(Cases ~ error(ets_e()) + 
                  trend(ets_t(), alpha = ets_a(), beta = ets_b()) + 
                  season(ets_s(), gamma = ets_g())))
    
    # generate forecast plot
    ets_mdl %>%
      forecast(h = ets_n()) %>%
      autoplot(ets_slice)
    
  })
  
  output$ets_results <- renderDataTable({
    
    # slice dates
    ets_slice <- ets_tbl %>% dplyr::filter(Date >= ets_sd() &
                                             Date <= ets_ed())
    
    # tuned ets model
    ets_mdl <- ets_slice %>%
      model(ETS(Cases ~ error(ets_e()) + 
                  trend(ets_t(), alpha = ets_a(), beta = ets_b()) + 
                  season(ets_s(), gamma = ets_g())))
    
    # generate forecast results
    results <- ets_mdl %>%
      forecast(h = ets_n())
    
    results <- results[-c(1,3)]
    colnames(results) <- c("Date", "Forecast")
    
    results
  },
  options = list(
    pageLength = 10
  )
  )
  
  output$var_avp <- renderPlot({
    
    # slice dates
    var_slice <- var_tbl %>% dplyr::filter(Date >= input$periodRangeVar[1] &
                                                 Date <= input$periodRangeVar[2])
    
    # Parse inputs
    v <- "Cases"
    mode <- '"aicc"'
    
    # Mega if else loop to conjure string input for VAR model formula
    # Sorry cant think of a better way...
    for (s in input$checkBoxVar) {
      if (s == "avg_rainfall") {
        if (input$radioAvgRainfallInput == "None") {s <- s} 
        else if (input$radioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgRainfallInput > 0) {s <- paste0("diff",input$diffAvgRainfallInput,"_",s)}
      }
      if (s == "tot_rainfall") {
        if (input$radioTotRainfallInput == "None") {s <- s} 
        else if (input$radioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioTotRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffTotRainfallInput > 0) {s <- paste0("diff",input$diffTotRainfallInput,"_",s)}
      }
      if (s == "max_30m_rainfall") {
        if (input$radioMax30mRainfallInput == "None") {s <- s} 
        else if (input$radioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax30mRainfallInput > 0) {s <- paste0("diff",input$diffMax30mRainfallInput,"_",s)}
      }
      if (s == "max_60m_rainfall") {
        if (input$radioMax60mRainfallInput == "None") {s <- s} 
        else if (input$radioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax60mRainfallInput > 0) {s <- paste0("diff",input$diffMax60mRainfallInput,"_",s)}
      }
      if (s == "max_120m_rainfall") {
        if (input$radioMax120mRainfallInput == "None") {s <- s} 
        else if (input$radioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax120mRainfallInput > 0) {s <- paste0("diff",input$diffMax120mRainfallInput,"_",s)}
      }
      if (s == "avg_temp") {
        if (input$radioAvgTempInput == "None") {s <- s} 
        else if (input$radioAvgTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgTempInput > 0) {s <- paste0("diff",input$diffAvgTempInput,"_",s)}
      }
      if (s == "max_temp") {
        if (input$radioMaxTempInput == "None") {s <- s} 
        else if (input$radioMaxTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMaxTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMaxTempInput > 0) {s <- paste0("diff",input$diffMaxTempInput,"_",s)}
      }
      if (s == "min_temp") {
        if (input$radioMinTempInput == "None") {s <- s} 
        else if (input$radioMinTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMinTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMinTempInput > 0) {s <- paste0("diff",input$diffMinTempInput,"_",s)}
      }
      if (s == "avg_wind") {
        if (input$radioAvgWindInput == "None") {s <- s} 
        else if (input$radioAvgWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgWindInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgWindInput > 0) {s <- paste0("diff",input$diffAvgWindInput,"_",s)}
      }
      if (s == "max_wind") {
        if (input$radioMaxWindInput == "None") {s <- s} 
        else if (input$radioMaxWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMaxWindInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMaxWindInput > 0) {s <- paste0("diff",input$diffMaxWindInput,"_",s)}
      }
      v <- paste0(v,",",s)
    }
    
    strr <- paste0('var_tbl %>% model(',mode,' = VAR(vars(',v,'), ic=',mode,'))')
    var_mdl <- eval(parse(text = strr))
    
    # find fit
    var_fitted <- fitted(var_mdl)[,2:3] %>% 
      as_tibble() 
    
    colnames(var_fitted) <- c("Date", "Cases")
    
    # define types
    var_fitted$Type <- "Fit"
    var_slice$Type <- "Observed"
    var_avp <- dplyr::bind_rows(var_fitted, var_slice)
    
    # plot
    ggplot(data = var_avp) +
      geom_line(aes(x = Date, y = Cases, colour = Type)) +
      ggtitle("Observed vs Fitted")
    
    
  })
  
  output$var_acf <- renderPlot({
    
    # slice dates
    var_slice <- var_tbl %>% dplyr::filter(Date >= input$periodRangeVar[1] &
                                             Date <= input$periodRangeVar[2])
    
    # Parse inputs
    v <- "Cases"
    mode <- '"aicc"'
    
    # Mega if else loop to conjure string input for VAR model formula
    # Sorry cant think of a better way...
    for (s in input$checkBoxVar) {
      if (s == "avg_rainfall") {
        if (input$radioAvgRainfallInput == "None") {s <- s} 
        else if (input$radioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgRainfallInput > 0) {s <- paste0("diff",input$diffAvgRainfallInput,"_",s)}
      }
      if (s == "tot_rainfall") {
        if (input$radioTotRainfallInput == "None") {s <- s} 
        else if (input$radioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioTotRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffTotRainfallInput > 0) {s <- paste0("diff",input$diffTotRainfallInput,"_",s)}
      }
      if (s == "max_30m_rainfall") {
        if (input$radioMax30mRainfallInput == "None") {s <- s} 
        else if (input$radioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax30mRainfallInput > 0) {s <- paste0("diff",input$diffMax30mRainfallInput,"_",s)}
      }
      if (s == "max_60m_rainfall") {
        if (input$radioMax60mRainfallInput == "None") {s <- s} 
        else if (input$radioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax60mRainfallInput > 0) {s <- paste0("diff",input$diffMax60mRainfallInput,"_",s)}
      }
      if (s == "max_120m_rainfall") {
        if (input$radioMax120mRainfallInput == "None") {s <- s} 
        else if (input$radioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax120mRainfallInput > 0) {s <- paste0("diff",input$diffMax120mRainfallInput,"_",s)}
      }
      if (s == "avg_temp") {
        if (input$radioAvgTempInput == "None") {s <- s} 
        else if (input$radioAvgTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgTempInput > 0) {s <- paste0("diff",input$diffAvgTempInput,"_",s)}
      }
      if (s == "max_temp") {
        if (input$radioMaxTempInput == "None") {s <- s} 
        else if (input$radioMaxTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMaxTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMaxTempInput > 0) {s <- paste0("diff",input$diffMaxTempInput,"_",s)}
      }
      if (s == "min_temp") {
        if (input$radioMinTempInput == "None") {s <- s} 
        else if (input$radioMinTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMinTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMinTempInput > 0) {s <- paste0("diff",input$diffMinTempInput,"_",s)}
      }
      if (s == "avg_wind") {
        if (input$radioAvgWindInput == "None") {s <- s} 
        else if (input$radioAvgWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgWindInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgWindInput > 0) {s <- paste0("diff",input$diffAvgWindInput,"_",s)}
      }
      if (s == "max_wind") {
        if (input$radioMaxWindInput == "None") {s <- s} 
        else if (input$radioMaxWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMaxWindInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMaxWindInput > 0) {s <- paste0("diff",input$diffMaxWindInput,"_",s)}
      }
      v <- paste0(v,",",s)
    }
    
    strr <- paste0('var_tbl %>% model(',mode,' = VAR(vars(',v,'), ic=',mode,'))')
    var_mdl <- eval(parse(text = strr))
    
    # Plot ACF
    var_mdl %>%
      augment() %>%
      ACF() %>%
      autoplot() %>%
      plot_layout(height="200%")
    
  })
  
  output$var_met <- renderTable({
    
    # slice dates
    var_slice <- var_tbl %>% dplyr::filter(Date >= input$periodRangeVar[1] &
                                             Date <= input$periodRangeVar[2])
    
    var_cv <- var_slice %>%
      stretch_tsibble(.init = round(0.5*nrow(var_slice)), .step = 20) 
    
    # Parse inputs
    v <- "Cases"
    mode <- '"aicc"'
    
    # Mega if else loop to conjure string input for VAR model formula
    # Sorry cant think of a better way...
    for (s in input$checkBoxVar) {
      if (s == "avg_rainfall") {
        if (input$radioAvgRainfallInput == "None") {s <- s} 
        else if (input$radioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgRainfallInput > 0) {s <- paste0("diff",input$diffAvgRainfallInput,"_",s)}
      }
      if (s == "tot_rainfall") {
        if (input$radioTotRainfallInput == "None") {s <- s} 
        else if (input$radioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioTotRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffTotRainfallInput > 0) {s <- paste0("diff",input$diffTotRainfallInput,"_",s)}
      }
      if (s == "max_30m_rainfall") {
        if (input$radioMax30mRainfallInput == "None") {s <- s} 
        else if (input$radioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax30mRainfallInput > 0) {s <- paste0("diff",input$diffMax30mRainfallInput,"_",s)}
      }
      if (s == "max_60m_rainfall") {
        if (input$radioMax60mRainfallInput == "None") {s <- s} 
        else if (input$radioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax60mRainfallInput > 0) {s <- paste0("diff",input$diffMax60mRainfallInput,"_",s)}
      }
      if (s == "max_120m_rainfall") {
        if (input$radioMax120mRainfallInput == "None") {s <- s} 
        else if (input$radioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax120mRainfallInput > 0) {s <- paste0("diff",input$diffMax120mRainfallInput,"_",s)}
      }
      if (s == "avg_temp") {
        if (input$radioAvgTempInput == "None") {s <- s} 
        else if (input$radioAvgTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgTempInput > 0) {s <- paste0("diff",input$diffAvgTempInput,"_",s)}
      }
      if (s == "max_temp") {
        if (input$radioMaxTempInput == "None") {s <- s} 
        else if (input$radioMaxTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMaxTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMaxTempInput > 0) {s <- paste0("diff",input$diffMaxTempInput,"_",s)}
      }
      if (s == "min_temp") {
        if (input$radioMinTempInput == "None") {s <- s} 
        else if (input$radioMinTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMinTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMinTempInput > 0) {s <- paste0("diff",input$diffMinTempInput,"_",s)}
      }
      if (s == "avg_wind") {
        if (input$radioAvgWindInput == "None") {s <- s} 
        else if (input$radioAvgWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgWindInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgWindInput > 0) {s <- paste0("diff",input$diffAvgWindInput,"_",s)}
      }
      if (s == "max_wind") {
        if (input$radioMaxWindInput == "None") {s <- s} 
        else if (input$radioMaxWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMaxWindInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMaxWindInput > 0) {s <- paste0("diff",input$diffMaxWindInput,"_",s)}
      }
      v <- paste0(v,",",s)
    }
    
    strr <- paste0('var_cv %>% model(',mode,' = VAR(vars(',v,'), ic=',mode,')) %>% forecast(h = 1) %>% accuracy(var_slice)')
    var_cv_metrics <- eval(parse(text = strr))
    
    # generate table
    if (length(input$checkBoxVar) == 0) {
      return(var_cv_metrics[c(".model","RMSE","MAE","MAPE")])
    } else {
      return(var_cv_metrics[c(".response","RMSE","MAE","MAPE")])
    }
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Cross Validation Metrics</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$var_forecast <- renderPlot({
    
    # slice dates
    var_slice <- var_tbl %>% dplyr::filter(Date >= input$periodRangeVar[1] &
                                             Date <= input$periodRangeVar[2])
    
    
    # Parse inputs
    v <- "Cases"
    mode <- '"aicc"'
    
    # Mega if else loop to conjure string input for VAR model formula
    # Sorry cant think of a better way...
    for (s in input$checkBoxVar) {
      if (s == "avg_rainfall") {
        if (input$radioAvgRainfallInput == "None") {s <- s} 
        else if (input$radioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgRainfallInput > 0) {s <- paste0("diff",input$diffAvgRainfallInput,"_",s)}
      }
      if (s == "tot_rainfall") {
        if (input$radioTotRainfallInput == "None") {s <- s} 
        else if (input$radioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioTotRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffTotRainfallInput > 0) {s <- paste0("diff",input$diffTotRainfallInput,"_",s)}
      }
      if (s == "max_30m_rainfall") {
        if (input$radioMax30mRainfallInput == "None") {s <- s} 
        else if (input$radioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax30mRainfallInput > 0) {s <- paste0("diff",input$diffMax30mRainfallInput,"_",s)}
      }
      if (s == "max_60m_rainfall") {
        if (input$radioMax60mRainfallInput == "None") {s <- s} 
        else if (input$radioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax60mRainfallInput > 0) {s <- paste0("diff",input$diffMax60mRainfallInput,"_",s)}
      }
      if (s == "max_120m_rainfall") {
        if (input$radioMax120mRainfallInput == "None") {s <- s} 
        else if (input$radioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMax120mRainfallInput > 0) {s <- paste0("diff",input$diffMax120mRainfallInput,"_",s)}
      }
      if (s == "avg_temp") {
        if (input$radioAvgTempInput == "None") {s <- s} 
        else if (input$radioAvgTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgTempInput > 0) {s <- paste0("diff",input$diffAvgTempInput,"_",s)}
      }
      if (s == "max_temp") {
        if (input$radioMaxTempInput == "None") {s <- s} 
        else if (input$radioMaxTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMaxTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMaxTempInput > 0) {s <- paste0("diff",input$diffMaxTempInput,"_",s)}
      }
      if (s == "min_temp") {
        if (input$radioMinTempInput == "None") {s <- s} 
        else if (input$radioMinTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMinTempInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMinTempInput > 0) {s <- paste0("diff",input$diffMinTempInput,"_",s)}
      }
      if (s == "avg_wind") {
        if (input$radioAvgWindInput == "None") {s <- s} 
        else if (input$radioAvgWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioAvgWindInput == "Z") {s <- paste0("z_",s)}
        if (input$diffAvgWindInput > 0) {s <- paste0("diff",input$diffAvgWindInput,"_",s)}
      }
      if (s == "max_wind") {
        if (input$radioMaxWindInput == "None") {s <- s} 
        else if (input$radioMaxWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$radioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$radioMaxWindInput == "Z") {s <- paste0("z_",s)}
        if (input$diffMaxWindInput > 0) {s <- paste0("diff",input$diffMaxWindInput,"_",s)}
      }
      v <- paste0(v,",",s)
    }
    
    strr <- paste0('var_tbl %>% model(',mode,' = VAR(vars(',v,'), ic=',mode,'))')
    var_mdl <- eval(parse(text = strr))
    
    # generate forecast plot
    var_mdl %>%
      forecast(h = input$nVarInput) %>%
      autoplot(var_slice)
    
  })
  
}
