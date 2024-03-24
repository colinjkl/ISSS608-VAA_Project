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

arima_ts <-  df %>% dplyr::select(Date, Cases)
arima_tbl <- arima_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)

ets_ts <-  df %>% dplyr::select(Date, Cases)
ets_tbl <- ets_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)

var_ts <-  df %>% dplyr::select(-c("Year", "WkNo"))
var_tbl <- var_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(colnames(var_ts))


# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$lm_avp_plot <- renderUI({
    plotOutput('lm_avp')
  })
  
  output$lm_metric_table_1 <- renderUI({
    tableOutput('lm_met_1')
  })
  
  output$lm_metric_table_2 <- renderUI({
    tableOutput('lm_met_2')
  })

  output$lm_coeff_plot <- renderUI({
    plotOutput('lm_coeff', height=600)
  })
  
  output$lm_diagnostic_plot <- renderUI({
    plotOutput('lm_diagnostic', height=600)
  })
  
  output$lm_ts_plot <- renderUI({
    plotOutput('lm_ts', height = 200*(length(input$checkBoxLm)+1))
  })
  
  observeEvent(input$forecastVarButton, {
    output$var_forecast_plot <- renderUI({
      plotOutput('var_forecast', width = 600*(length(input$checkBoxVar)+1))
    })
  })
  
  output$lm_avp <- renderPlot({
    
    # slice dates
    df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                       Date <= input$periodRangeLm[2])
    
    # lm formula construction
    j <- ""
    for (s in input$checkBoxLm) {
      if (s == "avg_rainfall") {
        if (input$lmRadioAvgRainfallInput == "None") {s <- s} 
        else if (input$lmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "tot_rainfall") {
        if (input$lmRadioTotRainfallInput == "None") {s <- s} 
        else if (input$lmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_30m_rainfall") {
        if (input$lmRadioMax30mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_60m_rainfall") {
        if (input$lmRadioMax60mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_120m_rainfall") {
        if (input$lmRadioMax120mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_temp") {
        if (input$lmRadioAvgTempInput == "None") {s <- s} 
        else if (input$lmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_temp") {
        if (input$lmRadioMaxTempInput == "None") {s <- s} 
        else if (input$lmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "min_temp") {
        if (input$lmRadioMinTempInput == "None") {s <- s} 
        else if (input$lmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_wind") {
        if (input$lmRadioAvgWindInput == "None") {s <- s} 
        else if (input$lmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_wind") {
        if (input$lmRadioMaxWindInput == "None") {s <- s} 
        else if (input$lmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (j == "") {
        j <- s
      } else {
        j <- paste0(j,"+",s)
      }
    }
    
    if (j != "") {
      
      # lm model
      lm_mdl <- lm(as.formula(paste0("Cases ~ ",j)), data=df_slice)
      
      # stepwise
      if (input$lmRadioStepwise == "Forward") {
        lm_mdl <- step(lm_mdl, direction = "forward")
      } else if (input$lmRadioStepwise == "Backward") {
        lm_mdl <- step(lm_mdl, direction = "backward")
      }
      
      # melt results
      df_a <- data.frame(Date=df_slice$Date, Cases=lm_mdl$fitted.values, Type="Fitted")
      df_b <- data.frame(Date=df_slice$Date, Cases=df_slice$Cases, Type="Observed")
      df_c <- dplyr::bind_rows(df_a, df_b)
      
      # plot
      ggplot(data = df_c) +
        geom_line(aes(x = Date, y = Cases, colour = Type)) +
        ggtitle("Observed vs Fitted")
    } else {
      
      df_a <- data.frame(Date=df_slice$Date, Cases=NA, Type="Fitted")
      df_b <- data.frame(Date=df_slice$Date, Cases=df_slice$Cases, Type="Observed")
      df_c <- dplyr::bind_rows(df_a, df_b)
      
      ggplot(data = df_c) +
        geom_line(aes(x = Date, y = Cases, colour = Type)) +
        ggtitle("Observed vs Fitted")
    }
    
  })
  
  output$lm_met_1 <- renderTable({
    
    # slice dates
    df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                       Date <= input$periodRangeLm[2])
    
    # lm formula construction
    j <- ""
    for (s in input$checkBoxLm) {
      if (s == "avg_rainfall") {
        if (input$lmRadioAvgRainfallInput == "None") {s <- s} 
        else if (input$lmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "tot_rainfall") {
        if (input$lmRadioTotRainfallInput == "None") {s <- s} 
        else if (input$lmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_30m_rainfall") {
        if (input$lmRadioMax30mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_60m_rainfall") {
        if (input$lmRadioMax60mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_120m_rainfall") {
        if (input$lmRadioMax120mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_temp") {
        if (input$lmRadioAvgTempInput == "None") {s <- s} 
        else if (input$lmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_temp") {
        if (input$lmRadioMaxTempInput == "None") {s <- s} 
        else if (input$lmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "min_temp") {
        if (input$lmRadioMinTempInput == "None") {s <- s} 
        else if (input$lmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_wind") {
        if (input$lmRadioAvgWindInput == "None") {s <- s} 
        else if (input$lmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_wind") {
        if (input$lmRadioMaxWindInput == "None") {s <- s} 
        else if (input$lmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (j == "") {
        j <- s
      } else {
        j <- paste0(j,"+",s)
      }
    }
    
    if (j != "") {
      
      # lm model
      lm_mdl <- lm(as.formula(paste0("Cases ~ ",j)), data=df_slice)
      
      # stepwise
      if (input$lmRadioStepwise == "Forward") {
        lm_mdl <- step(lm_mdl, direction = "forward")
      } else if (input$lmRadioStepwise == "Backward") {
        lm_mdl <- step(lm_mdl, direction = "backward")
      }
      
      # summary of model
      summ <- summary(lm_mdl)
      
      # find MAPE
      mape <- MAPE(y_pred = lm_mdl$fitted.values, y_true = df_slice$Cases)
      
      data.frame("Adjusted R^2" = summ$adj.r.squared,
                 "F-Statistics" = summ$fstatistic[[1]],
                 "MAPE" = round(mape,2))
      
    } else {
      data.frame("Adjusted R^2" = NA,
                 "F-Statistics" = NA,
                 "MAPE" = NA)
    }
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Model Metrics</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$lm_met_2 <- renderTable({
    
    # slice dates
    df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                       Date <= input$periodRangeLm[2])
    
    # lm formula construction
    j <- ""
    for (s in input$checkBoxLm) {
      if (s == "avg_rainfall") {
        if (input$lmRadioAvgRainfallInput == "None") {s <- s} 
        else if (input$lmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "tot_rainfall") {
        if (input$lmRadioTotRainfallInput == "None") {s <- s} 
        else if (input$lmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_30m_rainfall") {
        if (input$lmRadioMax30mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_60m_rainfall") {
        if (input$lmRadioMax60mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_120m_rainfall") {
        if (input$lmRadioMax120mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_temp") {
        if (input$lmRadioAvgTempInput == "None") {s <- s} 
        else if (input$lmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_temp") {
        if (input$lmRadioMaxTempInput == "None") {s <- s} 
        else if (input$lmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "min_temp") {
        if (input$lmRadioMinTempInput == "None") {s <- s} 
        else if (input$lmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_wind") {
        if (input$lmRadioAvgWindInput == "None") {s <- s} 
        else if (input$lmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_wind") {
        if (input$lmRadioMaxWindInput == "None") {s <- s} 
        else if (input$lmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (j == "") {
        j <- s
      } else {
        j <- paste0(j,"+",s)
      }
    }
    
    if (j != "") {
      
      # lm model
      lm_mdl <- lm(as.formula(paste0("Cases ~ ",j)), data=df_slice)
      
      # stepwise
      if (input$lmRadioStepwise == "Forward") {
        lm_mdl <- step(lm_mdl, direction = "forward")
      } else if (input$lmRadioStepwise == "Backward") {
        lm_mdl <- step(lm_mdl, direction = "backward")
      }
      
      lm_mdl %>% broom::tidy()
      
    } else {
      data.frame("term" = NA,
                 "estimate" = NA,
                 "std.error" = NA,
                 "statistic" = NA,
                 "p.value" = NA)
    }
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Variables in Model (after stepwise)</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$lm_coeff <- renderPlot({
    
    # slice dates
    df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                                 Date <= input$periodRangeLm[2])
    
    # lm formula construction
    j <- ""
    for (s in input$checkBoxLm) {
      if (s == "avg_rainfall") {
        if (input$lmRadioAvgRainfallInput == "None") {s <- s} 
        else if (input$lmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "tot_rainfall") {
        if (input$lmRadioTotRainfallInput == "None") {s <- s} 
        else if (input$lmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_30m_rainfall") {
        if (input$lmRadioMax30mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_60m_rainfall") {
        if (input$lmRadioMax60mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_120m_rainfall") {
        if (input$lmRadioMax120mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_temp") {
        if (input$lmRadioAvgTempInput == "None") {s <- s} 
        else if (input$lmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_temp") {
        if (input$lmRadioMaxTempInput == "None") {s <- s} 
        else if (input$lmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "min_temp") {
        if (input$lmRadioMinTempInput == "None") {s <- s} 
        else if (input$lmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_wind") {
        if (input$lmRadioAvgWindInput == "None") {s <- s} 
        else if (input$lmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_wind") {
        if (input$lmRadioMaxWindInput == "None") {s <- s} 
        else if (input$lmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (j == "") {
        j <- s
      } else {
        j <- paste0(j,"+",s)
      }
    }
    
    if (j != "") {
      
      # lm model
      lm_mdl <- lm(as.formula(paste0("Cases ~ ",j)), data=df_slice)
      
      # stepwise
      if (input$lmRadioStepwise == "Forward") {
        lm_mdl <- step(lm_mdl, direction = "forward")
      } else if (input$lmRadioStepwise == "Backward") {
        lm_mdl <- step(lm_mdl, direction = "backward")
      }
    
    # plot
      lm_mdl %>% 
        ggcoefstats(output = "plot",
                    stats.label.args = list(size = 6),
                    title = "Variable Coefficients Statistics")
    } else {
      ggplot() + theme_light()
    }
    
  })
  
  output$lm_diagnostic <- renderPlot({
    
    # slice dates
    df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                       Date <= input$periodRangeLm[2])
    
    # lm formula construction
    j <- ""
    for (s in input$checkBoxLm) {
      if (s == "avg_rainfall") {
        if (input$lmRadioAvgRainfallInput == "None") {s <- s} 
        else if (input$lmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "tot_rainfall") {
        if (input$lmRadioTotRainfallInput == "None") {s <- s} 
        else if (input$lmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_30m_rainfall") {
        if (input$lmRadioMax30mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_60m_rainfall") {
        if (input$lmRadioMax60mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_120m_rainfall") {
        if (input$lmRadioMax120mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_temp") {
        if (input$lmRadioAvgTempInput == "None") {s <- s} 
        else if (input$lmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_temp") {
        if (input$lmRadioMaxTempInput == "None") {s <- s} 
        else if (input$lmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "min_temp") {
        if (input$lmRadioMinTempInput == "None") {s <- s} 
        else if (input$lmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_wind") {
        if (input$lmRadioAvgWindInput == "None") {s <- s} 
        else if (input$lmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_wind") {
        if (input$lmRadioMaxWindInput == "None") {s <- s} 
        else if (input$lmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (j == "") {
        j <- s
      } else {
        j <- paste0(j,"+",s)
      }
    }
    
    if (j != "") {
      
      # lm model
      lm_mdl <- lm(as.formula(paste0("Cases ~ ",j)), data=df_slice)
      
      # stepwise
      if (input$lmRadioStepwise == "Forward") {
        lm_mdl <- step(lm_mdl, direction = "forward")
      } else if (input$lmRadioStepwise == "Backward") {
        lm_mdl <- step(lm_mdl, direction = "backward")
      }
      
      
      # Create plots
      plots <- plot(check_model(lm_mdl, panel = FALSE))
      
      if (input$lmDiagnosticInput == "Posterior Predictive") {
        plots[[1]]
      } else if (input$lmDiagnosticInput == "Linearity") {
        plots[[2]]
      } else if (input$lmDiagnosticInput == "Homogeneity of Variance") {
        plots[[3]]
      } else if (input$lmDiagnosticInput == "Influential Observations") {
        plots[[4]]
      } else if (input$lmDiagnosticInput == "Collinearity") {
        plots[[5]]
      } else if (input$lmDiagnosticInput == "Normality of Residuals") {
        plots[[6]]
      }
    } else {
      ggplot() + theme_light()
    } 
    
  })
  
  output$lm_ts <- renderPlot({
    
    # slice dates
    df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                       Date <= input$periodRangeLm[2])
    
    # lm formula construction
    j <- c("Cases")
    for (s in input$checkBoxLm) {
      if (s == "avg_rainfall") {
        if (input$lmRadioAvgRainfallInput == "None") {s <- s} 
        else if (input$lmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "tot_rainfall") {
        if (input$lmRadioTotRainfallInput == "None") {s <- s} 
        else if (input$lmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_30m_rainfall") {
        if (input$lmRadioMax30mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_60m_rainfall") {
        if (input$lmRadioMax60mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_120m_rainfall") {
        if (input$lmRadioMax120mRainfallInput == "None") {s <- s} 
        else if (input$lmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_temp") {
        if (input$lmRadioAvgTempInput == "None") {s <- s} 
        else if (input$lmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_temp") {
        if (input$lmRadioMaxTempInput == "None") {s <- s} 
        else if (input$lmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "min_temp") {
        if (input$lmRadioMinTempInput == "None") {s <- s} 
        else if (input$lmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "avg_wind") {
        if (input$lmRadioAvgWindInput == "None") {s <- s} 
        else if (input$lmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
      }
      if (s == "max_wind") {
        if (input$lmRadioMaxWindInput == "None") {s <- s} 
        else if (input$lmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
        else if (input$lmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
        else if (input$lmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
      }

      j <- c(j, s)

    }
    
    lm_tbl <- df_slice %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)
    
    first_var <- TRUE
    for (v in j) {
      if (first_var == TRUE) {
        p <- eval(parse(text = paste0("lm_tbl %>% autoplot(",v,")")))
        first_var <- FALSE
      } else {
        p <- p / eval(parse(text = paste0("lm_tbl %>% autoplot(",v,")")))
      }
    }
    
    p
    
    
  })
  
  output$arima_rdl <- renderPlot({
    
    if (input$arimaTuneButton == 0) return()
    
    isolate({
      
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
    
  })
  
  output$arima_avp <- renderPlot({
    
    if (input$arimaTuneButton == 0) return()
    
    isolate({
      
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
    
    
  })
  
  output$arima_met <- renderTable({
    
    if (input$arimaTuneButton == 0) return()
    
    isolate({
      
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
      
    })
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Cross Validation Metrics</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$arima_forecast <- renderPlot({
    
    if (input$forecastArimaButton == 0) return()
    
    isolate({
      
      # slice dates
      arima_slice <- arima_tbl %>% dplyr::filter(Date >= input$periodRange[1] &
                                                   Date <= input$periodRange[2])
      
      # tuned arima model
      arima_mdl <- arima_slice %>% 
        model(ARIMA(Cases ~ pdq(input$pInput,
                                input$dInput,
                                input$qInput)))
      
      # generate forecast plot
      arima_mdl %>%
        forecast(h = input$nInput) %>%
        autoplot(arima_slice)
      
    })
    
  })
  
  output$arima_results <- renderDataTable({
    
    if (input$forecastArimaButton == 0) return()
    
    isolate({
      
      # slice dates
      arima_slice <- arima_tbl %>% dplyr::filter(Date >= input$periodRange[1] &
                                                   Date <= input$periodRange[2])
      
      # tuned arima model
      arima_mdl <- arima_slice %>% 
        model(ARIMA(Cases ~ pdq(input$pInput,
                                input$dInput,
                                input$qInput)))
      
      # generate forecast results
      results <- arima_mdl %>%
        forecast(h = input$nInput)
      
      results <- results[-c(1,3)]
      colnames(results) <- c("Date", "Forecast")
      
      results
    })
    
  },
  options = list(
    pageLength = 10
  ))
  
  output$ets_avp <- renderPlot({
    
    if (input$etsTuneButton == 0) return()
    
    isolate({
      
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
  
  })
  
  output$ets_component <- renderPlot({
    
    if (input$etsTuneButton == 0) return()
    
    isolate({
      
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
    
  })
  
  output$ets_report <- renderText({
    
    if (input$etsTuneButton == 0) return()
    
    isolate({
      
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
    
  })
  
  output$ets_met <- renderTable({
    
    if (input$etsTuneButton == 0) return()
    
    isolate({
      
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
      
    })
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Cross Validation Metrics</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$ets_forecast <- renderPlot({
    
    if (input$forecastEtsButton == 0) return()
    
    isolate({
      
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
    
  })
  
  output$ets_results <- renderDataTable({
    
    if (input$forecastEtsButton == 0) return()
    
    isolate({
      
      # slice dates
      ets_slice <- ets_tbl %>% dplyr::filter(Date >= input$periodRangeEts[1] &
                                               Date <= input$periodRangeEts[2])
      
      
      # tuned ets model
      ets_mdl <- ets_slice %>%
        model(ETS(Cases ~ error(input$eInput) + 
                    trend(input$tInput, alpha = input$trendAlphaEts, beta = input$trendBetaEts) + 
                    season(input$sInput, gamma = input$seasonGammaEts)))
      
      # generate forecast results
      results <- ets_mdl %>%
        forecast(h = input$nEtsInput)
      
      results <- results[-c(1,3)]
      colnames(results) <- c("Date", "Forecast")
      
      results
      
    })
    
  },
  options = list(
    pageLength = 10
  ))
  
  output$var_avp <- renderPlot({
    
    if (input$varTuneButton == 0) return()
    
    isolate({
      
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
    
  })
  
  output$var_acf <- renderPlot({
    
    if (input$varTuneButton == 0) return()
    
    isolate({
      
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
    
  })
  
  output$var_met <- renderTable({
    
    if (input$varTuneButton == 0) return()
    
    isolate({
      
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
      
    })
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Cross Validation Metrics</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$var_forecast <- renderPlot({
    
    if (input$forecastVarButton == 0) return()
    
    isolate({
      
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
    
  })
  
}
