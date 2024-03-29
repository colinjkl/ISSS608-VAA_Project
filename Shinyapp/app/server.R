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
  ggstatsplot,
  MLmetrics,
  performance,
  caret,
  qqplotr,
  lubridate,
  ggthemes, 
  sf, 
  terra, 
  gstat, 
  automap, 
  tmap, 
  viridis, 
  zoo,
  rstantools,
  urca
)

stations <- read_csv("data/RainfallStation.csv")

climate_data <- read_csv("data/clean_climate_data.csv")

df <- read_csv("data/dengue_climate_joined_by_week_transformed_diff.csv")

df$Date <- lubridate::ymd(lubridate::parse_date_time(paste(df$Year, df$WkNo, 1, sep="/"),'Y/W/w'))

arima_ts <-  df %>% dplyr::select(Date, Cases)
arima_tbl <- arima_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)

ets_ts <-  df %>% dplyr::select(Date, Cases)
ets_tbl <- ets_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(Cases)

var_ts <-  df %>% dplyr::select(-c("Year", "WkNo"))
var_tbl <- var_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(colnames(var_ts))

tslm_ts <-  df %>% dplyr::select(-c("Year", "WkNo"))
tslm_tbl <- tslm_ts %>% as_tsibble(index = Date) %>% fill_gaps(.full = TRUE) %>% fill(colnames(tslm_ts))


# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$lm_ts_plot <- renderUI({
    input$lmTuneButton
    
    isolate({
      plotOutput('lm_ts', height = 200*(length(input$checkBoxLm)+1))
    })
  })
  
  output$var_acf_plot <- renderUI({
    input$varTuneButton
    
    isolate({
      plotOutput('var_acf', height = 200*(length(input$checkBoxVar)+1))
    })
  })
  
  output$var_other_plot <- renderUI({
    input$varTuneButton
    
    isolate({
      plotOutput('var_other', height = 200*(length(input$checkBoxVar)+1))
    })
  })
  
  # anova plot 1 ----
  output$anova_1 <- renderPlot({
    
    if (input$anovaTuneButton == 0) return()
    
    isolate({
      
      #Inputs
      input_station <- input$stationsAnova1
      input_msmt <- input$variableAnova
      start_year <- input$periodRangeAnova[1]
      end_year <- input$periodRangeAnova[2]
      input_year <- seq(start_year, end_year)
      
      input_data <- climate_data %>%
        select('Station','Year','Month','Day',input_msmt)%>%
        filter(Station == input_station,
               Year %in% input_year)
      
      ggbetweenstats(
        data = input_data,
        x = Year, 
        y = !!rlang::sym(input_msmt),
        type = "p",
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE,
        centrality.label.args = list(size  = 5),
        ggplot.component = list(theme(text = element_text(size = 14),
                                      plot.title = element_text(size = 24))),
        title = paste0("Comparison of ",input_msmt," between years (", start_year, "-", end_year,")")
      )
      
    })
    
  })
  
  # anova plot 2 ----
  output$anova_2 <- renderPlot({
    
    if (input$anovaTuneButton == 0) return()
    
    isolate({
      
      #Inputs
      input_station <- input$stationsAnova2
      input_msmt <- input$variableAnova
      start_year <- input$periodRangeAnova[1]
      end_year <- input$periodRangeAnova[2]
      input_year <- seq(start_year, end_year)
      
      input_data <- climate_data %>%
        select('Station','Year','Month','Day',input_msmt)%>%
        filter(Station == input_station,
               Year %in% input_year)
      
      ggbetweenstats(
        data = input_data,
        x = Year, 
        y = !!rlang::sym(input_msmt),
        type = "p",
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE,
        centrality.label.args = list(size  = 5),
        ggplot.component = list(theme(text = element_text(size = 14),
                                      plot.title = element_text(size = 24))),
        title = paste0("Comparison of ",input_msmt," between years (", start_year, "-", end_year,")")
      ) 
      
    })
    
  })
  
  # geo plot 1 ----
  output$geo_1 <- renderPlot({
    
    if (input$geoTuneButton == 0) return()
    
    isolate({
      
      if (input$methodGeoIdw == "IDW") {
      
        #Inputs
        input_year <- input$periodRangeGeo1
        input_msmt <- input$variableGeoIdw
        input_agg <- input$aggGeoIdw
        input_nmax <- input$nmaxGeoIdw
        
        #Preparing the input data
        input_data <- climate_data %>%
          select('Station','Year','Month','Day',input_msmt)%>%
          filter(Year == input_year)
        
        rfdata <- input_data%>%
          select(c(1,5))%>%
          group_by(Station)%>%
          summarise(year_agg = switch(input_agg,
                                      sum = sum(!!rlang::sym(input_msmt)),
                                      mean = mean(!!rlang::sym(input_msmt))
          ))%>%
          mutate(
            year_agg = ifelse(is.na(year_agg), 0, year_agg)
          )%>%
          ungroup()
        
        #Converting aspatial data into geospatial data
        rfdata <- rfdata %>%
          left_join(stations)
        
        rfdata_sf<- st_as_sf(rfdata,
                             coords = c("Longitude", "Latitude"),
                             crs = 4326)%>%
          st_transform(crs = 3414)
        
        #Importing planning subzone boundary data
        mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019")%>%
          st_transform(crs = 3414)
        
        grid <- terra::rast(mpsz2019, 
                            nrows = 690, 
                            ncols = 1075)
        
        xy <- terra::xyFromCell(grid, 
                                1:ncell(grid))
        
        coop <- st_as_sf(as.data.frame(xy), 
                         coords = c("x", "y"),
                         crs = st_crs(mpsz2019))
        coop <- st_filter(coop, mpsz2019)
        
        res <- gstat(formula = year_agg ~ 1, 
                     locations = rfdata_sf, 
                     nmax = input_nmax,
                     set = list(idp = 0))
        
        resp <- predict(res, coop)
        
        resp$x <- st_coordinates(resp)[,1]
        resp$y <- st_coordinates(resp)[,2]
        resp$pred <- resp$var1.pred
        
        pred <- terra::rasterize(resp, grid, 
                                 field = "pred", 
                                 fun = "mean")
        
        tmap_options(check.and.fix = TRUE)
        tmap_mode("plot")
        tm_shape(pred) + 
          tm_raster(alpha = 0.6, 
                    palette = "viridis",
                    title = paste0(input_msmt)) +
          tm_layout(main.title = paste0("Distribution of ",input_msmt," for Year ",input_year),
                    main.title.position = "center",
                    main.title.size = 1.2,
                    legend.height = 0.45, 
                    legend.width = 0.35,
                    frame = TRUE) +
          tm_compass(type="8star", size = 2) +
          tm_scale_bar() +
          tm_grid(alpha =0.2)
      
      } else {
        
        #Inputs for prototyping
        input_year <- input$periodRangeGeo1
        input_msmt <- input$variableGeoKrig
        input_agg <- input$aggGeoKrig
        input_model <- input$modelGeoKrig
        input_psill <- input$psilGeoKrig
        input_range <- input$rangeGeoKrig
        input_nugget <- input$nuggetGeoKrig
        
         #Preparing the input data
        input_data <- climate_data %>%
          select('Station','Year','Month','Day',input_msmt)%>%
          filter(Year == input_year)
        
        rfdata <- input_data%>%
          select(c(1,5))%>%
          group_by(Station)%>%
          summarise(year_agg = switch(input_agg,
                                      sum = sum(!!rlang::sym(input_msmt)),
                                      mean = mean(!!rlang::sym(input_msmt))
          ))%>%
          mutate(
            year_agg = ifelse(is.na(year_agg), 0, year_agg)
          )%>%
          ungroup()
        
        #Converting aspatial data into geospatial data
        rfdata <- rfdata %>%
          left_join(stations)
        
        rfdata_sf<- st_as_sf(rfdata,
                             coords = c("Longitude", "Latitude"),
                             crs = 4326)%>%
          st_transform(crs = 3414)
        
        #Importing planning subzone boundary data
        mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019")%>%
          st_transform(crs = 3414)
        
        # fitting variogram model
        v <- variogram(year_agg ~ 1, 
                       data = rfdata_sf)
        
        fv <- fit.variogram(object = v,
                            model = vgm(
                              psill = input_psill, 
                              model = input_model,
                              range = input_range,  
                              nugget = input_nugget))
        
        k <- gstat(formula = year_agg ~ 1, 
                   data = rfdata_sf, 
                   model = fv)
        
        grid <- terra::rast(mpsz2019, 
                            nrows = 690, 
                            ncols = 1075)
        
        xy <- terra::xyFromCell(grid, 
                                1:ncell(grid))
        
        coop <- st_as_sf(as.data.frame(xy), 
                         coords = c("x", "y"),
                         crs = st_crs(mpsz2019))
        coop <- st_filter(coop, mpsz2019)
        
        resp <- predict(k, coop)
        resp$x <- st_coordinates(resp)[,1]
        resp$y <- st_coordinates(resp)[,2]
        resp$pred <- resp$var1.pred
        resp$pred <- resp$pred
        
        kpred <- terra::rasterize(resp, grid, 
                                  field = "pred")
        
        tmap_options(check.and.fix = TRUE)
        tmap_mode("plot")
        tm_shape(kpred) + 
          tm_raster(alpha = 0.6, 
                    palette = "viridis",
                    title = paste0(input_msmt)) +
          tm_layout(main.title = paste0("Distribution of ",input_msmt," for Year ",input_year),
                    main.title.position = "center",
                    main.title.size = 1.2,
                    legend.height = 0.45, 
                    legend.width = 0.35,
                    frame = TRUE) +
          tm_compass(type="8star", size = 2) +
          tm_scale_bar() +
          tm_grid(alpha =0.2)
        
      }
      
    })
    
  })
  
  # geo plot 2 ----
  output$geo_2 <- renderPlot({
    
    if (input$geoTuneButton == 0) return()
    
    isolate({
      
      if (input$methodGeoIdw == "IDW") {
        
        #Inputs
        input_year <- input$periodRangeGeo2
        input_msmt <- input$variableGeoIdw
        input_agg <- input$aggGeoIdw
        input_nmax <- input$nmaxGeoIdw
        
        #Preparing the input data
        input_data <- climate_data %>%
          select('Station','Year','Month','Day',input_msmt)%>%
          filter(Year == input_year)
        
        rfdata <- input_data%>%
          select(c(1,5))%>%
          group_by(Station)%>%
          summarise(year_agg = switch(input_agg,
                                      sum = sum(!!rlang::sym(input_msmt)),
                                      mean = mean(!!rlang::sym(input_msmt))
          ))%>%
          mutate(
            year_agg = ifelse(is.na(year_agg), 0, year_agg)
          )%>%
          ungroup()
        
        #Converting aspatial data into geospatial data
        rfdata <- rfdata %>%
          left_join(stations)
        
        rfdata_sf<- st_as_sf(rfdata,
                             coords = c("Longitude", "Latitude"),
                             crs = 4326)%>%
          st_transform(crs = 3414)
        
        #Importing planning subzone boundary data
        mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019")%>%
          st_transform(crs = 3414)
        
        grid <- terra::rast(mpsz2019, 
                            nrows = 690, 
                            ncols = 1075)
        
        xy <- terra::xyFromCell(grid, 
                                1:ncell(grid))
        
        coop <- st_as_sf(as.data.frame(xy), 
                         coords = c("x", "y"),
                         crs = st_crs(mpsz2019))
        coop <- st_filter(coop, mpsz2019)
        
        res <- gstat(formula = year_agg ~ 1, 
                     locations = rfdata_sf, 
                     nmax = input_nmax,
                     set = list(idp = 0))
        
        resp <- predict(res, coop)
        
        resp$x <- st_coordinates(resp)[,1]
        resp$y <- st_coordinates(resp)[,2]
        resp$pred <- resp$var1.pred
        
        pred <- terra::rasterize(resp, grid, 
                                 field = "pred", 
                                 fun = "mean")
        
        tmap_options(check.and.fix = TRUE)
        tmap_mode("plot")
        tm_shape(pred) + 
          tm_raster(alpha = 0.6, 
                    palette = "viridis",
                    title = paste0(input_msmt)) +
          tm_layout(main.title = paste0("Distribution of ",input_msmt," for Year ",input_year),
                    main.title.position = "center",
                    main.title.size = 1.2,
                    legend.height = 0.45, 
                    legend.width = 0.35,
                    frame = TRUE) +
          tm_compass(type="8star", size = 2) +
          tm_scale_bar() +
          tm_grid(alpha =0.2)
        
      } else {
        
        #Inputs for prototyping
        input_year <- input$periodRangeGeo2
        input_msmt <- input$variableGeoKrig
        input_agg <- input$aggGeoKrig
        input_model <- input$modelGeoKrig
        input_psill <- input$psilGeoKrig
        input_range <- input$rangeGeoKrig
        input_nugget <- input$nuggetGeoKrig
        
        #Preparing the input data
        input_data <- climate_data %>%
          select('Station','Year','Month','Day',input_msmt)%>%
          filter(Year == input_year)
        
        rfdata <- input_data%>%
          select(c(1,5))%>%
          group_by(Station)%>%
          summarise(year_agg = switch(input_agg,
                                      sum = sum(!!rlang::sym(input_msmt)),
                                      mean = mean(!!rlang::sym(input_msmt))
          ))%>%
          mutate(
            year_agg = ifelse(is.na(year_agg), 0, year_agg)
          )%>%
          ungroup()
        
        #Converting aspatial data into geospatial data
        rfdata <- rfdata %>%
          left_join(stations)
        
        rfdata_sf<- st_as_sf(rfdata,
                             coords = c("Longitude", "Latitude"),
                             crs = 4326)%>%
          st_transform(crs = 3414)
        
        #Importing planning subzone boundary data
        mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019")%>%
          st_transform(crs = 3414)
        
        # fitting variogram model
        v <- variogram(year_agg ~ 1, 
                       data = rfdata_sf)
        
        fv <- fit.variogram(object = v,
                            model = vgm(
                              psill = input_psill, 
                              model = input_model,
                              range = input_range,  
                              nugget = input_nugget))
        
        k <- gstat(formula = year_agg ~ 1, 
                   data = rfdata_sf, 
                   model = fv)
        
        grid <- terra::rast(mpsz2019, 
                            nrows = 690, 
                            ncols = 1075)
        
        xy <- terra::xyFromCell(grid, 
                                1:ncell(grid))
        
        coop <- st_as_sf(as.data.frame(xy), 
                         coords = c("x", "y"),
                         crs = st_crs(mpsz2019))
        coop <- st_filter(coop, mpsz2019)
        
        resp <- predict(k, coop)
        resp$x <- st_coordinates(resp)[,1]
        resp$y <- st_coordinates(resp)[,2]
        resp$pred <- resp$var1.pred
        resp$pred <- resp$pred
        
        kpred <- terra::rasterize(resp, grid, 
                                  field = "pred")
        
        tmap_options(check.and.fix = TRUE)
        tmap_mode("plot")
        tm_shape(kpred) + 
          tm_raster(alpha = 0.6, 
                    palette = "viridis",
                    title = paste0(input_msmt)) +
          tm_layout(main.title = paste0("Distribution of ",input_msmt," for Year ",input_year),
                    main.title.position = "center",
                    main.title.size = 1.2,
                    legend.height = 0.45, 
                    legend.width = 0.35,
                    frame = TRUE) +
          tm_compass(type="8star", size = 2) +
          tm_scale_bar() +
          tm_grid(alpha =0.2)
        
      }
      
    })
    
  })
  
  # geo vario 1 ----
  output$vario_1 <- renderPlot({
    
    if (input$geoTuneButton == 0) return()
    
    isolate({
      
      if (input$methodGeoIdw == "IDW") {
        
        return()
        
      } else {
        
        #Inputs for prototyping
        input_year <- input$periodRangeGeo1
        input_msmt <- input$variableGeoKrig
        input_agg <- input$aggGeoKrig
        input_model <- input$modelGeoKrig
        input_psill <- input$psilGeoKrig
        input_range <- input$rangeGeoKrig
        input_nugget <- input$nuggetGeoKrig
        
        #Preparing the input data
        input_data <- climate_data %>%
          select('Station','Year','Month','Day',input_msmt)%>%
          filter(Year == input_year)
        
        rfdata <- input_data%>%
          select(c(1,5))%>%
          group_by(Station)%>%
          summarise(year_agg = switch(input_agg,
                                      sum = sum(!!rlang::sym(input_msmt)),
                                      mean = mean(!!rlang::sym(input_msmt))
          ))%>%
          mutate(
            year_agg = ifelse(is.na(year_agg), 0, year_agg)
          )%>%
          ungroup()
        
        #Converting aspatial data into geospatial data
        rfdata <- rfdata %>%
          left_join(stations)
        
        rfdata_sf<- st_as_sf(rfdata,
                             coords = c("Longitude", "Latitude"),
                             crs = 4326)%>%
          st_transform(crs = 3414)
        
        #Importing planning subzone boundary data
        mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019")%>%
          st_transform(crs = 3414)
        
        # fitting variogram model
        v <- variogram(year_agg ~ 1, 
                       data = rfdata_sf)
        
        plot(v)

      }
      
    })
    
  })
  
  # geo vario 2 ----
  output$vario_2 <- renderPlot({
    
    if (input$geoTuneButton == 0) return()
    
    isolate({
      
      if (input$methodGeoIdw == "IDW") {
        
        return()
        
      } else {
        
        #Inputs for prototyping
        input_year <- input$periodRangeGeo2
        input_msmt <- input$variableGeoKrig
        input_agg <- input$aggGeoKrig
        input_model <- input$modelGeoKrig
        input_psill <- input$psilGeoKrig
        input_range <- input$rangeGeoKrig
        input_nugget <- input$nuggetGeoKrig
        
        #Preparing the input data
        input_data <- climate_data %>%
          select('Station','Year','Month','Day',input_msmt)%>%
          filter(Year == input_year)
        
        rfdata <- input_data%>%
          select(c(1,5))%>%
          group_by(Station)%>%
          summarise(year_agg = switch(input_agg,
                                      sum = sum(!!rlang::sym(input_msmt)),
                                      mean = mean(!!rlang::sym(input_msmt))
          ))%>%
          mutate(
            year_agg = ifelse(is.na(year_agg), 0, year_agg)
          )%>%
          ungroup()
        
        #Converting aspatial data into geospatial data
        rfdata <- rfdata %>%
          left_join(stations)
        
        rfdata_sf<- st_as_sf(rfdata,
                             coords = c("Longitude", "Latitude"),
                             crs = 4326)%>%
          st_transform(crs = 3414)
        
        #Importing planning subzone boundary data
        mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019")%>%
          st_transform(crs = 3414)
        
        # fitting variogram model
        v <- variogram(year_agg ~ 1, 
                       data = rfdata_sf)
        
        plot(v)
        
      }
      
    })
    
  })
  

  # lm avp ----
  output$lm_avp <- renderPlot({
    
    if (input$lmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                         Date <= input$periodRangeLm[2])
      
      v <- "Cases"
      if (input$lmRadioCasesInput == "None") {v <- v} 
      else if (input$lmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$lmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$lmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      
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
        lm_mdl <- lm(as.formula(paste0(v," ~ ",j)), data=df_slice)
        
        # stepwise
        if (input$lmRadioStepwise == "Forward") {
          lm_mdl <- step(lm_mdl, direction = "forward")
        } else if (input$lmRadioStepwise == "Backward") {
          lm_mdl <- step(lm_mdl, direction = "backward")
        }
        
        # melt results
        df_a <- data.frame(Date=df_slice$Date, Cases=lm_mdl$fitted.values, Type="Fitted")
        df_b <- data.frame(Date=df_slice$Date, Cases=df_slice[[v]], Type="Observed")
        df_c <- dplyr::bind_rows(df_a, df_b)
        
        colnames(df_c)[2] <- v
        
        # plot
        ggplot(data = df_c) +
          geom_line(aes(x = Date, y = .data[[v]], colour = Type)) +
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
    
  })
  
  # lm met 1 ----
  output$lm_met_1 <- renderTable({
    
    if (input$lmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                         Date <= input$periodRangeLm[2])
      
      # lm formula construction
      
      v <- "Cases"
      if (input$lmRadioCasesInput == "None") {v <- v} 
      else if (input$lmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$lmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$lmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      
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
        lm_mdl <- lm(as.formula(paste0(v," ~ ",j)), data=df_slice)
        
        # stepwise
        if (input$lmRadioStepwise == "Forward") {
          lm_mdl <- step(lm_mdl, direction = "forward")
        } else if (input$lmRadioStepwise == "Backward") {
          lm_mdl <- step(lm_mdl, direction = "backward")
        }
        
        # summary of model
        summ <- summary(lm_mdl)
        
        # find MAPE
        mape <- MAPE(y_pred = lm_mdl$fitted.values, y_true = df_slice[[v]])
        
        data.frame("Adjusted R^2" = summ$adj.r.squared,
                   "F-Statistics" = summ$fstatistic[[1]],
                   "MAPE" = round(mape,2))
        
      } else {
        data.frame("Adjusted R^2" = NA,
                   "F-Statistics" = NA,
                   "MAPE" = NA)
      }
      
    })
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Model Metrics</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  # lm met 2 ----
  output$lm_met_2 <- renderTable({
    
    if (input$lmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                         Date <= input$periodRangeLm[2])
      
      # lm formula construction
      v <- "Cases"
      if (input$lmRadioCasesInput == "None") {v <- v} 
      else if (input$lmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$lmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$lmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      
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
        lm_mdl <- lm(as.formula(paste0(v," ~ ",j)), data=df_slice)
        
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
      
    })
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Variables in Model (after stepwise)</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  # lm coeff ----
  output$lm_coeff <- renderPlot({
    
    if (input$lmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                         Date <= input$periodRangeLm[2])
      
      # lm formula construction
      v <- "Cases"
      if (input$lmRadioCasesInput == "None") {v <- v} 
      else if (input$lmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$lmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$lmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      
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
        lm_mdl <- lm(as.formula(paste0(v," ~ ",j)), data=df_slice)
        
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
    
  })
  
  # lm diagnostic ----
  output$lm_diagnostic <- renderPlot({
    
    if (input$lmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                         Date <= input$periodRangeLm[2])
      
      # lm formula construction
      v <- "Cases"
      if (input$lmRadioCasesInput == "None") {v <- v} 
      else if (input$lmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$lmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$lmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      
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
        lm_mdl <- lm(as.formula(paste0(v," ~ ",j)), data=df_slice)
        
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
    
  })
  
  # lm ts ----
  output$lm_ts <- renderPlot({
    
    if (input$lmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      df_slice <- df %>% dplyr::filter(Date >= input$periodRangeLm[1] &
                                         Date <= input$periodRangeLm[2])
      
      # lm formula construction
      v <- "Cases"
      if (input$lmRadioCasesInput == "None") {v <- v} 
      else if (input$lmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$lmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$lmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      
      j <- c(v)
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
      for (k in j) {
        if (first_var == TRUE) {
          p <- eval(parse(text = paste0("lm_tbl %>% autoplot(",k,")")))
          first_var <- FALSE
        } else {
          p <- p / eval(parse(text = paste0("lm_tbl %>% autoplot(",k,")")))
        }
      }
      
      p
      
    })
    
  })
  
  # tslm avp ----
  output$tslm_avp <- renderPlot({
    
    if (input$tslmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      tslm_slice <- tslm_tbl %>% dplyr::filter(Date >= input$periodRangeTslm[1] &
                                               Date <= input$periodRangeTslm[2])
      
      # Parse inputs
      v <- "Cases"
      
      if (input$tslmRadioCasesInput == "None") {v <- v} 
      else if (input$tslmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$tslmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$tslmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$tslmDiffCasesInput > 0) {v <- paste0("diff",input$tslmDiffCasesInput,"_",v)}
      
      v1 <- v
      j = ""
      
      # Mega if else loop to conjure string input for VAR model formula
      # Sorry cant think of a better way...
      for (s in input$checkBoxTslm) {
        if (s == "avg_rainfall") {
          if (input$tslmRadioAvgRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgRainfallInput > 0) {s <- paste0("diff",input$tslmDiffAvgRainfallInput,"_",s)}
        }
        if (s == "tot_rainfall") {
          if (input$tslmRadioTotRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffTotRainfallInput > 0) {s <- paste0("diff",input$tslmDiffTotRainfallInput,"_",s)}
        }
        if (s == "max_30m_rainfall") {
          if (input$tslmRadioMax30mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax30mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax30mRainfallInput,"_",s)}
        }
        if (s == "max_60m_rainfall") {
          if (input$tslmRadioMax60mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax60mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax60mRainfallInput,"_",s)}
        }
        if (s == "max_120m_rainfall") {
          if (input$tslmRadioMax120mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax120mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax120mRainfallInput,"_",s)}
        }
        if (s == "avg_temp") {
          if (input$tslmRadioAvgTempInput == "None") {s <- s} 
          else if (input$tslmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgTempInput > 0) {s <- paste0("diff",input$tslmDiffAvgTempInput,"_",s)}
        }
        if (s == "max_temp") {
          if (input$tslmRadioMaxTempInput == "None") {s <- s} 
          else if (input$tslmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMaxTempInput > 0) {s <- paste0("diff",input$tslmDiffMaxTempInput,"_",s)}
        }
        if (s == "min_temp") {
          if (input$tslmRadioMinTempInput == "None") {s <- s} 
          else if (input$tslmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMinTempInput > 0) {s <- paste0("diff",input$tslmDiffMinTempInput,"_",s)}
        }
        if (s == "avg_wind") {
          if (input$tslmRadioAvgWindInput == "None") {s <- s} 
          else if (input$tslmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgWindInput > 0) {s <- paste0("diff",input$tslmDiffAvgWindInput,"_",s)}
        }
        if (s == "max_wind") {
          if (input$tslmRadioMaxWindInput == "None") {s <- s} 
          else if (input$tslmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMaxWindInput > 0) {s <- paste0("diff",input$tslmDiffMaxWindInput,"_",s)}
        }
        if (j == "") {
          j <- s
        } else {
          j <- paste0(j,"+",s)
        }
      }
      
      if (j != "") {
        
        # tslm model
        tslm_mdl <- tslm_slice %>%
          model(TSLM(as.formula(paste0(v," ~ ",j))))

        # find fit
        tslm_fitted <- fitted(tslm_mdl)[,2:3] %>% 
          as_tibble() 
        
        colnames(tslm_fitted) <- c("Date", v1)
        
        # define types
        tslm_fitted$Type <- "Fit"
        tslm_slice$Type <- "Observed"
        tslm_avp <- dplyr::bind_rows(tslm_fitted, tslm_slice)
        
        # plot
        ggplot(data = tslm_avp) +
          geom_line(aes(x = Date, y = .data[[v1]], colour = Type)) +
          ggtitle("Observed vs Fitted")
        
      } else {
        
        df_a <- data.frame(Date=tslm_slice$Date, Cases=NA, Type="Fitted")
        df_b <- data.frame(Date=tslm_slice$Date, Cases=tslm_slice$Cases, Type="Observed")
        df_c <- dplyr::bind_rows(df_a, df_b)
        
        ggplot(data = df_c) +
          geom_line(aes(x = Date, y = Cases, colour = Type)) +
          ggtitle("Observed vs Fitted")
      }
      
    })
    
  })
  
  # tslm met ----
  output$tslm_met <- renderTable({
    
    if (input$tslmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      tslm_slice <- tslm_tbl %>% dplyr::filter(Date >= input$periodRangeTslm[1] &
                                                 Date <= input$periodRangeTslm[2])
      
      # Parse inputs
      v <- "Cases"
      
      if (input$tslmRadioCasesInput == "None") {v <- v} 
      else if (input$tslmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$tslmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$tslmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$tslmDiffCasesInput > 0) {v <- paste0("diff",input$tslmDiffCasesInput,"_",v)}
      
      v1 <- v
      j = ""
      
      # Mega if else loop to conjure string input for VAR model formula
      # Sorry cant think of a better way...
      for (s in input$checkBoxTslm) {
        if (s == "avg_rainfall") {
          if (input$tslmRadioAvgRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgRainfallInput > 0) {s <- paste0("diff",input$tslmDiffAvgRainfallInput,"_",s)}
        }
        if (s == "tot_rainfall") {
          if (input$tslmRadioTotRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffTotRainfallInput > 0) {s <- paste0("diff",input$tslmDiffTotRainfallInput,"_",s)}
        }
        if (s == "max_30m_rainfall") {
          if (input$tslmRadioMax30mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax30mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax30mRainfallInput,"_",s)}
        }
        if (s == "max_60m_rainfall") {
          if (input$tslmRadioMax60mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax60mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax60mRainfallInput,"_",s)}
        }
        if (s == "max_120m_rainfall") {
          if (input$tslmRadioMax120mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax120mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax120mRainfallInput,"_",s)}
        }
        if (s == "avg_temp") {
          if (input$tslmRadioAvgTempInput == "None") {s <- s} 
          else if (input$tslmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgTempInput > 0) {s <- paste0("diff",input$tslmDiffAvgTempInput,"_",s)}
        }
        if (s == "max_temp") {
          if (input$tslmRadioMaxTempInput == "None") {s <- s} 
          else if (input$tslmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMaxTempInput > 0) {s <- paste0("diff",input$tslmDiffMaxTempInput,"_",s)}
        }
        if (s == "min_temp") {
          if (input$tslmRadioMinTempInput == "None") {s <- s} 
          else if (input$tslmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMinTempInput > 0) {s <- paste0("diff",input$tslmDiffMinTempInput,"_",s)}
        }
        if (s == "avg_wind") {
          if (input$tslmRadioAvgWindInput == "None") {s <- s} 
          else if (input$tslmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgWindInput > 0) {s <- paste0("diff",input$tslmDiffAvgWindInput,"_",s)}
        }
        if (s == "max_wind") {
          if (input$tslmRadioMaxWindInput == "None") {s <- s} 
          else if (input$tslmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMaxWindInput > 0) {s <- paste0("diff",input$tslmDiffMaxWindInput,"_",s)}
        }
        if (j == "") {
          j <- s
        } else {
          j <- paste0(j,"+",s)
        }
      }
      
      
      
      if (j != "") {
        # tslm model
        tslm_cv_metrics <- tslm_slice %>%
          model(TSLM(as.formula(paste0(v," ~ ",j)))) %>% accuracy()
        
        return(tslm_cv_metrics[c("RMSE","MAE","MAPE")])
        
      } else {
        data.frame("RMSE" = NA,
                   "MAE" = NA,
                   "MAPE" = NA)
      }
      
    })
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Model Metrics</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  # tslm residuals ----
  output$tslm_rdl <- renderPlot({
    
    if (input$tslmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      tslm_slice <- tslm_tbl %>% dplyr::filter(Date >= input$periodRangeTslm[1] &
                                                 Date <= input$periodRangeTslm[2])
      
      # Parse inputs
      v <- "Cases"
      
      if (input$tslmRadioCasesInput == "None") {v <- v} 
      else if (input$tslmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$tslmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$tslmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$tslmDiffCasesInput > 0) {v <- paste0("diff",input$tslmDiffCasesInput,"_",v)}
      
      v1 <- v
      j = ""
      
      # Mega if else loop to conjure string input for VAR model formula
      # Sorry cant think of a better way...
      for (s in input$checkBoxTslm) {
        if (s == "avg_rainfall") {
          if (input$tslmRadioAvgRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgRainfallInput > 0) {s <- paste0("diff",input$tslmDiffAvgRainfallInput,"_",s)}
        }
        if (s == "tot_rainfall") {
          if (input$tslmRadioTotRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffTotRainfallInput > 0) {s <- paste0("diff",input$tslmDiffTotRainfallInput,"_",s)}
        }
        if (s == "max_30m_rainfall") {
          if (input$tslmRadioMax30mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax30mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax30mRainfallInput,"_",s)}
        }
        if (s == "max_60m_rainfall") {
          if (input$tslmRadioMax60mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax60mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax60mRainfallInput,"_",s)}
        }
        if (s == "max_120m_rainfall") {
          if (input$tslmRadioMax120mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax120mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax120mRainfallInput,"_",s)}
        }
        if (s == "avg_temp") {
          if (input$tslmRadioAvgTempInput == "None") {s <- s} 
          else if (input$tslmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgTempInput > 0) {s <- paste0("diff",input$tslmDiffAvgTempInput,"_",s)}
        }
        if (s == "max_temp") {
          if (input$tslmRadioMaxTempInput == "None") {s <- s} 
          else if (input$tslmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMaxTempInput > 0) {s <- paste0("diff",input$tslmDiffMaxTempInput,"_",s)}
        }
        if (s == "min_temp") {
          if (input$tslmRadioMinTempInput == "None") {s <- s} 
          else if (input$tslmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMinTempInput > 0) {s <- paste0("diff",input$tslmDiffMinTempInput,"_",s)}
        }
        if (s == "avg_wind") {
          if (input$tslmRadioAvgWindInput == "None") {s <- s} 
          else if (input$tslmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgWindInput > 0) {s <- paste0("diff",input$tslmDiffAvgWindInput,"_",s)}
        }
        if (s == "max_wind") {
          if (input$tslmRadioMaxWindInput == "None") {s <- s} 
          else if (input$tslmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMaxWindInput > 0) {s <- paste0("diff",input$tslmDiffMaxWindInput,"_",s)}
        }
        if (j == "") {
          j <- s
        } else {
          j <- paste0(j,"+",s)
        }
      }
      
      if (j != "") {
        
        # tslm model
        tslm_mdl <- tslm_slice %>%
          model(TSLM(as.formula(paste0(v," ~ ",j))))
        
        # tslm residuals
        gg_tsresiduals(tslm_mdl)
        
      } else {
        
        return()
      }
      
    })
    
  })
  
  # tslm coeffs ----
  output$tslm_coeff <- renderPlot({
    
    if (input$tslmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      tslm_slice <- tslm_tbl %>% dplyr::filter(Date >= input$periodRangeTslm[1] &
                                                 Date <= input$periodRangeTslm[2])
      
      # Parse inputs
      v <- "Cases"
      
      if (input$tslmRadioCasesInput == "None") {v <- v} 
      else if (input$tslmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$tslmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$tslmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$tslmDiffCasesInput > 0) {v <- paste0("diff",input$tslmDiffCasesInput,"_",v)}
      
      v1 <- v
      j = ""
      
      # Mega if else loop to conjure string input for VAR model formula
      # Sorry cant think of a better way...
      for (s in input$checkBoxTslm) {
        if (s == "avg_rainfall") {
          if (input$tslmRadioAvgRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgRainfallInput > 0) {s <- paste0("diff",input$tslmDiffAvgRainfallInput,"_",s)}
        }
        if (s == "tot_rainfall") {
          if (input$tslmRadioTotRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffTotRainfallInput > 0) {s <- paste0("diff",input$tslmDiffTotRainfallInput,"_",s)}
        }
        if (s == "max_30m_rainfall") {
          if (input$tslmRadioMax30mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax30mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax30mRainfallInput,"_",s)}
        }
        if (s == "max_60m_rainfall") {
          if (input$tslmRadioMax60mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax60mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax60mRainfallInput,"_",s)}
        }
        if (s == "max_120m_rainfall") {
          if (input$tslmRadioMax120mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax120mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax120mRainfallInput,"_",s)}
        }
        if (s == "avg_temp") {
          if (input$tslmRadioAvgTempInput == "None") {s <- s} 
          else if (input$tslmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgTempInput > 0) {s <- paste0("diff",input$tslmDiffAvgTempInput,"_",s)}
        }
        if (s == "max_temp") {
          if (input$tslmRadioMaxTempInput == "None") {s <- s} 
          else if (input$tslmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMaxTempInput > 0) {s <- paste0("diff",input$tslmDiffMaxTempInput,"_",s)}
        }
        if (s == "min_temp") {
          if (input$tslmRadioMinTempInput == "None") {s <- s} 
          else if (input$tslmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMinTempInput > 0) {s <- paste0("diff",input$tslmDiffMinTempInput,"_",s)}
        }
        if (s == "avg_wind") {
          if (input$tslmRadioAvgWindInput == "None") {s <- s} 
          else if (input$tslmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgWindInput > 0) {s <- paste0("diff",input$tslmDiffAvgWindInput,"_",s)}
        }
        if (s == "max_wind") {
          if (input$tslmRadioMaxWindInput == "None") {s <- s} 
          else if (input$tslmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMaxWindInput > 0) {s <- paste0("diff",input$tslmDiffMaxWindInput,"_",s)}
        }
        if (j == "") {
          j <- s
        } else {
          j <- paste0(j,"+",s)
        }
      }
      
      if (j != "") {
        
        # tslm model
        tslm_mdl <- tslm_slice %>%
          model(TSLM(as.formula(paste0(v," ~ ",j))))
        
        # tslm residuals
        ggcoefstats(tslm_mdl %>% tidy(), 
                    output = "plot")
        
      } else {
        
        return()
      }
      
    })
    
  })
  
  # tslm coeffs error ----
  output$tslm_coeff_error <- renderTable({
    
    if (input$tslmTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      tslm_slice <- tslm_tbl %>% dplyr::filter(Date >= input$periodRangeTslm[1] &
                                                 Date <= input$periodRangeTslm[2])
      
      # Parse inputs
      v <- "Cases"
      
      if (input$tslmRadioCasesInput == "None") {v <- v} 
      else if (input$tslmRadioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$tslmRadioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$tslmRadioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$tslmDiffCasesInput > 0) {v <- paste0("diff",input$tslmDiffCasesInput,"_",v)}
      
      v1 <- v
      j = ""
      
      # Mega if else loop to conjure string input for VAR model formula
      # Sorry cant think of a better way...
      for (s in input$checkBoxTslm) {
        if (s == "avg_rainfall") {
          if (input$tslmRadioAvgRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioAvgRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgRainfallInput > 0) {s <- paste0("diff",input$tslmDiffAvgRainfallInput,"_",s)}
        }
        if (s == "tot_rainfall") {
          if (input$tslmRadioTotRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioTotRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioTotRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioTotRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffTotRainfallInput > 0) {s <- paste0("diff",input$tslmDiffTotRainfallInput,"_",s)}
        }
        if (s == "max_30m_rainfall") {
          if (input$tslmRadioMax30mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax30mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax30mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax30mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax30mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax30mRainfallInput,"_",s)}
        }
        if (s == "max_60m_rainfall") {
          if (input$tslmRadioMax60mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax60mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax60mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax60mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax60mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax60mRainfallInput,"_",s)}
        }
        if (s == "max_120m_rainfall") {
          if (input$tslmRadioMax120mRainfallInput == "None") {s <- s} 
          else if (input$tslmRadioMax120mRainfallInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMax120mRainfallInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMax120mRainfallInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMax120mRainfallInput > 0) {s <- paste0("diff",input$tslmDiffMax120mRainfallInput,"_",s)}
        }
        if (s == "avg_temp") {
          if (input$tslmRadioAvgTempInput == "None") {s <- s} 
          else if (input$tslmRadioAvgTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgTempInput > 0) {s <- paste0("diff",input$tslmDiffAvgTempInput,"_",s)}
        }
        if (s == "max_temp") {
          if (input$tslmRadioMaxTempInput == "None") {s <- s} 
          else if (input$tslmRadioMaxTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMaxTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMaxTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMaxTempInput > 0) {s <- paste0("diff",input$tslmDiffMaxTempInput,"_",s)}
        }
        if (s == "min_temp") {
          if (input$tslmRadioMinTempInput == "None") {s <- s} 
          else if (input$tslmRadioMinTempInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMinTempInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMinTempInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMinTempInput > 0) {s <- paste0("diff",input$tslmDiffMinTempInput,"_",s)}
        }
        if (s == "avg_wind") {
          if (input$tslmRadioAvgWindInput == "None") {s <- s} 
          else if (input$tslmRadioAvgWindInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioAvgWindInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioAvgWindInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffAvgWindInput > 0) {s <- paste0("diff",input$tslmDiffAvgWindInput,"_",s)}
        }
        if (s == "max_wind") {
          if (input$tslmRadioMaxWindInput == "None") {s <- s} 
          else if (input$tslmRadioMaxWindInput == "Log") {s <- paste0("log_",s)} 
          else if (input$tslmRadioMaxWindInput == "MinMax") {s <- paste0("mm_",s)} 
          else if (input$tslmRadioMaxWindInput == "Z") {s <- paste0("z_",s)}
          if (input$tslmDiffMaxWindInput > 0) {s <- paste0("diff",input$tslmDiffMaxWindInput,"_",s)}
        }
        if (j == "") {
          j <- s
        } else {
          j <- paste0(j,"+",s)
        }
      }
      print(j)
      if (j != "") {
        
        
        
        # tslm model
        tslm_mdl <- tslm_slice %>%
          model(TSLM(as.formula(paste0(v," ~ ",j))))
        
        # tslm residuals
        tslm_mdl %>% tidy() %>% select(-c(".model"))
        
      } else {
        
        return()
      }
      
    })
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Model Estimates</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  # arima auto ----
  observe({
    
    req(input$arimaAutoButton)
    
    isolate({
      
      # slice dates
      arima_slice <- arima_tbl %>% dplyr::filter(Date >= input$periodRange[1] &
                                                   Date <= input$periodRange[2])
      
      # tuned arima model
      arima_mdl <- arima_slice %>% 
        model(ARIMA(Cases))
      
      # Extract params
      a <- capture.output(report(arima_mdl))
      c <- ""
      for (b in a) {
        c <- paste0(c,b,"\n")
      }
      params <- gsub("[\\(\\)]", "", regmatches(c, gregexpr("\\(.*?\\)", c))[[1]])
      params <- as.list(strsplit(params, ',')[[1]])
      
      para1 <- params[[1]]
      para2 <- params[[2]]
      para3 <- params[[3]]
      
      # Update inputs
      updateNumericInput(session, "pInput", value = as.numeric(para1))
      updateNumericInput(session, "dInput", value = as.numeric(para2))
      updateNumericInput(session, "qInput", value = as.numeric(para3))
      
    })
    
  })
  
  # arima rdl ----
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
  
  # arima avp ----
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
  
  # arima met ----
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
  
  # arima forecast ----
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
  
  # arima results ----
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
  
  # ets auto ----
  observe({
    
    req(input$etsAutoButton)
    
    isolate({
      
      # slice dates
      ets_slice <- ets_tbl %>% dplyr::filter(Date >= input$periodRangeEts[1] &
                                               Date <= input$periodRangeEts[2])
      
      # tuned ets model
      ets_mdl <- ets_slice %>%
        model(ETS(Cases))
      
      # Extract params
      a <- capture.output(report(ets_mdl))
      para1 <- ""
      para2 <- ""
      para3 <- ""
      c <- ""
      for (b in a) {
        if (grepl("alpha", b)) {
          para1 <- as.list(strsplit(b, ' = ')[[1]])
          para1 <- trimws(para1[[2]])
        }
        if (grepl("beta", b)) {
          para2 <- as.list(strsplit(b, ' = ')[[1]])
          para2 <- trimws(para2[[2]])
        }
        if (grepl("gamma", b)) {
          para3 <- as.list(strsplit(b, ' = ')[[1]])
          para3 <- trimws(para3[[2]])
        }
        c <- paste0(c,b,"\n")
      }
      params <- gsub("[\\(\\)]", "", regmatches(c, gregexpr("\\(.*?\\)", c))[[1]])
      params <- as.list(strsplit(params, ',')[[1]])
      para4 <- params[[1]]
      para5 <- params[[2]]
      para6 <- params[[3]]
      
      # Update inputs
      if (para4 == "M") {
        updateSelectInput(session, "eInput", selected = "M")
        updateNumericInput(session, "trendAlphaEts", value = as.numeric(para1))
      } else if (para4 == "A") {
        updateSelectInput(session, "eInput", selected = "A")
        updateNumericInput(session, "trendAlphaEts", value = as.numeric(para1))
      }
      
      if (para5 == "M") {
        updateSelectInput(session, "tInput", selected = "M")
        updateNumericInput(session, "trendBetaEts", value = as.numeric(para2))
      } else if (para5 == "A") {
        updateSelectInput(session, "tInput", selected = "A")
        updateNumericInput(session, "trendBetaEts", value = as.numeric(para2))
      } else {
        updateSelectInput(session, "tInput", selected = "N")
        updateNumericInput(session, "trendBetaEts", value = 0)
      }

      if (para6 == "M") {
        updateSelectInput(session, "sInput", selected = "M")
        updateNumericInput(session, "seasonGammaEts", value = as.numeric(para3))
      } else if (para6 == "A") {
        updateSelectInput(session, "sInput", selected = "A")
        updateNumericInput(session, "seasonGammaEts", value = as.numeric(para3))
      } else {
        updateSelectInput(session, "sInput", selected = "N")
        updateNumericInput(session, "seasonGammaEts", value = 0)
      }
      
    })
    
  })
  
  # ets avp ----
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
  
  # ets component ----
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
  
  # ets report ----
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
  
  # ets_met ----
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
  
  # ets forecast ----
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
  
  # ets results ----
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
  
  # var avp ----
  output$var_avp <- renderPlot({
    
    if (input$varTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      var_slice <- var_tbl %>% dplyr::filter(Date >= input$periodRangeVar[1] &
                                               Date <= input$periodRangeVar[2])
      
      # Parse inputs
      v <- "Cases"
      mode <- '"aicc"'
      
      if (input$radioCasesInput == "None") {v <- v} 
      else if (input$radioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$radioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$radioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$diffCasesInput > 0) {v <- paste0("diff",input$diffCasesInput,"_",v)}
      
      v1 <- v
      
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
      
      strr <- paste0('var_slice %>% model(',mode,' = VAR(vars(',v,'), ic=',mode,'))')
      print(strr)
      var_mdl <- eval(parse(text = strr))
      
      # find fit
      var_fitted <- fitted(var_mdl)[,2:3] %>% 
        as_tibble() 
      
      colnames(var_fitted) <- c("Date", v1)
      
      # define types
      var_fitted$Type <- "Fit"
      var_slice$Type <- "Observed"
      var_avp <- dplyr::bind_rows(var_fitted, var_slice)
      
      # plot
      ggplot(data = var_avp) +
        geom_line(aes(x = Date, y = .data[[v1]], colour = Type)) +
        ggtitle("Observed vs Fitted")
      
    })
    
  })
  
  # var acf ----
  output$var_acf <- renderPlot({
    
    if (input$varTuneButton == 0) return()
    
    isolate({
      
      # slice dates
      var_slice <- var_tbl %>% dplyr::filter(Date >= input$periodRangeVar[1] &
                                               Date <= input$periodRangeVar[2])
      
      # Parse inputs
      v <- "Cases"
      mode <- '"aicc"'
      
      if (input$radioCasesInput == "None") {v <- v} 
      else if (input$radioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$radioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$radioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$diffCasesInput > 0) {v <- paste0("diff",input$diffCasesInput,"_",v)}
      
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
      
      strr <- paste0('var_slice %>% model(',mode,' = VAR(vars(',v,'), ic=',mode,'))')
      var_mdl <- eval(parse(text = strr))
      
      # Plot ACF
      var_mdl %>%
        augment() %>%
        ACF() %>%
        autoplot() %>%
        plot_layout(height="200%")
      
    })
    
  })
  
  # var met ----
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
      
      if (input$radioCasesInput == "None") {v <- v} 
      else if (input$radioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$radioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$radioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$diffCasesInput > 0) {v <- paste0("diff",input$diffCasesInput,"_",v)}
      
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
  
  # var forecast ----
  output$var_forecast <- renderPlot({
    
    if (input$forecastVarButton == 0) return()
    
    isolate({
      
      # slice dates
      var_slice <- var_tbl %>% dplyr::filter(Date >= input$periodRangeVar[1] &
                                               Date <= input$periodRangeVar[2])
      
      
      # Parse inputs
      v <- "Cases"
      mode <- '"aicc"'
      
      if (input$radioCasesInput == "None") {v <- v} 
      else if (input$radioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$radioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$radioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$diffCasesInput > 0) {v <- paste0("diff",input$diffCasesInput,"_",v)}
      
      tv <- v
      
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
      
      strr <- paste0('var_slice %>% model(',mode,' = VAR(vars(',v,'), ic=',mode,'))')
      var_mdl <- eval(parse(text = strr))
      
      # generate forecast plot
      fc <- var_mdl %>%
        forecast(h = input$nVarInput)
      
      ci <- hilo(fc)
      
      if (v != tv) {
        var_fc <- data.frame(Date=fc$Date, 
                             temp=fc$.mean[,tv],
                             lwr80=ci$`80%`[[tv]]$lower,
                             upr80=ci$`80%`[[tv]]$upper,
                             lwr95=ci$`95%`[[tv]]$lower,
                             upr95=ci$`95%`[[tv]]$upper,
                             Type="Forecast")
      } else {
        var_fc <- data.frame(Date=fc$Date, 
                             temp=fc$.mean,
                             lwr80=ci$`80%`$lower,
                             upr80=ci$`80%`$upper,
                             lwr95=ci$`95%`$lower,
                             upr95=ci$`95%`$upper,
                             Type="Forecast")
      }
      
      colnames(var_fc)[2] <- tv
      
      var_obs <- data.frame(Date=var_slice$Date, 
                            temp=var_slice[tv],
                            lwr80=NA,
                            upr80=NA,
                            lwr95=NA,
                            upr95=NA,
                            Type="Observed")
      
      var_plot <- dplyr::bind_rows(var_obs, var_fc)
      
      ggplot(data = var_plot, aes(x=Date, y=.data[[tv]] ,colour=Type)) + 
        geom_line() +
        geom_ribbon(aes(ymin=lwr80, ymax=upr80), alpha=0.2, linetype=0, fill="blue") +
        geom_ribbon(aes(ymin=lwr95, ymax=upr95), alpha=0.1, linetype=0, fill="blue") + 
        scale_colour_manual(values = c("Observed"="black", "Forecast"="blue")) +
        ggtitle(paste0("Forecast ",tv," for next ",input$nVarInput," periods")) +
        theme(plot.title = element_text(size=32))
      
    })
    
  })
  
  # var other ----
  output$var_other <- renderPlot({
    
    if (input$forecastVarButton == 0) return()
    
    isolate({
      
      # slice dates
      var_slice <- var_tbl %>% dplyr::filter(Date >= input$periodRangeVar[1] &
                                               Date <= input$periodRangeVar[2])
      
      
      # Parse inputs
      v <- "Cases"
      mode <- '"aicc"'
      
      if (input$radioCasesInput == "None") {v <- v} 
      else if (input$radioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$radioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$radioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$diffCasesInput > 0) {v <- paste0("diff",input$diffCasesInput,"_",v)}
      
      l <- c(v)
      v1 <- v
      
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
        l <- c(l,s)
      }
      
      if (v == v1) return()
      strr <- paste0('var_slice %>% model(',mode,' = VAR(vars(',v,'), ic=',mode,'))')
      var_mdl <- eval(parse(text = strr))
      
      # generate forecast plot
      fc <- var_mdl %>%
        forecast(h = input$nVarInput)
      
      ci <- hilo(fc)
      first_plot <- TRUE
      for (tv in l) {
        
        if (tv == v1) next
        
        var_fc <- data.frame(Date=fc$Date, 
                             temp=fc$.mean[,tv],
                             lwr80=ci$`80%`[[tv]]$lower,
                             upr80=ci$`80%`[[tv]]$upper,
                             lwr95=ci$`95%`[[tv]]$lower,
                             upr95=ci$`95%`[[tv]]$upper,
                             Type="Forecast")
        
        colnames(var_fc)[2] <- tv
        
        var_obs <- data.frame(Date=var_slice$Date, 
                              temp=var_slice[tv],
                              lwr80=NA,
                              upr80=NA,
                              lwr95=NA,
                              upr95=NA,
                              Type="Observed")
        
        var_plot <- dplyr::bind_rows(var_obs, var_fc)
        
        p0 <- ggplot(data = var_plot, aes(x=Date, y=.data[[tv]] ,colour=Type)) + 
          geom_line() +
          geom_ribbon(aes(ymin=lwr80, ymax=upr80), alpha=0.2, linetype=0, fill="blue") +
          geom_ribbon(aes(ymin=lwr95, ymax=upr95), alpha=0.1, linetype=0, fill="blue") + 
          scale_colour_manual(values = c("Observed"="black", "Forecast"="blue")) +
          ggtitle(paste0("Forecast ",tv," for next ",input$nVarInput," periods"))
        
        if (first_plot == TRUE) {
          p <- p0
          first_plot <- FALSE
        } else {
          p <- p / p0
        }
        
      }
      
      p
      
    })
    
  })
  
  # var results ----
  output$var_results <- renderDataTable({
    
    if (input$forecastVarButton == 0) return()
    
    isolate({
      
      # slice dates
      var_slice <- var_tbl %>% dplyr::filter(Date >= input$periodRangeVar[1] &
                                               Date <= input$periodRangeVar[2])
      
      
      # Parse inputs
      v <- "Cases"
      mode <- '"aicc"'
      
      if (input$radioCasesInput == "None") {v <- v} 
      else if (input$radioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$radioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$radioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$diffCasesInput > 0) {v <- paste0("diff",input$diffCasesInput,"_",v)}
      
      
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
      
      strr <- paste0('var_slice %>% model(',mode,' = VAR(vars(',v,'), ic=',mode,'))')
      var_mdl <- eval(parse(text = strr))
      
      # generate forecast plot
      fc <- var_mdl %>%
        forecast(h = input$nVarInput)
      
      if (v == "Cases") {
        data.frame(Cases = fc$.mean)
      } else {
        fc$.mean
      }
      
    })
    
  })
  
  # var report ----
  output$var_report <- renderTable({
    
    if (input$forecastVarButton == 0) return()
    
    isolate({
      
      # slice dates
      var_slice <- var_tbl %>% dplyr::filter(Date >= input$periodRangeVar[1] &
                                               Date <= input$periodRangeVar[2])
      
      
      # Parse inputs
      v <- "Cases"
      mode <- '"aicc"'
      
      if (input$radioCasesInput == "None") {v <- v} 
      else if (input$radioCasesInput == "Log") {v <- paste0("log_",v)} 
      else if (input$radioCasesInput == "MinMax") {v <- paste0("mm_",v)} 
      else if (input$radioCasesInput == "Z") {v <- paste0("z_",v)}
      if (input$diffCasesInput > 0) {v <- paste0("diff",input$diffCasesInput,"_",v)}
      
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
      
      strr <- paste0('var_slice %>% model(',mode,' = VAR(vars(',v,'), ic=',mode,'))')
      var_mdl <- eval(parse(text = strr))
      
      
      broom::tidy(var_mdl)
      
      
    })
    
  },
  hover = TRUE,
  bordered = TRUE,
  striped = TRUE,
  width = "100%",
  align = "c",
  caption = "<h4>Model variable estimates and error</h4>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  
}
