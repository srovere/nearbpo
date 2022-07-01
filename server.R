shiny::shinyServer(function(input, output, session) {
  # Station selector
  dropDownServer(id = "stationId", label = "Station", custom_icon = "globe",
                 option_ids = dplyr::pull(stations, station_id),
                 option_names = dplyr::pull(stations, name))
  
  # Variable selector
  dropDownServer(id = "variableId", label = "Variable", custom_icon = "tasks",
                 option_ids = dplyr::pull(variables, variable_id),
                 option_names = dplyr::pull(variables, name))
  
  # Reactive for finding observations given a station and variable
  findObservations <- shiny::reactive({
    if (! is.null(input$stationId) && ! is.null(input$variableId)) {
      observations <- observation_facade$find(station_id = input$stationId, variable_id = input$variable_id)
      return(observations)
    }
    return(NULL)
  })
  
  # Reactive for proforming STL decomposition based on time-series
  findSTLDecomposition <- shiny::reactive({
    observations <- findObservations()
    if (! is.null(observations)) {
      # Sealizar decomposicion
      stl_object <- stlplus::stlplus(x = dplyr::pull(observations, observed_value),
                                     t = dplyr::pull(observations, observation_date),
                                     n.p = 365, s.window = 'periodic')
      
      # Return original series + STL decomposition
      stl_decomposition <- dplyr::bind_cols(
        observations,
        dplyr::select(stl_object$data, seasonal, trend, remainder)
      )
      return (stl_decomposition)
    }
    return(NULL)
  })
  
  # Plot anomalies
  output$anomaliesPlot <- highcharter::renderHighchart({
    if (input$menu == "anomalyDetection") {
      observations <- findObservations()
      if (! is.null(observations) && ! is.null(input$significanceLevel)) {
        # Transform tibble into zoo object
        timeSeries <- observations %>%
          dplyr::select(observation_date, observed_value) %>%
          zoo::read.zoo(index.column = "observation_date") %>%
          zoo::na.approx()
        
        # Find station and variable information
        station <- stations %>%
          dplyr::filter(station_id == input$stationId)
        variable <- variables %>%
          dplyr::filter(variable_id == input$variableId)
         
        # Anomaly detection
        timeSeriesTibble <- data.frame(date = zoo::index(timeSeries),
                                       value = zoo::coredata(timeSeries)) %>%
          tibble::as_tibble()
        anomaliesDetected <- timeSeriesTibble %>%
          anomalize::time_decompose(value, method = "stl", frequency = 365) %>%
          anomalize::anomalize(remainder, method = "gesd", alpha = input$significanceLevel) %>%
          dplyr::filter(anomaly == "Yes") %>%
          dplyr::mutate(title = paste0("!! ", observed, " ºC"), text = "Possible anomaly")
        
        # Plot anomalies
        highcharter::highchart(type = "stock") %>%
          highcharter::hc_add_series(data = xts::as.xts(timeSeries), id = "time_series", name = variable$name) %>%
          highcharter::hc_add_series(data = anomaliesDetected, mapping = highcharter::hcaes(x = date),
                                     name = "Anomalies", type = "flags", onSeries = "time_series") %>%
          highcharter::hc_xAxis(title = list(text = "Date")) %>%
          highcharter::hc_yAxis(title = list(text = sprintf("%s (%s)", variable$name, variable$unit))) %>%
          highcharter::hc_chart(type = 'line', zoomType = 'x', panning = TRUE, panKey = 'shift') %>%
          highcharter::hc_legend(enabled = TRUE, layout = "horizontal") %>%
          highcharter::hc_tooltip(shared = TRUE, valueDecimals = 2) %>%
          highcharter::hc_colors(c('#1b9e77','#e41a1c')) %>%
          highcharter::hc_title(text = sprintf("Anomalies detection (%s)", variable$name)) %>%
          highcharter::hc_subtitle(text = sprintf("%s (%d)", station$name, station$station_id)) %>%
          highcharter::hc_exporting(enabled = TRUE) %>%
          highcharter::hc_add_theme(highcharter::hc_theme_flat())
      }
    }
  })
  
  # Plot STL decomposed time series
  output$trendAnalysis <- highcharter::renderHighchart({
    if (input$menu == "trendAnalysis") {
      stl_decomposition <- findSTLDecomposition()
      if (! is.null(stl_decomposition)) {
        # Find station and variable information
        station <- stations %>%
          dplyr::filter(station_id == input$stationId)
        variable <- variables %>%
          dplyr::filter(variable_id == input$variableId)
        
        highcharter::highchart(type = "stock") %>%
          highcharter::hc_add_series(data = xts::xts(stl_decomposition$trend, order.by = stl_decomposition$observation_date),
                                     id = "trend", name = "Trend") %>%
          highcharter::hc_add_series(data = xts::xts(stl_decomposition$seasonal, order.by = stl_decomposition$observation_date),
                                     id = "seasonal", name = "Seasonality") %>%
          highcharter::hc_add_series(data = xts::xts(stl_decomposition$remainder, order.by = stl_decomposition$observation_date),
                                     id = "remainder", name = "Remainder") %>%
          highcharter::hc_add_series(data = xts::xts(stl_decomposition$observed_value, order.by = stl_decomposition$observation_date),
                                     id = "complete", name = "Original time-series") %>%
          highcharter::hc_xAxis(title = list(text = "Date")) %>%
          highcharter::hc_yAxis(title = list(text = sprintf("%s (%s)", variable$name, variable$unit))) %>%
          highcharter::hc_chart(type = 'line', zoomType = 'x', panning = TRUE, panKey = 'shift') %>%
          highcharter::hc_legend(enabled = TRUE, layout = "horizontal") %>%
          highcharter::hc_tooltip(shared = TRUE, valueDecimals = 2) %>%
          highcharter::hc_colors(c('#e6ab02','#1b9e77','#377eb8','#e41a1c')) %>%
          highcharter::hc_title(text = sprintf("Additive STL decomposition for %s", variable$name)) %>%
          highcharter::hc_subtitle(text = sprintf("%s (%d)", station$name, station$station_id)) %>%
          highcharter::hc_exporting(enabled = TRUE) %>%
          highcharter::hc_add_theme(highcharter::hc_theme_flat())
      }
    }
  })
})