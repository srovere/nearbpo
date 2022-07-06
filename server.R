shiny::shinyServer(function(input, output, session) {
  # Station selector
  dropDownServer(id = "stationId", label = "Station", custom_icon = "globe",
                 option_ids = dplyr::pull(stations, station_id),
                 option_names = dplyr::pull(stations, name))
  
  # Variable selector
  dropDownServer(id = "variableId", label = "Variable", custom_icon = "tasks",
                 option_ids = dplyr::pull(variables, variable_id),
                 option_names = dplyr::pull(variables, name))
  
  # Reactive for finding stations
  stationsReactive <- shiny::reactiveValues(stations = NULL, version = 0)
  findStations <- shiny::reactive({
    if (is.null(stationsReactive$stations) || stationsReactive$version) {
      warning("Updating stations")
      stationsReactive$stations <- station_facade$findAll() %>%
        dplyr::arrange(name)
    }
    return(stationsReactive$stations)
  })
  
  # Reactive for finding observations given a station and variable
  findObservations <- shiny::reactive({
    if (! is.null(input$stationId) && ! is.null(input$variableId)) {
      observations <- observation_facade$find(station_id = input$stationId, variable_id = input$variableId)
      return(observations)
    }
    return(NULL)
  })
  
  # Reactive for finding anomalies
  findAnomalies <- shiny::reactive({
    observations <- findObservations()
    if (! is.null(observations) && (nrow(observations) > 0) && ! is.null(input$significanceLevel)) {
      # Impute missing values
      timeSeries <- observations %>%
        dplyr::select(observation_date, observed_value) %>%
        zoo::read.zoo(index.column = "observation_date") %>%
        zoo::na.approx()
      
      # Anomaly detection
      timeSeriesTibble <- data.frame(date = zoo::index(timeSeries),
                                     value = zoo::coredata(timeSeries)) %>%
        tibble::as_tibble()
      
      tryCatch({
        anomaliesDetected <- timeSeriesTibble %>%
          anomalize::time_decompose(value, method = "stl", frequency = 365, message = FALSE) %>%
          anomalize::anomalize(remainder, method = "gesd", alpha = input$significanceLevel) %>%
          dplyr::filter(anomaly == "Yes") %>%
          dplyr::mutate(title = paste0("!! ", observed, " ºC"), text = "Possible anomaly")
        return(anomaliesDetected)
      }, error = function(e) {
        warning(e$message)  
      })
    }
    return(NULL)
  })
  
  # Reactive for proforming STL decomposition based on time-series
  findSTLDecomposition <- shiny::reactive({
    observations <- findObservations()
    if (! is.null(observations) && (nrow(observations) > 0)) {
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
      anomalies    <- findAnomalies()
      if (! is.null(observations) && ! is.null(anomalies)) {
        # Transform observations into zoo object
        timeSeries <- observations %>%
          dplyr::select(observation_date, observed_value) %>%
          zoo::read.zoo(index.column = "observation_date") %>%
          zoo::na.approx()
        
        # Find station and variable information
        station <- stations %>%
          dplyr::filter(station_id == input$stationId)
        variable <- variables %>%
          dplyr::filter(variable_id == input$variableId)
        
        # Plot anomalies
        highcharter::highchart(type = "stock") %>%
          highcharter::hc_add_series(data = xts::as.xts(timeSeries), id = "time_series", name = variable$name) %>%
          highcharter::hc_add_series(data = anomalies, mapping = highcharter::hcaes(x = date),
                                     name = "Anomalies", type = "flags", onSeries = "time_series") %>%
          highcharter::hc_xAxis(title = list(text = "Date")) %>%
          highcharter::hc_yAxis(title = list(text = sprintf("%s (%s)", variable$name, variable$units))) %>%
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
  
  # Set "Anomaly" status
  output$uiSetAnomalyStatus <- shiny::renderUI({
    anomalies <- findAnomalies()
    if (! is.null(anomalies)) {
      shiny::actionButton(inputId = "setAnomalyStatus", label = "Mark as anomalies", class = "blue-button", 
                          icon = shiny::icon(name = "exclamation-sign", lib = "glyphicon"))
    }
  })
  observeEvent(input$setAnomalyStatus, {
    anomalies <- findAnomalies()
    if (! is.null(anomalies)) {
      withProgress({
        # Find station and variable information
        station_id <- stations %>%
          dplyr::filter(station_id == input$stationId) %>%
          dplyr::pull(station_id)
        variable_id <- variables %>%
          dplyr::filter(variable_id == input$variableId) %>%
          dplyr::pull(variable_id)
        
        # Select dates
        dates <- dplyr::pull(anomalies, date)
        
        # Mark dates as "anomalies"
        tryCatch({
          observation_facade$set_anomalies(station_id, variable_id, dates)
          stationsReactive$version <- stationsReactive$version + 1
          shinyalert::shinyalert(title = "Anomaly detection",
                                 text = "Values have been successfully marked as anomalies",
                                 type = "success", confirmButtonCol = "#079d49")  
        }, error = function(e) {
          warning(paste0(e$call, ": ", e$message))
          shinyalert::shinyalert(title = "Anomaly detection",
                                 text = "Error while trying to mark values as anomalies",
                                 type = "error", confirmButtonCol = "#079d49")
        })
      }, message = "Marking values as anomalies. Please, wait...", value = NULL)
    }
  })
  
  # Map of stations with anomalies
  selectedStation <- shiny::reactiveValues(station_id = NULL, anomalies_data = NULL, version = 0)
  output$stationsMap <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      leaflet::addTiles(map = ., urlTemplate = config$basemap$url,
                        attribution = config$basemap$attribution) %>%
      leaflet::setView(map = ., zoom = config$basemap$zoom,
                       lng = config$basemap$center$longitude,
                       lat = config$basemap$center$latitude)
  })
  observe({
    # Find stations and current status
    stations_status <- findStations()
    if (! is.null(stations_status) && ! is.null(stationsReactive$version)) {
      # Set selected station = NULL
      selectedStation$station_id <- NULL
      
      withProgress({
        # Define colors
        color_palette <- function(status) {
          purrr::map(.x = status, .f = ~config$basemap$colors[[.x]])
        } 
        
        # Define bounding box
        bounding_box <- stations %>%
          sf::st_as_sf(x = ., coords = c("longitude", "latitude"), crs = 4326) %>%
          sf::st_bbox()
        
        # Legend data
        legend <- purrr::map_dfr(
          .x = names(config$basemap$colors),
          .f = function(status) {
            tibble::tibble(label = config$basemap$legend[[status]], color = config$basemap$colors[[status]])
          }
        ) %>% tibble::deframe()
        
        # Update map
        leaflet::leafletProxy(mapId = "stationsMap", data = stations_status) %>%
          # Clear all markers
          leaflet::clearControls() %>%
          leaflet::clearMarkers(map = .) %>%
          # Add circle markers
          leaflet::addCircleMarkers(map = ., lng = ~longitude, lat = ~latitude, radius = 10, layerId = ~as.character(station_id),
                                    options = list(
                                      weight = 1,
                                      fillOpacity = 0.8,
                                      fill = TRUE,
                                      color = "#1f1f1f",
                                      fillColor = ~color_palette(status),
                                      popup = ~paste0('<b>', name, "</b> (", station_id, ")<br>Lat./Lon.: ",
                                                      round(latitude, 2), ", ", round(longitude, 2),
                                                      '<br>Elevation: ', elevation, ' m')
                                    )) %>%
          # Add legend
          leaflet::addLegend(map = ., position = "bottomright", labels = names(legend), 
                             colors = unname(unlist((legend))), title = "Reference", opacity = 1)
      }, message = "Updating stations map. Please, wait...", value = NULL)
    }
  })
  observe({
    event <- input$stationsMap_marker_click
    if (! is.null(event)) {
      # Set selected station
      selectedStation$station_id <- as.integer(event$id)
    }
  })
  output$uiAnomalyTableHeader <- shiny::renderText({
    if (! is.null(input$variableId) && ! is.null(selectedStation$station_id) && ! is.null(selectedStation$version)) {
      # Find station and variable
      station  <- stations %>%
        dplyr::filter(station_id == selectedStation$station_id)
      variable <- variables %>%
        dplyr::filter(variable_id == input$variableId)
      
      # Find anomalies
      anomalies <- selectedStation$anomalies_data
      if (! is.null(anomalies) && (nrow(anomalies) > 0)) {
        paste0(
          tags$h2(sprintf("%s (%s)", station$name, station$station_id))
        ) 
      } else {
        paste0(
          tags$h2(sprintf("%s (%s)", station$name, station$station_id)),
          tags$h4(sprintf("There are no %s anomalies reported for this station", tolower(variable$name)))
        ) 
      }
    }
  })
  output$anomaliesTable <- DT::renderDataTable({
    if (! is.null(input$variableId) && ! is.null(selectedStation$station_id) && ! is.null(selectedStation$version)) {
      # Find station and variable data
      station  <- stations %>%
        dplyr::filter(station_id == selectedStation$station_id)
      variable <- variables %>%
        dplyr::filter(variable_id == input$variableId)
      
      # Find anomalies for selected station
      anomalies <- observation_facade$get_anomalies(station$station_id, variable$variable_id)
        
      if (nrow(anomalies) > 0) {
        # Save anomalies data
        selectedStation$anomalies_data <- anomalies
        
        # Build datatable object
        anomaliesTable <- DT::datatable(data = dplyr::select(anomalies, observation_date, observed_value),
                                        rownames = FALSE, colnames = c("Observation date", "Observed value"), 
                                        editable = list(target = "cell", disable = list(columns = c(0))),
                                        selection = "none",
                                        options = list(
                                          dom = 't',
                                          columns = list(list(type = "date"), list(type = "num")), 
                                          paging = FALSE, bSort = TRUE, bInfo = FALSE, pageLength = 20, lengthChange = FALSE
                                        )) %>%
          DT::formatRound(columns = c(2), digits = 2, dec.mark = ".", mark = "")
        return (anomaliesTable)
      } else {
        # Set anomalies to NULL
        selectedStation$anomalies_data <- NULL  
      }
    }
  })
  observeEvent(input$anomaliesTable_cell_edit, {
    if (! is.null(input$variableId) && ! is.null(selectedStation$station_id) &&
        ! is.null(selectedStation$anomalies_data) && ! is.null(selectedStation$version)) {
      withProgress({
        # Find observation date and new value
        editedCell       <- input$anomaliesTable_cell_edit
        anomalies        <- selectedStation$anomalies_data
        observation_date <- anomalies[editedCell$row, ]$observation_date
        observed_value   <- editedCell$value
        
        # Change value and mark as "Not-an-anomaly"
        new_anomalies <- observation_facade$set_observed_value(station_id = selectedStation$station_id, variable_id = input$variableId,
                                                               observation_date = observation_date, observed_value = observed_value)
        
        # Update reactives
        selectedStation$version <- selectedStation$version + 1
        if (nrow(new_anomalies) == 0) {
          # Reload map
          stationsReactive$version <- stationsReactive$version + 1
        }
      }, message = "Updating observation status. Please, wait...", value = NULL)
    }
  })
  output$uiMarkNotAnAnomaly <- shiny::renderUI({
    if (! is.null(input$variableId) && ! is.null(selectedStation$station_id) && ! is.null(selectedStation$version)) {
      # Find anomalies
      anomalies <- observation_facade$get_anomalies(selectedStation$station_id, input$variableId)
      if (nrow(anomalies) > 0) {
        shiny::actionButton(inputId = "clearAnomalies", label = "Mark anomalies as valid values", class = "blue-button", 
                            icon = shiny::icon(name = "check", lib = "glyphicon"))
      }
    }
  })
  observeEvent(input$clearAnomalies, {
    withProgress({
      # Find anomalies
      anomalies <- observation_facade$get_anomalies(selectedStation$station_id, input$variableId)
      if (! is.null(anomalies) && (nrow(anomalies) > 0)) {
          # Clear anomalies
          tryCatch({
            observation_facade$set_anomalies(selectedStation$station_id, input$variableId, c())
            selectedStation$anomalies_data <- NULL
            stationsReactive$version <- stationsReactive$version + 1
            shinyalert::shinyalert(title = "Anomaly detection",
                                   text = "All remaning dates have been marked as valid",
                                   type = "success", confirmButtonCol = "#079d49")  
          }, error = function(e) {
            shinyalert::shinyalert(title = "Anomaly detection",
                                   text = "Error while trying to mark remaining dates as valid",
                                   type = "error", confirmButtonCol = "#079d49")
          })
        }
      }, message = "Marking remaining dates as valid. Please, wait...", value = NULL)
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
          highcharter::hc_yAxis(title = list(text = sprintf("%s (%s)", variable$name, variable$units))) %>%
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