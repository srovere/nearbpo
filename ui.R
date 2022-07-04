# Define UI for application
shiny::shinyUI(
  shiny::navbarPage(
    theme = shinythemes::shinytheme("flatly"),
    title = "NearBPO Test App",
    collapsible = FALSE,
    id = "menu",
    tabPanel(title = "Anomalies detection", value = "anomalyDetection"),
    tabPanel(title = "Data correction", value = "dataCorrection"),
    tabPanel(title = "Trend analysis", value = "trendAnalysis"),
    
    # HTML <head>
    tags$head(
      shinyalert::useShinyalert(),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css?v=3")
    ),
    
    # Sidebar panel
    sidebarPanel(width = 3,
      # Variable selector
      dropDownUI("variableId"),
      
      # Station selector
      conditionalPanel(
        condition = "(input.menu == 'anomalyDetection') || (input.menu == 'trendAnalysis')",
        dropDownUI("stationId"),
      ),
      
      # Significance level for E-S-D test
      conditionalPanel(
        condition = "input.menu == 'anomalyDetection'",
        shiny::sliderInput(inputId = "significanceLevel",
                           label = shiny::div(icon("scale", lib = "glyphicon"), span(style = "padding-left: 5px;", "Significance level")),
                           min = config$anomaly_detection$significance_level$minimum, 
                           max = config$anomaly_detection$significance_level$maximum, 
                           step = config$anomaly_detection$significance_level$step,
                           value = config$anomaly_detection$significance_level$default),
        hr(),
        shiny::uiOutput(outputId = "uiSetAnomalyStatus")
      ),
      
      # Instructions for data correction + action button
      conditionalPanel(
        condition = "input.menu == 'dataCorrection'",
        tags$p("You may edit an observed value by double-clicking on it. After changing the value, click outside the input box to save."),
        hr(),
        shiny::uiOutput(outputId = "uiMarkNotAnAnomaly")
      )
    ),
    
    # Main panel
    mainPanel(width = 9, 
      # Anomalies detection
      conditionalPanel(
        condition = "input.menu == 'anomalyDetection'",
        shinycssloaders::withSpinner(
          highcharter::highchartOutput("anomaliesPlot", height = "700px"),
        type = 5, color = "#008d4c")  
      ),
      
      conditionalPanel(
        condition = "input.menu == 'dataCorrection'",
        fluidRow(
          column(6, shinycssloaders::withSpinner(
            leaflet::leafletOutput("stationsMap", height = "700px"),
          type = 5, color = "#008d4c")),
          column(6, 
            shiny::uiOutput(outputId = "uiAnomalyTableHeader"),
            br(),
            shinycssloaders::withSpinner(
              DT::dataTableOutput("anomaliesTable", height = "700px"),
            type = 5, color = "#008d4c")
          )
        )
      ),
      
      # Trend analysis
      conditionalPanel(
        condition = "input.menu == 'trendAnalysis'",
        shinycssloaders::withSpinner(
          highcharter::highchartOutput("trendAnalysis", height = "700px"),
        type = 5, color = "#008d4c")  
      )
    )
  )
)