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
      # Station selector
      dropDownUI("stationId"),
      
      # Variable selector
      dropDownUI("variableId"),
      
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