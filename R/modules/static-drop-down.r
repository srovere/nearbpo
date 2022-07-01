# Define the UI for dropdowns
dropDownUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("out"))
  )
}

# Define the server logic for dropdowns
dropDownServer <- function(id, label, custom_icon, option_ids, option_names) {
  moduleServer(
    id,
    function(input, output, session) {
      output$out <- shiny::renderUI({
        choices        <- option_ids
        names(choices) <- option_names
        
        shiny::selectInput(inputId = id, choices = choices, 
                           label = shiny::div(icon(custom_icon, lib = "glyphicon"), span(style = "padding-left: 5px;", label)))
      })
    }
  )
}