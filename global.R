#
# Global initialization code. This code is intended to read a configuration 
# file (YAML) and to create facade instances for accessing the data
#

# Load packages
require(dplyr)
require(highcharter)
require(shiny)
require(shinyalert)
require(shinythemes)
require(stlplus)
require(yaml)

# Read configuration file
config <- yaml::yaml.load_file("configuration.yml")

# Load R classes for handling database transactions
source("R/DataSource.R", echo = FALSE)
source("R/Facade.R", echo = FALSE)
source("R/ObservationFacade.R", echo = FALSE)
source("R/StationFacade.R", echo = FALSE)
source("R/VariableFacade.R", echo = FALSE)

# Load Shiny modules
source("R/modules/static-drop-down.r", echo = FALSE)

# Create DataSource instance
dataSource <- DataSource$new(parameters = config$data_source$parameters)

# Create facades
variable_facade <- VariableFacade$new(dataSource)
station_facade <- StationFacade$new(dataSource)
observation_facade <- ObservationFacade$new(dataSource)

# Select stations and variables
stations <- station_facade$findAll() %>%
  dplyr::arrange(name)
variables <- variable_facade$findAll() %>%
  dplyr::arrange(name)