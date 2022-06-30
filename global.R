#
# Global initialization code. This code is intended to read a configuration 
# file (YAML) and to create facade instances for accessing the data
#

# Load packages
require(yaml)

# Read configuration file
config <- yaml::yaml.load_file("configuration.yml")

# Load R classes for handling database transactions
source("R/DataSource.R", echo = FALSE)
source("R/Facade.R", echo = FALSE)
source("R/ObservationFacade.R", echo = FALSE)
source("R/StationFacade.R", echo = FALSE)
source("R/VariableFacade.R", echo = FALSE)

# Create DataSource instance
dataSource <- DataSource$new(parameters = config$data_source$parameters)

# Create facades
variable_facade <- VariableFacade$new(dataSource)
variables <- variable_facade$findAll()
station_facade <- StationFacade$new(dataSource)
stations <- station_facade$findAll()
observation_facade <- ObservationFacade$new(dataSource)
observations <- observation_facade$find(station_id = 87544, from = as.Date("2019-01-01"), to = as.Date("2019-01-31"))
