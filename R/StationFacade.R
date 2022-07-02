require(R6)
require(dplyr)

StationFacade <- R6Class("StationFacade",
	inherit = Facade,
	public = list(
	  initialize = function(dataSource) {
	    private$dataSource <- dataSource
	  },
	  
	  findAll = function() {
	    stations <- super$find(table = "stations")
	    return(stations)
	  }
	)
)