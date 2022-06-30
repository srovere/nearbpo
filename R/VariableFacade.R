require(R6)
require(dplyr)

VariableFacade <- R6Class("VariableFacade",
	inherit = Facade,
	public = list(
	  initialize = function(dataSource) {
	    private$dataSource <- dataSource
	  },
	  
		findAll = function() {
		  variables <- private$dataSource$select(
		    table = "variables", query = list()
		  )
		  return(variables)
		}
	)
)