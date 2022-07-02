require(R6)
require(dplyr)

VariableFacade <- R6Class("VariableFacade",
	inherit = Facade,
	public = list(
	  initialize = function(dataSource) {
	    private$dataSource <- dataSource
	  },
	  
		findAll = function() {
		  variables <- super$find(table = "variables")
		  return(variables)
		}
	)
)