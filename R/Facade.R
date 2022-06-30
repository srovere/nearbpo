require(R6)
require(dplyr)
require(dbplyr)

Facade <- R6Class("Facade",
	private = list(
	  dataSource = NULL
	)
)
