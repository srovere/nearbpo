require(R6)
require(dplyr)

ObservationFacade <- R6Class("ObservationFacade",
	inherit = Facade,
	public = list(
	  initialize = function(dataSource) {
	    private$dataSource <- dataSource
	  },
	  
	  find = function(station_id, variable_id = NULL, from = NULL, to = NULL) {
	    # Define query filters
	    filters <- list(
	      list(columnName = "station_id", operator = "eq", value = as.character(station_id))
	    )
	    if (! is.null(variable_id)) {
	      filters <- append(filters, list(list(columnName = "variable_id", operator = "te", value = as.character(variable_id))))
	    }
	    if (! is.null(from)) {
	      filters <- append(filters, list(list(columnName = "observation_date", operator = "gte", value = as.character(from))))
	    }
	    if (! is.null(to)) {
	      filters <- append(filters, list(list(columnName = "observation_date", operator = "lte", value = as.character(to))))
	    }
	    
	    # Execute query
	    observations <- private$dataSource$select(
	      table = "observations", query = filters
	    ) %>% dplyr::mutate(observation_date = as.Date(observation_date))
	    return(observations)
	  }
	)
)