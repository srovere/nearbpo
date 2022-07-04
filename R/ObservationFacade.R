require(R6)
require(dplyr)

ObservationFacade <- R6Class("ObservationFacade",
	inherit = Facade,
	public = list(
	  initialize = function(dataSource) {
	    private$dataSource <- dataSource
	  },
	  
	  find = function(station_id, variable_id, from = NULL, to = NULL, status = NULL) {
	    # Define "eq" filters
	    eq_filters <- list("station_id" = station_id, "variable_id" = variable_id)
	    
	    # Define "non-eq" filters
	    non_eq_filters <- list()
	    if (! is.null(from)) {
	      non_eq_filters <- append(non_eq_filters, list(list(column_name = "observation_date", operator = "gte", value = from)))
	    }
	    if (! is.null(to)) {
	      non_eq_filters <- append(non_eq_filters, list(list(column_name = "observation_date", operator = "lte", value = to)))
	    }
	    
	    # Define "post-query" filters
	    post_query_filters <- list("status" = status)
	    
	    # Execute query
	    observations <- super$find(
	      table = "observations", eq_filters = eq_filters, 
	      non_eq_filters = non_eq_filters,
	      post_query_filters = post_query_filters
	    ) %>% dplyr::mutate(observation_date = as.Date(observation_date))
	    return(observations)
	  },
	  
	  get_anomalies = function(station_id, variable_id) {
	    self$find(station_id = station_id, variable_id = variable_id, status = 'A')
	  },
	  
	  set_anomalies = function(station_id, variable_id, dates) {
	    # 1. Current must be marked as "Not-an-Anomaly" unless included in new list of anomalies
	    old_anomalies <- self$get_anomalies(station_id, variable_id) %>%
	      dplyr::select(station_id, variable_id, observation_date)
	    if (length(dates) > 0) {
	      not_anomalies <- old_anomalies %>%
	        dplyr::filter(! observation_date %in% dates)  
	    } else {
	      not_anomalies <- old_anomalies
	    }
	    if (nrow(not_anomalies) > 0) {
	      # Set as "Not-an-Anomaly"
	      count <- super$update(
	        table = "observations",
	        pk_rows = not_anomalies,
	        new_data_rows = tibble::tibble(status = rep('N', nrow(not_anomalies)))
	      )
	    }
	    
	    # 2. Find new anomalies and set status
	    new_anomalies_dates <- dates[which(! dates %in% dplyr::pull(old_anomalies, observation_date))]
	    if (length(new_anomalies_dates) > 0) {
	      # Set as "Anomaly"
	      new_anomalies <- tibble::tibble(station_id = station_id, variable_id = variable_id, observation_date = new_anomalies_dates)
	      count <- super$update(
	        table = "observations",
	        pk_rows = new_anomalies,
	        new_data_rows = tibble::tibble(status = rep('A', nrow(new_anomalies)))
	      )
	    }
	    
	    # 3. Set station status as "Quality Check" if anomalies > 0
	    new_status <- ifelse(length(dates) > 0, 'QC', 'V')
	    super$update(
	      table = "stations",
	      pk_rows = tibble::tibble(station_id = station_id),
	      new_data_rows = tibble::tibble(status = new_status)
	    )
	    return(NULL)
	  },
	  
	  set_observed_value = function(station_id, variable_id, observation_date, observed_value) {
	    # Set new value and mark observation as "Not-an-anomaly"
	    super$update(
	      table = "observations",
	      pk_rows = tibble::tibble(station_id = station_id, variable_id = variable_id, observation_date = observation_date),
	      new_data_rows = tibble::tibble(observed_value = observed_value, status = 'N')
	    )
	    
	    # Set station status as "V" if anomalies = 0
	    anomalies <- self$get_anomalies(station_id, variable_id)
	    if (nrow(anomalies) == 0) {
  	    super$update(
  	      table = "stations",
  	      pk_rows = tibble::tibble(station_id = station_id),
  	      new_data_rows = tibble::tibble(status = 'V')
  	    )
	    }
	    return(anomalies)
	  }
	)
)
