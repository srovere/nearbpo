require(R6)
require(jsonlite)
require(httr)
require(tibble)

DataSource <- R6Class("DataSource",
  private = list(
    parameters = NULL,
    get_base_url = function() {
      sprintf("https://%s-%s.apps.astra.datastax.com/api/rest/v1/keyspaces/%s/tables",
              private$parameters$db$id, private$parameters$db$region, private$parameters$db$keyspace)
    }
  ),
	public = list(
	  initialize = function(parameters) {
	    # Validate and set parameters
	    stopifnot(! is.null(parameters$db$id))
	    stopifnot(! is.null(parameters$db$region))
	    stopifnot(! is.null(parameters$db$keyspace))
	    stopifnot(! is.null(parameters$application$token))
	    private$parameters <- parameters
	  },
	  
	  columnNames = function(table) {
	    # Define headers
	    headers <- c("x-cassandra-token" = private$parameters$application$token)
	    
	    # Define URL
	    url <- url <- sprintf("%s/%s/columns", private$get_base_url(), table)
	    
	    # Make request
	    request <- httr::GET(url = url, config = httr::add_headers(headers))
	    
	    # Resurn columns
	    if (request$status_code == 200) {
	      response <- httr::content(request, as = "parsed")
	      return(purrr::map(.x = response, .f = ~ .x$name) %>% unlist())
	    } else {
	      stop(sprintf("Error reading response from %s: %d", url, request$status_code))
	    }
	    return(NULL)
	  },
	  
	  select = function(table, query, pageState = NULL, pageSize = 10000, columnNames = NULL) {
	    # Find column names if NULL
	    if (is.null(columnNames)) {
	      columnNames <- dataSource$columnNames(table)
	    }
	    
	    # Define headers
	    headers <- c("x-cassandra-token" = private$parameters$application$token)
	    
	    # Define method and URL data according to query
	    if (length(query) > 0) {
	      # Define POST URL
	      url <- sprintf("%s/%s/rows/query", private$get_base_url(), table)
	      
	      # Define body
	      body <- list(
	        pageSize = as.character(pageSize),
	        filters = purrr::map(
	          .x = query,
	          .f = function(condition) {
	            if (length(condition$value) > 1) {
	              condition$value <- as.character(condition$value)
	            } else {
	              condition$value <- list(as.character(condition$value))
	            }
	            return(condition)
	          }
	        )
	      ) 
	      if (! is.null(pageState)) {
	        body$pageState <- pageState
	      }
	      body <- jsonlite::toJSON(body, auto_unbox = TRUE)
	      
	      # Add Content-Type header
	      headers <- c(headers, "Content-Type" = "application/json")
	      
	      # Do request
	      request <- httr::POST(url = url, body = body, config = httr::add_headers(headers))
	    } else {
	      # Define GET URL
	      url <- sprintf("%s/%s/rows?pageSize=%d", private$get_base_url(), table, pageSize)
	      if (! is.null(pageState)) {
	        url <- sprintf("%s&pageState=%s", url, pageState)
	      }
	      
	      # Do request
	      request <- httr::GET(url = url, config = httr::add_headers(headers))
	    }
	    
	    # Fetch data
	    if (request$status_code == 200) {
	      response <- httr::content(request, as = "parsed")
	      if (response$count > 0) {
	        # Generate tibble from list of lists
	        rows <- purrr::map_dfr(
	          .x = response$rows, 
	          .f = ~ tibble::as_tibble(lapply(.x, FUN = function(x) { ifelse(! is.null(x), x, NA) }))
	        )
	        
	        # Order columns
	        rows <- rows[columnNames]
	        
	        # Find more data
	        if (! is.null(response$pageState)) {
	          rows <- dplyr::bind_rows(rows, self$select(table, query, response$pageState, pageSize, columnNames))
	        }
	        return(rows)
	      } else {
	        return(NULL)
	      }
	    } else {
	      response <- httr::content(request, as = "parsed")
	      stop(response)
	    }
	  }
	)
)
