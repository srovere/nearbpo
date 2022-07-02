require(R6)
require(dplyr)
require(dbplyr)

Facade <- R6Class("Facade",
	private = list(
	  dataSource = NULL,
	  filter = function(rows, filters) {
	    if (! is.null(filters) && (length(filters) > 0)) {
  	    for (col.name in names(filters)) {
  	      if (! is.null(filters[[col.name]]) && (length(filters[[col.name]]) > 0)) {
  	        field.name <- rlang::sym(col.name)
  	        if (length(filters[[col.name]]) > 1) {
  	          field.values <- filters[[col.name]]
  	          rows         <- rows %>% dplyr::filter(UQ(field.name) %in% field.values)
  	        } else {
  	          field.value <- filters[[col.name]]
  	          if (! is.na(field.value)) {
  	            rows  <- rows %>% dplyr::filter(UQ(field.name) == field.value)
  	          } else {
  	            rows  <- rows %>% dplyr::filter(is.na(UQ(field.name)))
  	          }
  	        }
  	      }
  	    }
	    }
	    return (rows)
	  }
	),
	public = list(
	  find = function(table, eq_filters = list(), non_eq_filters = list(), post_query_filters = list()) {
	    # Initialize filters
	    filters <- purrr::map(
	      .x = names(eq_filters),
	      .f = function(column_name) {
	        value <- eq_filters[[column_name]]
	        if (! is.null(value)) {
	          return(list(columnName = column_name, operator = "eq", value = as.character(value)))
	        }
	        return(NULL)
	      }
	    ) %>% purrr::discard(.x = ., .p = is.null)
	    
	    # Add non-eq filters if necessary
	    if (! is.null(non_eq_filters) && (length(non_eq_filters) > 0)) {
	      filters <- append(
	        filters,
	        purrr::map(
	          .x = seq_along(non_eq_filters),
	          .f = function(seq_index) {
	            non_eq_filter <- non_eq_filters[[seq_index]]
	            column_name   <- non_eq_filter[["column_name"]]
	            operator      <- non_eq_filter[["operator"]]
	            value         <- as.character(non_eq_filter[["value"]])
	            return(list(columnName = column_name, operator = operator, value = value))
	          }
	        )
	      )
	    }
	    
	    # Execute query
	    private$dataSource$select(table = table, query = filters) %>%
	      private$filter(rows = ., filters = post_query_filters)
	  },
	  
	  update = function(table, pk_rows, new_data_rows) {
	    # Convert compund primary keys (k1, k2, k3) to k1/k2/k3
	    primary_keys <- purrr::pmap(
	      .l = dplyr::mutate_all(pk_rows, .funs = as.character),
	      .f = function(...) {
	        pk_cols <- list(...)
	        return(paste0(pk_cols, collapse = "/"))
	      }
	    )
	   
	    # Convert data rows to list of lists
	    new_data <- purrr::pmap(
	      .l = dplyr::mutate_all(new_data_rows, .funs = as.character),
	      .f = function(...) {
	        data_cols <- list(...)
	        return(data_cols)
	      }
	    )
	    
	    # Perform update
	    private$dataSource$update(table = table, primary_keys = primary_keys, new_data = new_data)
	  }
	)
)
