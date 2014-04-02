#' Get data
#' 
#' Get data from the API. If at the lowest node, provide the user with a friendly message about this.
#' 
#' @param path URL to fetch metadata from. Defaults to the base URL of the Kolada web API, which is equal to the top node in the data tree.
#' @param quiet Quiet mode (never return a message to the user)
#' @param ... Further arguments to be passed to  \code{baseURL()}.
#' @export
#' 
kldMunicipality <- function(query, quiet=FALSE, baseUrl=NULL) {

	url <- buildPath(query, topnodes="municipality", baseUrl = NULL)
	
	response <- try(GET(
		url = url
	), silent = FALSE)
	
	responselist <- content(response)
	
	# Check if we the response is paginated
	if ("next" %in% names(responselist)) {
		more_pages <- TRUE
		while (more_pages) {
			new_query <- paste0(query, sprintf("?page=%s", responselist$count %/% 100))
			url <- buildPath(new_query, topnodes="municipality", baseUrl = NULL)
			
			response <- try(GET(
				url = url
			), silent = FALSE)
			
			tmp_responselist <- content(response)
			
			responselist$values <- append(responselist$values, tmp_responselist$values)
			responselist$count <- responselist$count + tmp_responselist$count
			
			# If we have reached the end of the pagination, break out of the loop.
			if (!"next" %in% names(tmp_responselist))
				more_pages <- FALSE
		}
		
		responselist$`next` <- NULL
	}
	
	#     df <- try(
	#       data.frame(
	#         t(sapply(
	#             RJSONIO::fromJSON(
	#                 paste(readLines(url, warn = F), collapse = ""),
	#                 encoding = "utf8"
	#             ),
	#             c
	#         ))
	#     ),silent=TRUE
	#       )
	
	# 	if(class(df)=="try-error"){
	# 	  stop(str_join("No internet connection to ",url),
	# 	       call.=FALSE)
	# 	}
	# 	
	kom

	return(df)
}
