#' Build a path from character elements
#' 
#' This function takes a list if strings and builds a URL to the Kolada web API \emph{in reverser order}.
#' 
#' @param varname The name of the variable in the web API. This can be a data node or a tree node.
#' @param topnodes A string or a list of strings containing the top nodes \emph{in top-to-bottom order}
#' @param baseUrl The base URL to use. This is only useful if you want to use the function for constructing a URL to another web service or if Kolada should suddenly change their base URL. If you want to pass arguments to \code{baseURL()}, use the \code{...} argument instead.
#' @param ... Further arguments passed to  \code{baseURL()}.

##### SHOULD THIS BE REMOVED? #####

buildPath <- function(varname, topnodes = NULL, baseUrl = NULL, ...) {
	if (is.null(baseUrl))
		baseUrl <- baseURL(...)
	
	# Error handling
	if (!is.null(topnodes))
		if (topnodes == "")
			stop(str_join(
				str_wrap("ERROR: Internal function rKolada:::buildPath: `topnodes` argument set to empty string"),
				str_wrap("The `topnodes` argument is required to be either NULL or a value or a vector other than [''] interpretable as a character string by paste().\n"),
				sep = "\n"
			))
	
	# Clean URL string: remove trailing slash
	baseUrl <- str_replace_all(baseUrl,"/$","")
	
	# Clean topnodes string: Remove whitespace and leading/trailing slashes
	topnodes <- str_trim(topnodes)
	topnodes <- str_replace_all(topnodes,"^/|/$","")
	
	# Build a vector, in the right order, for the URL elements
	urlElements <- c(baseUrl,topnodes,varname)
	
	# Create full path and return it
	return(
		paste0(urlElements, collapse="/")
	)
}

#' Get content from response
#' 
#' Get the content from a response object
#' 
#' @param response response object
#' @param type type format
#' 
getContent <- function(response, type = "csv") {
    
    if (!class(response) == "response") {
        stop("needs to be an response class object")
    }
    
    # Convert to character
    content <- httr::content(response)
    
    if (type == "csv") {
        content <- read.table(
            textConnection(content), 
            sep = ",", 
            header = T, 
            stringsAsFactors = F
        )
    } else {
        stop("Unsupported type format")
    }
    
    return(content)
}

#' Return base URL to Kolada API
#' 
#' ...
#' 
#' @param version The version of Kolada API to use. (Default: \code{v1})
#' @param lang The language (two letters) to use in the fetched data. (Default: \code{sv})
#' @param ... Additional parameters. These are currently ignored.
#' @export
baseURL <- function(version="v1",...) {
	paste(sprintf("http://api.kolada.se/%s/",version))
}
