url <- buildPath(varname="", topnodes="municipality")

response <- try(GET(
	url = url
), silent = FALSE)

a <- content(response)
names(a)

if ("next" %in% names(a))