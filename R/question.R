REQUEST <- function(type) {
	function(...) structure(list(...),
				class=c(paste0(type, "_Request"), "Request"))
}
GET_request <- function(x, how) {
	stopifnot(is.Identifier(x), is.function(how))
	REQUEST("GET")(x, how)
}
POST_request <- function(x, id) {
	stopifnot(is.Location(x) || is.Computation(x),
		  is.Location(x) && (is.missing(id) || is.Identifier(id)))
	this_call <- match.call()
	eval(as.call(c(quote(REQUEST("POST")), as.list(this_call[-1]))))
}
PUT_request <- function(x) {
	stopifnot(is.Data(x))
	REQUEST("PUT")(x)
}
