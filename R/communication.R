Endpoint <- function(type, connection) {
	function(at) {
		stopifnot(is.Location(at))
		context <- init.context()
		endpoint <- init.socket(context, type)
		class(endpoint) <- c(type, "Endpoint", class(replier))
		connection(endpoint, as.character(at))
		endpoint
	}
}
is.Endpoint <- function(x) inherits(x, "Endpoint")
Replier <- Endpoint("ZMQ_REP", bind.socket)
is.Replier <- function(x) inherits(x, "ZMQ_REP")
Requester <- Endpoint("ZMQ_REQ", connect.socket)
is.Requester <- function(x) inherits(x, "ZMQ_REQ")
