Endpoint <- function(type, connection) {
	function(at) {
		stopifnot(is.Location(at))
		context <- init.context()
		endpoint <- init.socket(context, type)
		class(endpoint) <- c(switch(type,
					    ZMQ_REP="Replier",
					    ZMQ_REQ="Requester",
					    ZMQ_PUB="Publisher",
					    ZMQ_SUB="Subscriber"),
				     "Endpoint",
				     class(endpoint))
		connection(endpoint, as.character(at))
		endpoint
	}
}
is.Endpoint <- function(x) inherits(x, "Endpoint")
Replier <- Endpoint("ZMQ_REP", bind.socket)
is.Replier <- function(x) inherits(x, "Replier")
Requester <- Endpoint("ZMQ_REQ", connect.socket)
is.Requester <- function(x) inherits(x, "Requester")
Publisher <- Endpoint("ZMQ_PUB", bind.socket)
is.Publisher <- function(x) inherits(x, "Publisher")
Subscriber <- function(at) {
	socket <- Endpoint("ZMQ_SUB", connect.socket)(at)
	subscribe(socket, '')
	socket
}
is.Subscriber <- function(x) inherits(x, "Subscriber")

format.Endpoint <- function(x, ...) format(as.Location(x))
print.Replier <- function(x, ...)
	cat("Replier with last binding at location: ", format(x), "\n")
print.Requester <- function(x, ...)
	cat("Requester with last connection at location: ", format(x), "\n")
print.Publisher <- function(x, ...)
	cat("Publisher with last binding at location: ", format(x), "\n")
print.Subscriber <- function(x, ...)
	cat("Subscriber with last connection at location: ", format(x), "\n")
