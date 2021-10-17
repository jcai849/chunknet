Endpoint <- function(type, connection) {
	function(at, context) {
		stopifnot(is.Location(at))
		initiated_context <- if (missing(context)) init.context() else context
		endpoint <- init.socket(initiated_context, type)
		class(endpoint) <- c(switch(type,
					    ZMQ_REP=c("Replier", "Listener"),
					    ZMQ_REQ=c("Requester", "Speaker"),
					    ZMQ_PUB=c("Publisher", "Speaker"),
					    ZMQ_SUB=c("Subscriber", "Listener")),
				     "Endpoint",
				     class(endpoint))
		connection(endpoint, as.character(at))
		endpoint
	}
}
is.Endpoint <- function(x) inherits(x, "Endpoint")
format.Endpoint <- function(x, ...) format(as.Location(x))
print.Endpoint <- function(x, ...) cat("Endpoint: ", format(x), "\n")

is.Listener <- function(x) inherits(x, "Listener")
print.Listener <- function(x, ...) {
	cat("Listener with last binding at: ")
	NextMethod()
}
is.Speaker <- function(x, ...) inherits(x, "Speaker")
print.Speaker <- function(x, ...) {
	cat("Speaker with last connection at: ")
	NextMethod()
}

maybe_connect <- function(endpoint, at)
	if (!missing(at)) connect.socket(endpoint, at)
Replier <- Endpoint("ZMQ_REP", bind.socket)
Requester <- Endpoint("ZMQ_REQ", maybe_connect)
Publisher <- Endpoint("ZMQ_PUB", bind.socket)
Subscriber <- Endpoint("ZMQ_SUB",
		       function(endpoint, at) {
			       maybe_connect(endpoint, at)
			       subscribe(endpoint, '')
		       })
is.Publisher <- function(x) inherits(x, "Publisher")
is.Requester <- function(x) inherits(x, "Requester")
is.Replier <- function(x) inherits(x, "Replier")
is.Subscriber <- function(x) inherits(x, "Subscriber")

print.Replier <- function(x, ...) { cat("Replier: "); NextMethod() }
print.Requester <- function(x, ...) { cat("Requester: "); NextMethod() }
print.Publisher <- function(x, ...) { cat("Publisher: "); NextMethod() }
print.Subscriber <- function(x, ...) { cat("Subscriber: "); NextMethod() }
