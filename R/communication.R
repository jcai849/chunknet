Endpoint <- function(type, connection) {
	function(x, context, ...) {
		stopifnot(is.Location(x))
		initialised_context <-
			if (missing(context)) init.context() else context
		endpoint <- init.socket(initialised_context, type)
		class(endpoint) <- c(switch(type,
					    ZMQ_REP=c("Replier", "Binder"),
					    ZMQ_REQ=c("Requester", "Connector"),
					    ZMQ_PUB=c("Publisher", "Binder"),
					    ZMQ_SUB=c("Subscriber", "Connector")),
				     "Endpoint",
				     class(endpoint))
		connection(endpoint, as.character(x))
		endpoint
	}
}
is.Endpoint <- function(x) inherits(x, "Endpoint")
format.Endpoint <- function(x, ...) format(as.Location(x))
print.Endpoint <- function(x, ...) cat("Endpoint: ", format(x), "\n")

is.Binder <- function(x) inherits(x, "Binder")
print.Binder <- function(x, ...) {
	cat("Binder with last binding at: ")
	NextMethod()
}
is.Connector <- function(x, ...) inherits(x, "Connector")
print.Connector <- function(x, ...) {
	cat("Connector with last connection at: ")
	NextMethod()
}
read <- function(x, ...) UseMethod("read")
read.connector <- function(x) {
	request <- receive.socket(x, ...)
	stopifnot(is.Request(request))
	request
}

maybe_connect <- function(endpoint, at)
	if (!missing(at)) connect.socket(endpoint, at)
Replier <- function(x, ...) UseMethod("Replier")
Requester <- function(x, ...) {
	if (missing(x)) Requester.Location() else UseMethod("Requester")
}
Publisher <- function(x, ...) UseMethod("Publisher")
Subscriber <- function(x, ...) {
	if (missing(x)) Subscriber.Location() else UseMethod("Subscriber")
}
Replier.Location <- Endpoint("ZMQ_REP", bind.socket)
Requester.Location <- Endpoint("ZMQ_REQ", maybe_connect)
Publisher.Location <- Endpoint("ZMQ_PUB", bind.socket)
Subscriber.Location <- Endpoint("ZMQ_SUB",
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

Listener <- function(x, ...) UseMethod("Listener")
Listener.Replier <- function(x, subscriber, ...) {
	stopifnot(is.subscriber(subscriber))
	listener <- list(replier=x, subscriber=subscriber)
	class(listener) <- c("Listener", class(listener))
	listener
}
Listener.ReplierLocation <- function(x, publisher_location, context, ...) {
	stopifnot(is.missing(publisher_location) || is.publisher_location)
	Listener(Replier(x, context), Subscriber(publisher_location, context))
}
Listener.Node <- function(x, ...) {
	Listener(Replier(x, ...), Subscriber(...))
}
is.Listener <- function(x) inherits(x, "Listener")
Replier.Listener  <- function(x, ...) x$replier
Subscriber.Listener  <- function(x, ...) x$subscriber
format.Listener <- function(x, ...)
	c(format(Replier(x)), format(Subscriber(x)))
print.Listener <- function(x, ...)
	cat("Listener:\n",
	    paste0(' ', capture.output(print(Replier(x)))),
	    paste0(' ', capture.output(print(Subscriber(x)))))
listen.Listener <- function(x, ...) {
	unlist(poll.socket(listeners, rep("read", length(listeners)), ...))
}

Speaker <- function(x, ...) UseMethod("Speaker")
Speaker.Requester <- function(x, publisher, ...) {
	stopifnot(is.Publisher(publisher))
	speaker <- list(requester=x, publisher=publisher)
	class(speaker) <- c("Speaker", class(speaker))
	speaker
}
Speaker.Node <- function(x, ...) {
	Speaker(Requester(...), Publisher(x, ...))
}
is.Speaker <- function(x) inherits(x, "Speaker")
Requester.Speaker <- function(x, ...) x$requester
Publisher.Speaker <- function(x, ...) x$publisher
format.Speaker <- function(x, ...)
	c(format(Requester(x)), format(Publisher(x)))
print.Speaker <- function(x, ...)
	cat("Speaker:\n",
	    paste0(' ', capture.output(print(Requester(x)))),
	    paste0(' ', capture.output(print(Publisher(x)))))

Communicator <- function(x, ...) UseMethod("Communicator", ...)
Communicator.Listener <- function(x, speaker, ...) {
	stopifnot(is.Speaker(speaker))
	endpoints <- list(listener=x, speaker=speaker)
	class(endpoints) <- c("Communicator", class(endpoints))
	endpoints
}
Communicator.Node <- function(x, ...) {
	context <- init.context()
	Communicator(Listener(x, context), Speaker(x, context))
}
is.Communicator <- function(x) inherits(x, "Communicator")
Listener.Communicator <- function(x, ...) x$listener
Speaker.Communicator <- function(x, ...) x$speaker
Publisher.Communicator <- function(x, ...) Publisher(Speaker(x))
Requester.Communicator <- function(x, ...) Requester(Speaker(x))
Replier.Communicator <- function(x, ...) Replier(Listener(x))
Subscriber.Communicator <- function(x, ...) Subscriber(Listener(x))
format.Communicator <- function(x, ...)
	cat("Communicator:\n",
	    paste0(' ', capture.output(print(Listener(x)))),
	    paste0(' ', capture.output(print(Speaker(x)))))
listen.Communicator <- function(x, ...) c(listen(Listener(x)), FALSE, FALSE)
`[`.Communicator <- function(x, i) c(Listener(x)[i], Speaker(x)[i])
