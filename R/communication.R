Endpoint <- function(type, connection) {
	function(x, context, ...) {
		stopifnot(is.Location(x))
		initialised_context <-
			if (missing(context)) init.context() else context
		endpoint <- init.socket(initialised_context, type)
		class(endpoint) <- c(switch(type,
					    ZMQ_REP=c("Replier", "Listener"),
					    ZMQ_REQ=c("Requester", "Speaker"),
					    ZMQ_PUB=c("Publisher", "Speaker"),
					    ZMQ_SUB=c("Subscriber", "Listener")),
				     "Endpoint",
				     class(endpoint))
		connection(endpoint, as.character(x))
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

MultiListener <- function(x, ...) UseMethod("MultiListener")
MultiListener.Replier <- function(x, subscriber, ...) {
	stopifnot(is.subscriber(subscriber))
	multi_listener <- list(replier=x, subscriber=subscriber)
	class(multi_listener) <- c("MultiListener", class(multi_listener))
	multi_listener
}
MultiListener.ReplierLocation <- function(x, publisher_location, context, ...) {
	stopifnot(is.missing(publisher_location) || is.publisher_location)
	MultiListener(Replier(x, context), Subscriber(publisher_location, context))
}
MultiListener.Node <- function(x, ...) {
	MultiListener(Replier(x, ...), Subscriber(...))
}
is.MultiListener <- function(x) inherits(x, "MultiListener")
Replier.MultiListener  <- function(x, ...) x$replier
Subscriber.MultiListener  <- function(x, ...) x$subscriber
listen.MultiListener <- function(x, ...) {
	unlist(poll.socket(listeners, rep("read", length(listeners)), ...))
}

MultiSpeaker <- function(x, ...) UseMethod("MultiSpeaker")
MultiSpeaker.Requester <- function(x, publisher, ...) {
	stopifnot(is.Publisher(publisher))
	multi_speaker <- list(requester=x, publisher=publisher)
	class(multi_speaker) <- c("MultiSpeaker", class(multi_speaker))
	multi_speaker
}
MultiSpeaker.Node <- function(x, ...) {
	MultiSpeaker(Requester(...), Publisher(x, ...))
}
is.MultiSpeaker <- function(x) inherits(x, "MultiSpeaker")
Requester.MultiSpeaker <- function(x, ...) x$requester
Publisher.MultiSpeaker <- function(x, ...) x$publisher

Communicator <- function(x, ...) UseMethod("Communicator", ...)
Communicator.MultiListener <- function(x, multi_speaker, ...) {
	stopifnot(is.MultiSpeaker(multi_speaker))
	endpoints <- list(multi_listener=x, multi_speaker=multi_speaker)
	class(endpoints) <- c("Communicator", class(endpoints))
	endpoints
}
Communicator.Node <- function(x, ...) {
	context <- init.context()
	Communicator(MultiListener(x, context), MultiSpeaker(x, context))
}
is.Communicator <- function(x) inherits(x, "Communicator")
MultiListener.Communicator <- function(x, ...) x$multi_listener
MultiSpeaker.Communicator <- function(x, ...) x$multi_speaker
Publisher.Communicator <- function(x, ...) Publisher(MultiSpeaker(x))
Requester.Communicator <- function(x, ...) Requester(MultiSpeaker(x))
Replier.Communicator <- function(x, ...) Replier(MultiListener(x))
Subscriber.Communicator <- function(x, ...) Subscriber(MultiListener(x))
listen.Communicator <- function(x, ...) c(listen(MultiListener(x)), FALSE, FALSE)
`[`.Communicator <- function(x, i) c(MultiListener(x)[i], MultiSpeaker(x)[i])
