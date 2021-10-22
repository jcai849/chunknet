Context <- function() {
	context <- init.context()
	structure(context, class=c("Context", class(context)))
}
is.Context <- function(x) inherits(x, "Context")

Endpoint <- function(x, ...) UseMethod("Endpoint")
Endpoint.Context <- function(x, type, ...) {
	stopifnot(is.character(type))
	endpoint <- init.socket(x, type)
	structure(endpoint, class=c("Endpoint", class(endpoint)))
}
Endpoint.character <- function(x, ...) {
	context <- Context()
	endpoint <- init.socket(context, x)
	structure(endpoint, class=c("Endpoint", class(endpoint)))
}
is.Endpoint <- function(x) inherits(x, "Endpoint")
format.Endpoint <- function(x, ...) format(as.Location(x))
print.Endpoint <- function(x, ...) cat("Endpoint: ", format(x), "\n")

Binder <- function(x, ...) UseMethod("Binder")
Binder.Location <- function(x, context, type, ...) {
	binder <- if (missing(context))
            Endpoint(type) else Endpoint(context, type=type, ...)
	bind.socket(binder, as.character(x))
	structure(binder, class=c("Binder", class(binder)))
}
is.Binder <- function(x) inherits(x, "Binder")
print.Binder <- function(x, ...) {
	cat("Binder with last binding at: ")
	NextMethod()
}
Connector <- function(x, ...) UseMethod("Connector")
Connector.Location <- function(x, context, type, ...) {
	connector <- if (missing(context))
        	Endpoint(type) else Endpoint(context, type=type, ...)
	connect.socket(connector, x)
	structure(connector, class=c("Connector", class(connector)))
}
Connector.Context <- function(x, type, ...) {
	connector <- Endpoint(x, type=type, ...)
	structure(connector, class=c("Connector", class(connector)))
}
Connector.character <- function(x, ...) {
	connector <- Endpoint(x, ...)
	structure(connector, class=c("Connector", class(connector)))
}
is.Connector <- function(x, ...) inherits(x, "Connector")
print.Connector <- function(x, ...) {
	cat("Connector with last connection at: ")
	NextMethod()
}
read <- function(x, ...) UseMethod("read")
read.Endpoint <- function(x, ...) {
	request <- receive.socket(x, ...)
	stopifnot(is.Request(request))
	request
}

Replier <- function(x, ...) UseMethod("Replier")
Requester <- function(x, ...) {
	if (missing(x)) Requester.Context() else UseMethod("Requester")
}
Publisher <- function(x, ...) UseMethod("Publisher")
Subscriber <- function(x, ...) {
	if (missing(x)) Subscriber.Context() else UseMethod("Subscriber")
}
Replier.Location <- function(x, context, ...) {
	replier <- Binder(x, context=context, type="ZMQ_REP", ...)
	structure(replier, class=c("Replier", class(replier)))
}
Replier.Node <- function(x, context, ...)
	Replier(ReplierLocation(x), context=context, ...)
Publisher.Location <- function(x, context, ...) {
	publisher <- Binder(x, context=context, type="ZMQ_PUB", ...)
	structure(publisher, class=c("Publisher", class(publisher)))
}
Publisher.Node <- function(x, context, ...)
	Publisher(PublisherLocation(x), context=context, ...)
Requester.Location <- function(x, context, ...) {
	requester <- Connector(x, context=context, type="ZMQ_REQ", ...)
	structure(requester, class=c("Requester", class(requester)))
}
Requester.Context <- function(x, ...) {
	requester <- Connector(x, type="ZMQ_REQ", ...)
	structure(requester, class=c("Requester", class(requester)))
}
Subscriber.Location <- function(x, context, ...) {
	subscriber <- Connector(x, context=context, type="ZMQ_SUB", ...)
	subscribe(subscriber, ' ')
	structure(subscriber, class=c("Subscriber", class(subscriber)))
}
Subscriber.Context <- function(x, ...) {
	subscriber <- Connector(x, type="ZMQ_SUB", ...)
	subscribe(subscriber, ' ')
	structure(subscriber, class=c("Subscriber", class(subscriber)))
}
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
	stopifnot(is.Subscriber(subscriber))
	listener <- list(replier=x, subscriber=subscriber)
	class(listener) <- c("Listener", class(listener))
	listener
}
Listener.ReplierLocation <- function(x, publisher_location, context, ...) {
	stopifnot(is.missing(publisher_location) || is.publisher_location)
	Listener(Replier(x, context=context, ...),
		 Subscriber(publisher_location, context=context, ...))
}
Listener.Node <- function(x, context, ...) {
	Listener(Replier(x, context=context, ...),
		 Subscriber(context, ...))
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
listen <- function(x, ...) UseMethod("listen")
listen.Listener <- function(x, ...) {
	unlist(poll.socket(x, rep(list("read"), length(x)), ...))
}

Speaker <- function(x, ...) UseMethod("Speaker")
Speaker.Requester <- function(x, publisher, ...) {
	stopifnot(is.Publisher(publisher))
	speaker <- list(requester=x, publisher=publisher)
	class(speaker) <- c("Speaker", class(speaker))
	speaker
}
Speaker.Node <- function(x, context, ...) {
	Speaker(Requester(context, ...),
		Publisher(x, context=context, ...))
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

Communicator <- function(x, ...) UseMethod("Communicator")
Communicator.Listener <- function(x, speaker, ...) {
	stopifnot(is.Speaker(speaker))
	endpoints <- list(listener=x, speaker=speaker)
	class(endpoints) <- c("Communicator", class(endpoints))
	endpoints
}
Communicator.Node <- function(x, ...) {
	context <- Context()
	Communicator(Listener(x, context=context, ...),
		     Speaker(x, context=context, ...))
}
is.Communicator <- function(x) inherits(x, "Communicator")
Listener.Communicator <- function(x, ...) x$listener
Speaker.Communicator <- function(x, ...) x$speaker
Publisher.Communicator <- function(x, ...) Publisher(Speaker(x))
Requester.Communicator <- function(x, ...) Requester(Speaker(x))
Replier.Communicator <- function(x, ...) Replier(Listener(x))
Subscriber.Communicator <- function(x, ...) Subscriber(Listener(x))
PublisherLocation.Communicator <- function(x, ...) Location(Publisher(x))
ReplierLocation.Communicator <- function(x, ...) Location(Replier(x))

format.Communicator <- function(x, ...)
	cat("Communicator:\n",
	    paste0(' ', capture.output(print(Listener(x)))),
	    paste0(' ', capture.output(print(Speaker(x)))))
listen.Communicator <- function(x, ...) c(listen(Listener(x), ...), FALSE, FALSE)
`[.Communicator` <- function(x, i) c(Listener(x)[i], Speaker(x)[i])
