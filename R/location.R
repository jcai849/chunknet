Location <- function(x, ...) UseMethod("Location")
Location.character <- function(x, port, ...) {
        stopifnot(is.integer(port))
        structure(list(host=x, port=port), class="Location")
}
is.Location <- function(x) inherits(x, "Location")
host <- function(x) x$host
port <- function(x) x$port
format.Location <- function(x, ...) as.character(x)
print.Location <- function(x, ...) cat("Location: ", format(x), '\n')
as.character.Location <- function(x) {
        paste0("tcp://", host(x), ":", port(x))
}

PublisherLocation <- function(x, ...) UseMethod("PublisherLocation")
SubscriberLocation <- function(x, ...) UseMethod("SubscriberLocation")
ReplierLocation <- function(x, ...) UseMethod("ReplierLocation")
RequesterLocation <- function(x, ...) UseMethod("RequesterLocation")

ReplierLocation.character <- function(x, ...) {
	replier_location <- Location(x, ...)
	structure(replier_location, class=c("ReplierLocation", class(replier_location)))
}
PublisherLocation.character <- function(x, ...) {
	publisher_location <- Location(x, ...)
	structure(publisher_location, class=c("PublisherLocation", class(publisher_location)))
}

Location.Endpoint <- function(x, ...) {
        address <- get.last.endpoint(x)
        re <- ".*://(.+):([[:digit:]]+).*"
        location_marshall <- regmatches(address, regexec(re, address))[[1]]
	location <- Location(host=location_marshall[2],
			     port=as.integer(location_marshall[3]))
	structure(location,
		  class=c(paste0(class(x), "Location"), class(location)))
}
is.PublisherLocation <- function(x) inherits(x, "PublisherLocation")
is.SubscriberLocation <- function(x) inherits(x, "SubscriberLocation")
is.ReplierLocation <- function(x) inherits(x, "ReplierLocation")
is.RequesterLocation <- function(x) inherits(x, "RequesterLocation")

IdentifiedLocations <- function(x, ...) {
	if (missing(x)) {
		IdentifiedLocations.Identifier()
	} else UseMethod("IdentifiedLocations")
}
IdentifiedLocations.Identifier <- function(x, location) {
	stopifnot(missing(location) || is.Location(location))
	identified_locations <- if (missing(x)) {
		AssociativeArray()
	} else AssociativeArray(x, location)
	structure(identified_locations,
		  class=c("IdentifiedLocations", class(identified_locations)))
}
is.IdentifiedLocations <- function(x) inherits(x, "IdentifiedLocations")

Node <- function(x, ...) UseMethod("Node")
Node.ReplierLocation <- function(x, publisher_location) {
	stopifnot(is.PublisherLocation(publisher_location))
	node <- list(replier_location=x,
		     publisher_location=publisher_location)
	structure(node, class=c("Node", class(node)))
}
is.Node <- function(x) inherits(x, "Node")
ReplierLocation.Node <- function(x, ...) x$replier_location
PublisherLocation.Node <- function(x, ...) x$publisher_location
print.Node <- function(x, ...)
	cat("Node: ",
	    paste0(' ', capture.output(print(ReplierLocation(x)))),
	    paste0(' ', capture.output(print(PublisherLocation(x)))),
	    sep="\n")

Nodes <- function(...) if (missing(...)) Nodes.Node() else UseMethod("Nodes", ..1)
Nodes.Node <- function(...) {
	nodes <- list(...)
	stopifnot(!length(nodes) || sapply(nodes, is.Node))
	structure(nodes, class=c("Nodes", class(nodes)))
}
ReplierLocation.Nodes <- function(x, ...) lapply(x, ReplierLocation)
PublisherLocation.Nodes <- function(x, ...) lapply(x, PublisherLocation)
Node.Communicator <- function(x, ...) {
    Node(ReplierLocation(x), PublisherLocation(x))
}

Index <- function(x, ...) {
	if (missing(x)) Index(Nodes(), IdentifiedLocations()) else UseMethod("Index")
}
Index.Nodes <- function(x, identified_locations, ...) {
	stopifnot(is.IdentifiedLocations(identified_locations))
	index <- list(nodes=x, identified_locations=identified_locations)
	structure(index, class=c("Index", class(index)))
}
Index.Node <- function(x, ...) {
	Index(Nodes(x), ...)
}
Index.Communicator <- function(x, ...) {
	Index(Node(x), IdentifiedLocations())
}
is.Index <- function(x) inherits(x, "Index")
Nodes.Index <- function(...) ..1$nodes
IdentifiedLocations.Index <- function(x, ...) x$identified_locations
ReplierLocation.Index <- function(x, ...) ReplierLocation(Nodes(x))
PublisherLocation.Index <- function(x, ...) PublisherLocation(Nodes(x))
