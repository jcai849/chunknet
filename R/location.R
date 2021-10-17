Location <- function(x, ...) UseMethod("as.Location")
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

Node <- function(replier_location, publisher_location) {
	stopifnot(is.ReplierLocation(replier_location),
		  is.PublisherLocation(publisher_location))
	node <- list(replier_location=replier_location,
		     publisher_location=publisher_location)
	structure(node, class=c("Node", class(node)))
}
is.Node <- function(x) inherits(x, "Node")
ReplierLocation.Node <- function(x, ...) x$replier_location
PublisherLocation.Node <- function(x, ...) x$publisher_location

Nodes <- function(...) {
	nodes <- list(...)
	stopifnot(sapply(nodes, is.Node))
	structure(nodes, class=c("Nodes", class(nodes)))
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
