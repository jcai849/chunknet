Location <- function(host, port) {
        stopifnot(is.character(host), is.integer(port))
        structure(list(host=host, port=port), class="Location")
}
is.Location <- function(x) inherits(x, "Location")
format.Location <- function(x, ...) as.character(x)
print.Location <- function(x, ...) cat("Location: ", format(x), '\n')

host <- function(x) x$host
port <- function(x) x$port
as.character.Location <- function(x) {
        paste0("tcp://", host(x), ":", port(x))
}
as.Location <- function(x, ...) UseMethod("as.Location")
as.Location.Endpoint <- function(x, ...) {
        address <- get.last.endpoint(x)
        re <- ".*://(.+):([[:digit:]]+).*"
        location_marshall <- regmatches(address, regexec(re, address))[[1]]
        location <- Location(host=location_marshall[2], port=as.integer(location_marshall[3]))
	structure(location, class=c(paste0(class(x), "Location"), class(location)))
}
is.EndpointLocation <- function(x) inherits(x, "EndpointLocation")
is.PublisherLocation <- function(x) inherits(x, "PublisherLocation")
is.SubscriberLocation <- function(x) inherits(x, "SubscriberLocation")
is.ReplierLocation <- function(x) inherits(x, "ReplierLocation")
is.RequesterLocation <- function(x) inherits(x, "RequesterLocation")
