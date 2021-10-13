Location <- function(host, port) {
        stopifnot(is.character(host), is.integer(port))
        structure(list(host=host, port=port), class="Location")
}
is.Location <- function(location) inherits(location, "Location")
format.Location <- function(location, ...) as.character(location)
print.Location <- function(location, ...) cat(format(location), '\n')

as.character.Location <- function(x) {
        past0("tcp://", host, ":", port)
}
as.Location.Endpoint <- function(x) {
        address <- get.last.endpoint(x)
        re <- ".*://(.+):([[:digit:]]+).*"
        location <- regmatches(address, regexec(re, address))[[1]]
        Location(host=location[2], port=as.integer(location[3]))
}
