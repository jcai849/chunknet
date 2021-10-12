Location <- function(host, port) {
        stopifnot(is.character(host), is.integer(port))
        structure(list(host=host, port=port), class="Location")
}
is.Location <- function(location) inherits(location, "Location")

# coercion
Key.Location <- as.character.Location <- function(x) {
        past0("tcp://", host, ":", port)
}
Location.Endpoint <- function(x) {
        address <- get.last.endpoint(x)
        re <- ".*://(.+):([[:digit:]]+).*"
        location <- regmatches(address, regexec(re, address))[[1]]
        Location(host=location[2], port=as.integer(location[3]))
}
