location <- function(address, port) {
	stopifnot(is.character(address),
		  is.integer(port))
	structure(list(address=address, port=port),
		  class="location")
}
address.location <- function(loc) loc$address
port.location <- function(loc) loc$port

is.location <- function(loc) inherits(loc, "location")

format.location <- function(loc, ...) 
	c("<Location", format(address(loc)), format(port(loc)), ">")
print.location <- function(loc, ...) cat(format(loc), sep="\n")
str.location <- function(loc, ...) {
	cat("Location:\n")
	strfields(loc,
		  "Address", address,
		  "Port", port)
}

locate.identifier <- function(id) {
	LOCATION
}
locate.data <- function(data) {
	locate(identifier(data))
}
locate.computation <- locate.data
