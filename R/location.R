location <- function(address, port) {
	stopifnot(is.character(address),
		  is.integer(port))
	structure(list(address=address, port=port),
		  class="location")
}
port.location <- function(loc) loc$port
address.location <- function(loc) loc$address
format.location <- function(loc) {
	as.character(c(underline("Location", '='),
		       underline("Address:", '-'),
		       with.spacing(address(loc)),
		       underline("Port:", '-'),
		       with.spacing(port(loc))))
}
print.location <- function(loc) {
	with.comment(cat(format(loc), sep="\n"))
}
is.location <- function(loc) inherits(loc, "location")

locate.identifier <- function(id) {
	LOCATION
}
locate.computation <- function(comp) {
	locate(id(comp))
}
