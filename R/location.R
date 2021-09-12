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
	c("Location", format(address(loc)),  format(port(loc)))
print.location <- function(loc, ...) cat(format(loc), "\n")
str.location <- function(loc, ...) {
	cat("Location:\n")
	strfields(loc,
		  "Address", address,
		  "Port", port)
}

locate.identifier <- function(id) {
	LOCATION
}
locate.data <- locate.computation <- function(data) {
	locate(identifier(data))
}

dependencygraph <- function(x) {
	cat("digraph G {\n")
	graph(x)
	cat("}\n")
}
graph.data <- function(dat) {
	arc(emerge(computation(dat)), dat)
	invisible(graph(emerge(computation(dat))))
}
graph.computation <- function(comp) {
	mapply(arc,
	       input(comp),
	       rep(list(comp), length(input(comp))))
	lapply(input(comp),
	       function(i) if (is.data(i))
		       arc(emerge(computation(i)), comp))
	invisible(lapply(input(comp), graph))
}
graph.default <- function(...) invisible(NULL)

arc.data <- arc.computation <- function(to, from) {
	cat(paste0(format(from), collapse="_"), " -> ", paste0(format(to), collapse="_"), ";\n")
}
arc.default <- function(to, from) {
	cat(paste0(format(from), collapse="_"), " -> ", '"', class(to)[1], '"', ";\n")
}
