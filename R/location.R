location_service <- function(host, parent_address) {
	HOST(host)
	if (!missing(parent_address)) resume_parent(parent_address)
	run_locator()
}

run_locator <- function() {
	full_node_graph <- node_graph()
	repeat {
		message <- receive.socket(REPLIER())
		full_node_graph <- respond(message, full_node_graph)
	}
}

respond.identifier <- function(identifier, full_node_graph) {
	matching_identifiers <- full_node_graph$identifier == identifier
	repsonse <- if (!any(matching_identifiers)) {
		sample(full_node_graph$node, 1)
	} else {
		full_node_graph$node[matching_identifiers][1]
	}
	send.socket(REPLIER(), response)
	full_node_graph
}
respond.Location <- function(identifier, full_node_graph) {
	updated_node_graph <- c(full_node_graph, node_graph(identifier=NA, node=location))
	send.socket(REPLIER(), get.last.endpoint(SUBSCRIBER()))
	updated_node_graph
}

node_graph <- function(identifier, node) {
	if (missing(identifier) && missing(node)) {
		structure(data.frame(identifier=NULL, node=NULL))
	} else {
		structure(data.frame(identifier=identifier, node=node),
			  class="NodeGraph")
	}
}

c.NodeGraph <- function(...) rbind(...)

Location <- function(host, port) {
	stopifnot(is.character(host),
		  is.integer(port))
	structure(list(host=host, port=port),
		  class="Location")
}
host.Location <- function(loc) loc$host
port.Location <- function(loc) loc$port

is.Location <- function(loc) inherits(loc, "Location")

format.Location <- function(loc, ...) 
		c("Location", format(host(loc)),  format(port(loc)))
print.Location <- function(loc, ...) cat(format(loc), "\n")

locate <- function(x, ...) UseMethod("locate")
locate.Location <- function(location, ...) {
}
locate.identifier <- function(id, ...) {
}
locate.chunk <- locate.data <- locate.computation <- function(data, ...) {
}
