location_service <- function(host, parent_address) {
	HOST(host)
	resume_parent(parent_address)
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
respond.location <- function(identifier, full_node_graph) {
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

c.NodeGraph <- function(...) rbind(...)
