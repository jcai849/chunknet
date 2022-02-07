postNode <- function(event) {
	add_node(event$data$payload)
}

getNodes <- function(event) {
	respond(event, get_all_nodes)
}

postDataLoc <- function(event) {
	location <- event$data$payload
	node_href <- get_node(location$address, location$port)
	data_href <- extract(event$data$header, "POST /data/(.*)") 
	add_data(data_href, node_href)
}

getDataLocs <- function(event) {
	data_href <- extract(event$data$header, "GET /data/(.*)")
	node_hrefs <- get_data_nodes(data_href)
	locs <- get_locs(node_hrefs)
	respond(event, locs)
}

Locator <- new.env()
with(Locator, {
	Nodes <- data.frame(node_href=character(), address=character(), port=integer())
	Data <- data.frame(node_href=character(), data_href=character())
})

add_node <- function(address, port) with(Locator, {
	log("Adding node of address %s and port %d", address, port)
	Nodes <- rbind(Nodes, data.frame(node_href=UUID::getuuid(), address=address, port=port))
})

get_locs <- function(node_hrefs) {
	log("Accessing location of node with href %s", node_hrefs)
	Nodes <- get("Nodes", Locator)
	Nodes[Nodes$node_href %in% node_hrefs, c("address", "port")]
}

get_all_nodes <- function() {
	log("Returning locations of all known nodes")
	get("Nodes", Locator)[, c("address", "port")]
}

get_node <- function(address, port) {
	log("Getting node by address %s and port %d", address, port)
	Nodes <- get("Nodes", Locator)
	Nodes[Nodes$address == address & Nodes$port == port, c("address", "port")]
}

add_data <- function(data_hrefs, node_hrefs) with(Data, {
	log("Adding data href %s to node %s", data_hrefs, node_hrefs)
	Data <- rbind(Data, data.frame(node_href=node_hrefs, data_href=data_hrefs))
})

get_data_nodes <- function(data_hrefs) {
	log("Getting locations of data identified by %s", data_hrefs)
	Data <- get("Data", Locator)
	Data$node_href[Data$data_href %in% data_href]
}
