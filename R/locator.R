postNode <- function(event) {
	payload <- event$data$payload 
	add_node(payload$address, payload$port)
}

getNodes <- function(event) {
	respond(event, get_all_nodes())
}

postDataLoc <- function(event) {
	location <- event$data$payload
	node_href <- get_node(location$address, location$port)
	data_href <- extract(event$data$header, "POST /data/(.*)") 
	add_data(data_href, node_href)
}

getDataLocs <- function(event) {
	data_hrefs <- extract(event$data$header, "GET /data/(.*)")
	node_hrefs <- get_data_nodes(data_hrefs)
	locs <- get_locs(node_hrefs)
	respond(event, locs)
}

Locator <- new.env()
with(Locator, {
	Nodes <- data.frame(node_href=character(), address=character(), port=integer(), loading=integer())
	Data <- data.frame(node_href=character(), data_href=character())
})

add_node <- function(address, port) {
	log("Adding node of address %s and port %d", address, port)
	Locator$Nodes <- rbind(Locator$Nodes, data.frame(node_href=uuid::UUIDgenerate(), address=address, port=port, loading=0L))
}

get_locs <- function(node_hrefs) {
	log("Accessing location of node with href %s", node_hrefs)
	Locator$Nodes[Locator$Nodes$node_href %in% node_hrefs, c("address", "port")]
}

get_all_nodes <- function() {
	log("Returning locations of all known nodes and their loadings")
	Locator$Nodes[, c("address", "port", "loading")]
}

get_node <- function(address, port) {
	log("Getting node by address %s and port %d", address, port)
	Locator$Nodes[Locator$Nodes$address == address & Locator$Nodes$port == port, ]$node_href
}

add_data <- function(data_hrefs, node_hrefs) {
	log("Adding data href %s to node %s", data_hrefs, node_hrefs)
	Locator$Data <- rbind(Locator$Data, data.frame(node_href=node_hrefs, data_href=data_hrefs))
	Locator$Nodes[node_href == node_hrefs,]$loading <- Locator$Nodes[node_href == node_hrefs,]$loading + 1
}

get_data_nodes <- function(data_hrefs) {
	log("Getting locations of data identified by %s", data_hrefs)
	Locator$Data$node_href[Locator$Data$data_href %in% data_hrefs]
}
