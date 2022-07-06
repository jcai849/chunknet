deleteDataLocs <- function(event) {
        data_href <- extract(orcv::header(event), "DELETE /data/(.*)")
        delete_data_locs(data_href)
}

postNode <- function(event) {
	loc <- orcv::location(event)
	add_node(loc)
}

getNodes <- function(event) {
	orcv::send(event, "NODES", get_all_nodes())
}

postDataLoc <- function(event) {
	loc <- orcv::payload(event)
	data_href <- extract(orcv::header(event), "POST /data/(.*)") 
	add_data(loc, data_href)
}

getDataLocs <- function(event) {
	data_hrefs <- extract(orcv::header(event), "GET /data/(.*)")
	locs <- get_locs(data_hrefs)
	orcv::send(event, "LOCS", locs)
}

Locator <- new.env()
with(Locator, {
	Data <- data.frame(location=list(), data_href=character())
	Nodes <- data.frame(location=list(), loading=integer())
})

get_locs <- function(data_href) {
	Locator$Data$location[Locator$Data$data_href %in% data_href] 
}

add_node <- function(loc) {
	Locator$Nodes <- rbind(Locator$Nodes, data.frame(location=loc, loading=0L))
}

get_all_nodes <- function() {
	Locator$Nodes
}

add_data <- function(loc, data_href) {
	Locator$Data <- rbind(Locator$Data, data.frame(location=loc, data_href=data_href))
	relevant_nodes <- Locator$Nodes$location == loc
	Locator$Nodes[relevant_nodes,]$loading <- Locator$Nodes[relevant_nodes,]$loading + 1
}

delete_data_locs <- function(data_href) {
        Locator$Data <- Locator$Data[!Locator$Data$data_href %in% data_href,]
}
