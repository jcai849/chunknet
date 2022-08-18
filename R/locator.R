Locator <- new.env()
with(Locator, {
	Data <- data.frame(location=list(), data_href=character())
	Nodes <- data.frame(location=list(), loading=integer())
})

deleteDataLocs <- function(event) {
        data_hrefs <- extract(orcv::header(event), "DELETE /data/(.*)")
        Locator$Data <- Locator$Data[!Locator$Data$data_href %in% data_hrefs,]
}

getDataLocs <- function(event) {
	data_href <- extract(orcv::header(event), "GET /data/(.*)")
	Locator$Data[Locator$Data$data_href %in% data_href, "location"] 
}

getHost <- function(event) {
	hosts <- extract(orcv::header(event), "GET /host/(.*)")
	addresses <- lapply(hosts, orcv::as.Location, port=0L)
	host_i <- match(addresses, orcv::address(Locator$Nodes$location))
	Locator$Nodes[host_i, "location"]
}

getNode <- function(event) { # returns least loaded n nodes, assumes loading will take place
	n <- extract(orcv::header(event), "GET /node/(.*)")
	least_loaded_i <- rep_len(order(Locator$Nodes[i,]$loading), n)
	Locator$Nodes$loading[least_loaded_i] <- Locator$Nodes$loading[least_loaded_i] + 1
	Locator$Nodes[least_loaded_i, "location"]
}

getAllNodes <- function(event) Locator$Nodes$location

postDataLocs <- function(event) {
	locations <- orcv::payload(event)
	data_hrefs <- extract(orcv::header(event), "POST /data/(.*)") 
	Locator$Data <- rbind(Locator$Data, data.frame(location=locations, data_href=data_hrefs))
	relevant_nodes <- Locator$Nodes$location == locations
	Locator$Nodes[relevant_nodes, "loading"] <- Locator$Nodes[relevant_nodes, "loading"] + 1
}

postNode <- function(event) {
	loc <- orcv::location(event)
	Locator$Nodes <- rbind(Locator$Nodes, data.frame(location=loc, loading=0L))
}
