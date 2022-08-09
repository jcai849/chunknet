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
	Locator$Data$location[Locator$Data$data_href %in% data_href] 
}

getNode <- function(event) { # returns least loaded node, assumes loading will take place
	addresses <- extract(orcv::header(event), "GET /node/(.*)")
	i <- if (!length(addresses)) TRUE else sapply(Locator$Nodes$location, orcv::address) %in% as.integer(addresses)
	least_loaded_i <- which.min(Locator$Nodes[i,]$loading)
	Locator$Nodes[i,]$loading[least_loaded_i] <- Locator$Nodes[i,]$loading[least_loaded_i] + 1
	Locator$Nodes[i,]$location[least_loaded_i]
}

getAllNodes <- function(event) Locator$Nodes$location

postDataLocs <- function(event) {
	locations <- orcv::payload(event)
	data_hrefs <- extract(orcv::header(event), "POST /data/(.*)") 
	Locator$Data <- rbind(Locator$Data, data.frame(location=locations, data_href=data_hrefs))
	relevant_nodes <- Locator$Nodes$location == locations
	Locator$Nodes[relevant_nodes,]$loading <- Locator$Nodes[relevant_nodes,]$loading + 1
}

postNode <- function(event) {
	loc <- orcv::location(event)
	Locator$Nodes <- rbind(Locator$Nodes, data.frame(location=loc, loading=0L))
}
