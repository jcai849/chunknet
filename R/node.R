node <- function(init_function) {
        function(address=NULL, port=0L, ..., verbose=FALSE) {
		options("largerscaleVerbose" = verbose)
                orcv::start(address, port, threads=1L)
                init_function(...)
                repeat {
                        event <- orcv::receive(keep_conn=TRUE)
                        handle(event)
                        log("...DONE")
                }
        }
}

locator_init <- function(...) {
	log("Locator initialising...")
	on("DELETE /data/*", non_responding(deleteDataLocs))
        on("EXIT", non_responding(function(...) q("no")))
	on("GET /data/*", getDataLocs)	# returns vector of locations
	on("GET /nodes", getNodes)	# returns data frame of locations & loadings
	on("POST /data/*", non_responding(postDataLoc))
	on("POST /node", non_responding(postNode))
}
worker_init <- function(locator_address, locator_port) {
	log("Worker initialising...")
	LOCATOR(locator_address, locator_port)
	orcv::send(LOCATOR(), "POST /node") 
        on("DELETE /data/*", non_responding(deleteData))
        on("EXIT", non_responding(function(...) q("no")))
	on("GET /async/data/*", non_responding(asyncGetData))
	on("GET /data/*", getData)
	on("POST /data/*", non_responding(postData))
	on("PUT /computation/*", non_responding(putComputation))
}

locator_node <- node(locator_init)
worker_node <- node(worker_init)

loc_cache <- function() {
	LOC <- NULL
	function(host, port)
	    if (missing(host) || missing(port)) LOC else LOC <<- orcv::as.Location(host, port)
}
LOCATOR <- loc_cache()

log <- function(msg, ...) {
	if (getOption("largerscaleVerbose", default=FALSE))
		cat(paste0(format(Sys.time(), "%H:%M:%OS9 "), sprintf(msg, ...), "\n"))
}
