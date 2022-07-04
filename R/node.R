node <- function(init_function) {
        function(address=NULL, port=0L, init_arg, verbose=FALSE) {
		options("largerscaleVerbose" = verbose)
                orcv::start(address, port)
                init_function(init_arg)
                repeat {
                        event <- orcv::receive(keep_conn=TRUE)
                        handle(event)
                        log("...DONE")
                }
        }
}

locator_init <- function(...) {
	log("Locator initialising...")
	on("POST /node", non_responding(postNode))
	on("GET /nodes", getNodes)
	on("POST /data/*", non_responding(postDataLoc))
	on("GET /data/*", getDataLocs)
	on("DELETE /data/*", non_responding(deleteDataLocs))
        on("EXIT", non_responding(function(...) q("no")))
}
worker_init <- function(locator_location) {
	log("Worker initialising...")
	LOCATOR(locator_location)
	send(LOCATOR(), "POST /node") 
	on("POST /data/*", non_responding(postData))
	on("GET /data/*", getData)
	on("POST /send-data/*", non_responding(postSendData))
	on("PUT /computation/*", non_responding(putComputation))
        on("DELETE /data/*", non_responding(deleteData))
        on("EXIT", non_responding(function(...) q("no")))
}

locator_node <- node(locator_init)
worker_node <- node(worker_init)

loc_cache <- function() {
	LOC <- NULL
	function(location)
	    if (missing(location)) LOC else LOC <<- location
}
LOCATOR <- loc_cache()

log <- function(msg, ...) {
	if (getOption("largerscaleVerbose", default=FALSE))
		cat(paste0(format(Sys.time(), "%H:%M:%OS9 "), sprintf(msg, ...), "\n"))
}
