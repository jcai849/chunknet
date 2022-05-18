node <- function(init_function) {
        function(address, port, init_arg, verbose=FALSE) {
		options("largerscaleVerbose" = verbose)
                SELF(address, port)
                orcv::start(SELF()$port)
                init_function(init_arg)
                repeat {
                        event <- next_event()
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
	LOCATOR(locator_location$address, locator_location$port)
	log("Sending location of address %s and port %d to locator node", SELF()$address, SELF()$port)
	event_external_push("POST /node", SELF(), LOCATOR()$address, LOCATOR()$port)
	on("POST /data/*", non_responding(postData))
	on("GET /data/*", getData)
	on("PUT /computation/*", non_responding(putComputation))
        on("DELETE /data/*", non_responding(deleteData))
        on("EXIT", non_responding(function(...) q("no")))
}

locator <- node(locator_init)
worker <- function(address, port, locator_address, locator_port, verbose=FALSE) {
	node(worker_init)(address, port,
			  list(address=locator_address, port=locator_port),
			  verbose)
}

loc_cache <- function() {
	ADDRESS <- character()
	PORT <- integer()
	function(address, port) {
	    if (missing(address) && missing(port)) {
		data.frame(address=ADDRESS, port=PORT)
	    } else {
		ADDRESS <<- address
		PORT    <<- port
	    }
	}
}
SELF <- loc_cache()
LOCATOR <- loc_cache()

log <- function(msg, ...) {
	if (getOption("largerscaleVerbose", default=FALSE))
		cat(paste0(format(Sys.time(), "%H:%M:%OS9 "), sprintf(msg, ...), "\n"))
}
