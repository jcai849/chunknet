node <- function(init_function) {
        function(address, port, init_args) {
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
    on("POST /node", postNode)
    on("GET /nodes", getNodes)
    on("POST /data/*", postDataLoc)
    on("GET /data/*", getDataLocs)
}

worker_init <- function(locator_location) {
    LOCATOR(locator_location$address, locator_location$port)
    event_external_push("POST /node", SELF(),
			LOCATOR()$address, LOCATOR()$port)
    on("POST /data/*", putData)
    on("PUT /computation/*", putComputation)
    on("GET /data/*", getData)
    on("newData *", putData)
    on("newComputation *", newComputation)
    on("prereqIsAvailable *", prereqIsAvailable)
    on("computationIsReady *", computationIsReady)
}

locator <- node(locator_init)
worker <- function(address, port, locator_address, locator_port) {
	node(worker_init)(address, port,
			  list(address=locator_address, port=locator_port))
}

loc_cache <- function() {
	function(address, port) {
	    if (missing(address) && missing(port)) {
		list(address=address, port=port)
	    } else {
		address <<- address
		port    <<- port
	    }
	}
}
SELF <- loc_cache()
LOCATOR <- loc_cache()

log <- function(msg, ...) {
    cat(paste0(format(Sys.time(), "%H:%M:%OS9 "),
        sprintf(msg, ...), "\n"))
}

