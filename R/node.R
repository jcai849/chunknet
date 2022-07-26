Events <- new.env() # event_name (header) -> handler function

handle <- function(event) {
    events <- names(Events)
    event_name <- orcv::header(event)
    handler_name <- events[Vectorize(grepl)(glob2rx(events), event_name)]
    handler <- get(handler_name, Events)
    handler(event)
}

node <- function(init_function) {
        function(address=NULL, port=0L, ..., verbose=FALSE) {
		options("chunknetVerbose" = verbose)
                orcv::start(address, port, threads=1L)
                init_function(...)
                repeat {
                        event <- orcv::receive(keep_conn=TRUE)
                        handle(event)
                        log("...DONE")
                }
        }
}

responding_internally <- function(handler) function(event) {
	handler(event)
}
non_responding_to_fd <- function(handler) function(event) {
        handler(event)
        close(event)
}
responding_with_work_started <- function(handler) function(event) {
        orcv::send(orcv::fd(event), "200")
        handler(event)
}
responding_with_result <- function(handler) function(event) {
        res <- handler(event)
        orcv::send(orcv::fd(event), "RES", res)
}

on <- function(event, handler) {
    log("Adding handler for event %s", event)
    assign(event, handler, Events)
}

locator_init <- function(...) {
	log("Locator initialising...")
	on("DELETE /data/*", non_responding_to_fd(deleteDataLocs))
        on("EXIT", non_responding_to_fd(function(...) q("no")))
	on("GET /data/*", responding_with_result(getDataLocs))	# returns vector of locations
	on("GET /nodes", responding_with_result(getNodes))	# returns data frame of locations & loadings
	on("POST /data/*", responding_with_work_started(postDataLoc))
	on("POST /node", non_responding_to_fd(postNode))
}
worker_init <- function(locator_address, locator_port) {
	log("Worker initialising...")
	LOCATOR(locator_address, locator_port)
	orcv::send(LOCATOR(), "POST /node") 
        on("DELETE /data/*", non_responding_to_fd(deleteData))
        on("EXIT", non_responding_to_fd(function(...) q("no")))
	on("GET /async/data/*", non_responding_to_fd(asyncGetData))
	on("GET /data/*", responding_internally(getData))
	on("POST /data/*", non_responding_to_fd(postData))
	on("PUT /computation/*", non_responding_to_fd(putComputation))
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
	if (getOption("chunknetVerbose", default=FALSE))
		cat(paste0(format(Sys.time(), "%H:%M:%OS9 "), sprintf(msg, ...), "\n"))
}

# extract the group given in the pattern from the text, splitting output
extract <- function(text, pattern, split=',') {
        strsplit(regmatches(text, regexec(pattern, text))[[1]][-1], split)
}
