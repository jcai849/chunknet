node <- function(init_function) {
        function(address, port, init_args) {
                LOC(address, port)
                orcv::start(LOC()$port)
                init_function(init_arg)
                repeat {
                        event <- next_event()
                        handle(event)
                        log("...DONE")
                }
        }
}

locator_init <- function(...) {
    on("POST /node/*", postNode)
    on("GET /nodes", getNodes)
    on("PUT /node/*/*", putDataLoc)
    on("GET /data/*", getDataLoc)
}

worker_init <- function(locator_location) {
    register_location(locator_location$address, locator_location$port)
    on("PUT /data/*", putData)
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

on <- function(event, handler) {
    log("Adding handler for event %s", event)
    assign(event, handler, Events)
}

next_event <- function() {
    event <- orcv::event_pop()
    log("Pulling Event: %s", event$data$header)
    event
}

event_internal_push <- function(event, context) {
    log("Pushing internal event: %s", event)
    fd <- orcv::event_push(list(header=event, payload=context),
			   LOC()$address, LOC()$port)
    orcv::event_complete(fd)
}

register_location <- function(loc_service_address, loc_service_port) {
    NODE(uuid::UUIDgenerate())
    header <- paste0("POST /node/", NODE())
    fd <- orcv::event_push(list(header=header, payload=NODE()),
                           loc_service_address, loc_service_port)
    orcv::event_complete(fd)
}

LOC <- local(function(address, port) {
    if (missing(address) && missing(port)) {
        list(address=address, port=port)
    } else {
        address <<- address
        port    <<- port
    }
})

NODE <- local(function(node_id) if (missing(node_id)) node_id else node_id <<- node_id)

log <- function(msg, ...) {
    cat(paste0(format(Sys.time(), "%H:%M:%OS9 "),
        sprintf(msg, ...), "\n"))
}

