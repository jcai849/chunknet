post_location <- function(href, location) {
	log("Sending location of %s to Locator", href)
	event_external_push(paste0("POST /data/", href), location, LOCATOR()$address, LOCATOR()$port)
}

get_location <- function(href) {
	fd <- orcv::event_push(list(header=paste0("GET /data/", href)), LOCATOR()$address, LOCATOR()$port)
	event <- orcv::await_response(fd)
	orcv::event_complete(event)
	event$data
}

get_all_locations <- function() {
	fd <- orcv::event_push(list(header="GET /nodes"), LOCATOR()$address, LOCATOR()$port)
	event <- orcv::await_response(fd)
	orcv::event_complete(event)
	event$data[order(event$data$loading), c("address", "port")]
}

remote_call <- function(procedure, arguments) {
	chunkref_args <- sapply(arguments, inherits, "ChunkReference")
        location <- if (!any(chunkref_args)) {
                get_all_locations()[1,]
        } else {
                get_location(arguments[chunkref_args][[1]]$href)
        }
	arguments <- lapply(arguments, function(arg)
                if (inherits(arg, "ChunkReference")) arg else push(arg, data.frame(address=location$address, port=location$port)))

	compref <- ComputationReference(procedure, arguments)
	event_external_push(paste0("PUT /computation/", compref$href), compref, location$address, location$port)
	post_location(compref$output_href, location)
	ChunkReference(compref$output_href)
}

request_pull <- function(href) {
	location <- get_location(href)[1,]
	fd <- event_external_push_keep(paste0("GET /data/", href), NULL, location$address, location$port)
}

push <- function(value, location) {
	chunkref <- ChunkReference()
	if (missing(location)) {
		log("push missing location. Accessing all locations")
                location <- get_all_locations()[1,]
        } else if (is.character(location)) {
		log("push given only hostname. Accessing all locations for address")
                all_locs <- get_all_locations()
                location <- all_locs[all_locs$address == location,][1,]
        }
	post_location(chunkref$href, location)
	log("Pushing data of %s to address %s port %d", chunkref$href, location$address, location$port) 
	event_external_push(paste0("POST /data/", chunkref$href), value, location$address, location$port)
	chunkref
}

pull <- function(x, ...) UseMethod("pull", x)

pull.default <- function(x, ...) {
	fd <- request_pull(x)
	event <- orcv::await_response(fd)
	orcv::event_complete(event$fd)
	event$data$payload
}

pull.ChunkReference <- function(x, ...) {
	pull(x$href)
}

pull_eventually <- function(href) {
	fd <- request_pull(href)
	orcv::monitor_response(fd)
}

kill_all_nodes <- function() {
        all_locations <- get_all_locations()
        for (i in seq(NROW(all_locations)))
                 event_external_push("EXIT", NULL, all_locations[i, "address"], all_locations[i, "port"])
        event_external_push("EXIT", NULL, LOCATOR()$address, LOCATOR()$port)
        invisible(NULL)
}

