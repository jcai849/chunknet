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

push <- function(value, location) {
	href <- uuid::UUIDgenerate()
	if (missing(location)) {
		location <- get_all_locations()[1,]
	}
	log("Pushing data of %s to address %s port %d", href, location$address, location$port) 
	event_external_push(paste0("POST /data/", href), value, location$address, location$port)
	post_location(href, location)
	structure(list(href=href, generator_href="."), class="Chunk")
}

remote_call <- function(procedure, arguments, alignments) {
	chunk_args <- sapply(arguments, inherits, "Chunk")
	location <- if (!any(chunk_args)) {
		get_all_locations()[1,]
	} else {
		get_location(arguments[chunk_args][[1]]$href)
	}

	arguments <- lapply(arguments, function(arg)
	if (inherits(arg, "Chunk")) arg else push(arg, list(address=location$address, port=location$port)))
	computation <- structure(list(procedure=procedure, arguments=arguments, alignments=NULL,
				      href=uuid::UUIDgenerate(), output_href=uuid::UUIDgenerate()),
				 class="Computation")
	event_external_push(paste0("PUT /computation/", computation$href), computation, location$address, location$port)
	post_location(computation$href, location)
	post_location(computation$output_href, location)
	structure(list(generator_href = computation$href, href=computation$output_href), class="Chunk") 
}

request_pull <- function(href) {
	location <- get_location(href)[1,]
	fd <- event_external_push_keep(paste0("GET /data/", href), NULL, location$address, location$port)
}

pull <- function(x, ...) UseMethod("pull", x)

pull.default <- function(x, ...) {
	fd <- request_pull(x)
	event <- orcv::await_response(fd)
	orcv::event_complete(event$fd)
	event$data$payload
}

pull.Chunk <- function(x, ...) {
	pull(x$href)
}

pull_eventually <- function(href) {
	fd <- request_pull(href)
	orcv::monitor_response(fd)
}
