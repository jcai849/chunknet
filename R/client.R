post_location <- function(href, location) {
	event_external_push(paste0("POST /data/", href), location, LOCATOR()$address, LOCATOR()$port)
}

get_location <- function(href) {
	loc_header <- paste0("GET /data/", href)
	fd <- orcv::event_push(list(header=paste0("GET /data/", href)), LOCATOR()$address, LOCATOR$port)
	event <- orcv::await_response(fd)
	orcv::event_complete(event)
	event$data
}

push <- function(value, location) {
	href <- uuid::UUIDgenerate()
	if (missing(location)) {
		location <- get_location(href)[1,]
		post_location(href, location)
	}
	event_external_push(paste0("POST /data/", href), value, location$address, location$port)
	structure(list(href=href, generator_href="."), class="Chunk")
}

remote_call <- function(procedure, arguments) {
	location <- get_location(arguments)[1,]

	arguments <- lapply(arguments, function(arg)
	if (inherits(arg, "Chunk")) arg else push(arg, list(address=location$address, port=location$port)))
	computation <- structure(list(procedure=procedure, arguments=arguments, alignments=NULL,
				      href=uuid::UUIDgenerate(), output_href=uuid::UUIDgenerate()),
				 class="Computation")
	post_location(computation$href, location)
	post_location(computation$output_href, location)
	event_external_push(paste0("PUT /computation/", computation$href), computation, location$address, location$port)
	structure(list(generator_href = computation$href, href=computation$output_href), class="Chunk") 
}

request_pull <- function(href) {
	location <- get_location(href)[1,]
	fd <- event_external_push_keep(paste0("GET /data/", href), NULL, location$address, location$port)
}

pull <- function(href) {
	fd <- request_pull(href)
	event <- orcv::await_response(fd)
	orcv::event_complete(event$fd)
	event$data
}

pull_eventually <- function(href) {
	fd <- request_pull(href)
	monitor_response(fd)
}
