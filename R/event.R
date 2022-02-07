# Event {
#       FD fd
#       list data {
#                  character header
#                  (maybe) (any) payload
#                 }
# }

Events <- new.env()

on <- function(event, handler) {
    log("Adding handler for event %s", event)
    assign(event, handler, Events)
}

handle <- function(event) {
    events <- names(Events)
    event_name <- event$data$header
    log("Handling Event: %s", event_name)
    handler_name <- events[Vectorize(grepl)(glob2rx(events), event_name)]
    handler <- get(handler_name, Events)
    handler(event)
}

non_responding <- function(handler) {
	function(event) {
		handler(event)
		orcv::event_complete(event)
	}
}

respond <- function(fd, data) {
        orcv::respond(fd, data)
        orcv::event_complete(fd)
}

next_event <- function() {
    event <- orcv::event_pop()
    log("Pulling Event: %s", event$data$header)
    event
}

event_external_push_keep <- function(header, payload, address, port) {
	log("Pushing event: %s", header)
	orcv::event_push(list(header=header, payload=payload), address, port)
}

event_external_push <-  function(header, payload, address, port) {
	fd <- event_external_push_keep(header, payload, address, port)
	orcv::event_complete(fd)
}

event_internal_push <- function(header, payload) {
	event_external_push(header, payload, SELF()$address, SELF()$port)
}

extract <- function(text, pattern) {
	regmatches(text, regexec(pattern, text))[[1]][-1]
}
