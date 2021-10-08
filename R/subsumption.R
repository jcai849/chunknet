child_store <- function(host, parent_address, location_service) {
	HOST(host)
	announce_address(location_service)
	resume_parent(parent_address)
	run_store()
}

run_store <- function() {
	listeners <- list(SUBSCRIBER(), REPLIER())
	poll_method <- lapply(listeners, function(...) "read")
	repeat {
		to_read <- poll(listeners, poll_method, timeout=-1L)
		messages <- lapply(listeners[unlist(to_read)], receive.socket)
		for (i in length(messages)) {
			data_store <- respond(messages[[i]],
					      listeners[unlist(to_read)][[i]],
					      data_store)
		}
	}
}

respond.computation <- function(message, respond_to, data_store) {
	store(message, data_store)
	store(chunk(message), data_store)
	respond(respond_to)
	if (all(available(prereqs(message)))) {
		mcparallel(dofunction..., detached=T)
	}
	# else set callbacks etc.
	data_store
}

respond.data <- function() {
}

respond.association <- function() {
}
