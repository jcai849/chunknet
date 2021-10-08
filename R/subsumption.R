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
		data_store <- mapply(respond,
				     messages,
				     listeners[unlist(to_read)],
				     data_store)
	}
}

respond.computation <- function(message, respond_do, data_store) {
	
}

respond.data <- function() {
}

respond.association <- function() {
}
