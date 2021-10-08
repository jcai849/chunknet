spawn_service <- function(generate_service_initialisation_command) {
	synchronizer <- init.socket(CONTEXT(), "ZMQ_REP")
	bind.socket(synchronizer, paste0("tcp://", HOST(), ":0"))
	system2("ssh", host, shQuote(paste("R", "-e",
		   generate_service_initialisation_command(host, synchronizer))))
	message <- receive.socket(synchronizer)
	send.socket(socket, "ACK")
	message
}
alter_body <- function(alter) {
	function(f) {
		body(f) <- alter(body(f))
		f
	}
}
backquoted_body <- alter_body(bquote)
deparsed_body <- alter_body(deparse1)
spawn_location_service <- function(host) {
	generate_location_service_command <- function(host, synchronizer) {
		largerscale::location_service(host=.(host),
		      parent_address=.(get.last.endpoint(synchronizer)))
	}
	location_service_address <-
		spawn_service(deparsed_body(backquoted_body(generate_location_service_command)))()
	LOCATION_SERVICE(location_service_address)
}
spawn_store <- spawn_service(deparsed_body(backquoted_body(function(host, synchronizer) {
	largerscale::child_store(host=.(host),
		 parent_address=.(get.last.endpoint(synchronizer)),
		 location_service=.(LOCATION_SERVICE())))))
}

resume_parent <- function(parent_address) {
	requester <- init.socket(CONTEXT(), "ZMQ_REQ")
	connect.socket(requester, parent_address)
	send.socket(requester, get.last.endpoint(REPLIER()))
	receive.socket(requester)
	disconnect.socket(requester, parent_address)
}
announce_address <- function(location_service) {
	LOCATION_SERVICE(location_service)
	requester <- init.socket(CONTEXT(), "ZMQ_REQ")
	connect.socket(requester, LOCATION_SERVICE())
	send.socket(requester, get.last.endpoint(REPLIER()))
	subscribe_to <- receive.socket(requester)
	connect.socket(SUBSCRIBER(), subscribe_to)
	disconnect.socket(requester, LOCATION_SERVICE())
}

set_sticky_value <- function() {
	value <- NULL
	function(set) {
		if (!missing(set)) value <<- set
		value
	}
}
HOST <- set_sticky_value()
LOCATION_SERVICE <- set_sticky_value()
initialise_sticky_value <- function(initialisation) {
	value <- NULL
	function() {
		if (is.null(value)) value <<- initialisation()
		value
	}
}
CONTEXT <- initialise_sticky_value(init.context)
SUBSCRIBER <- initialise_sticky_value(function() {
			SUBSCRIBER <<- init.socket(CONTEXT(), "ZMQ_SUB")
			subscribe(subscriber, '')
			SUBSCRIBER
})
initialise_sticky_communicator <- function(SOCK_TYPE) {
	binder_initialisation <- function() {
		replier <- init.socket(CONTEXT(), SOCK_TYPE)
		bind.socket(replier, paste0("tcp://", HOST(), ":0"))
		replier
	}
	initialise_sticky_value(binder_initialisation)
}
REPLIER <- initialise_sticky_communicator("ZMQ_REP")
PUBLISHER <- initialise_sticky_communicator("ZMQ_PUB")
