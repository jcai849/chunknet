# TODO: like and subscribe

genworker <- function(host) {
	tmpsock <- init.socket(maincontext(), "ZMQ_REP")
	bind.socket(tmpsock, "tcp://*:0")
	tmpaddr <- get.last.endpoint(tmpsock)
	system2("ssh", host, # Initialise R session with worker at given host,
		shQuote(paste("R", "-e", # passing in the tmp addr to contact.
			      paste0("largerscale::worker(", tmpaddr, ")"))))
	workeraddr <- receive.socket(tmpsock)					# 1. receive worker addr
	socket.send(tmpsock, list(wp=workerpool(), s=subscriptionaddr())	# 2. send workerpool and subscription address
	disconnect.socket(tmpsock)
	store(workeraddr, workerpool())
}

worker <- function(generatoraddr) {
	connect.socket(mainrequester(), generatoraddr)
	send.socket(mainrequester(), get.last.endpoint(mainreplier()))	# 1. send self addr
	msg <- receive(generatoraddr)	  				# 2. receive workerpool and subscription addr
	disconnect.socket(mainrequester())
	eapply(msg$wp, store, workerpool())
	dispatchermode()  # TODO
}

mainsocket <- function(type) {
	socket <- NULL
	function()
		if (is.null(socket)) {
			socket <<- init.socket(maincontext(), type)
			if (identical(type, "ZMQ_REP")) bind.socket(socket, "tcp://*:0")
		} else socket
}
mainrequester <- mainsocket("ZMQ_REQ")
mainreplier <- mainsocket("ZMQ_REP")

addr <- function(sock) get.last.endpoint(sock())
