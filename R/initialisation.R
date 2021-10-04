gendispatcher <- function(host) {
	tmpsock <- init.socket(maincontext(), "ZMQ_REP")
	bind.socket(tmpsock, paste0("tcp://", addr(), ":0"))
	tmpaddr <- get.last.endpoint(tmpsock)
#	system2("ssh", host, # Initialise R session with worker at given host,
#		shQuote(paste("R", "-e", # passing in the tmp addr to contact.
#			      paste0("largerscale::dispatcher(", tmpaddr, ", ", host, ")"))))
	workeraddr <- receive.socket(tmpsock)				# 1. receive worker addr
	subaddr <- get.last.endpoint(mainpublisher)
	socket.send(tmpsock, list(wp=workerpool(), s=subaddr))		# 2. send workerpool and subscraddrtion address
	disconnect.socket(tmpsock)
	store(workeraddr, workerpool())
}

dispatcher <- function(generatoraddr, selfaddr) {
	addr(selfaddr)
	connect.socket(mainrequester(), generatoraddr)
	send.socket(mainrequester(), get.last.endpoint(mainreplier()))	# 1. send self addr
	msg <- receive.socket(mainrequester())  				# 2. receive workerpool and subscraddrtion addr
	disconnect.socket(mainrequester())
	eapply(msg$wp, store, workerpool())
	connect.socket(mainsubscriber(), msg$s)
	subscribe(s, '')
	dispatch()
}

maincontext <-  local({
	context <- NULL
	function() {
		if (is.null(context)) context <<- init.context()
		context
	}
})

mainsocket <- function(type) {
	socket <- NULL
	function() {
		if (is.null(socket)) {
			socket <<- init.socket(maincontext(), type)
			if (identical(type, "ZMQ_REP") || identical(type, "ZMQ_PUB")) 
				bind.socket(socket, paste0("tcp://", addr(), ":0"))
		}
		socket
	}
}

mainrequester <- mainsocket("ZMQ_REQ")
mainreplier <- mainsocket("ZMQ_REP")
mainpublisher <- mainsocket("ZMQ_PUB")
mainsubscriber <- mainsocket("ZMQ_SUB")
addr <- local({
	addr <- NULL
	function(x) {
		if (!missing(x)) addr <<- x
		else if (is.null(addr)) stop("ip address should be set up w/ addr(x)")
		addr
	}
})
