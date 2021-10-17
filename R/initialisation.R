spawn <- function(initiatee, self, other) {
	stopifnot(is.Node(initiatee),
		  is.ReplierLocation(self),
		  is.missing(other) || is.ReplierLocation(other))
	synchronizer <- Replier(Location(host(self), port=0L))
	summon <- as.call(c(alist(largerscale::service),
			    list(self=initiatee,
				 initiator=as.Location(synchronizer)),
			    if (!missing(other)) list(other=other) else NULL,
			    list(live=TRUE)))
	system2("ssh", shQuote(shQuote(c(host, "R", "-e", deparse1(summon)))))
	initiatee_node <- receive.socket(synchronizer)
	POST(synchronizer, Index())
	initiatee_node
}

service <- function(self, initiator, other, live=TRUE) {
	stopifnot(is.Node(self),
		  is.missing(initiator) || is.ReplierLocation(initiator),
		  is.missing(other) || is.ReplierLocation(other))
	communicator <- Communicator(self)
	if (!missing(initiator)) notify(communicator, initiator)
	index <- if (!missing(other)) {
		index <- notify(communicator, other)
		index
	} else Index()
	for (publisher in Publisher(index))
		connect.socket(Subscriber(communicator), subscription)
	repository <- Repository(index, Eventuals())
	if (live)
		process_loop(communicator, repository)
}

notify <- function(communicator, location) {
	stopifnot(is.Communicator(communicator),
		  is.ReplierLocation(location))
	requester <- Requester(communicator)
	connect.socket(requester, as.character(location))
	POST(endpoint=requester, payload=Node(communicator))
	index <- payload(receive.socket(requester))
}
