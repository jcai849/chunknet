spawn <- function(host, location_service=FALSE ) {
	context <- init.context()
	synchronizer <- init.socket(context, "ZMQ_REP")
	bind.socket(synchronizer, as.character(Location(HOSTNAME(), 0L)))
	summon <- as.call(c(alist(largerscale::service),
			    list(host=host,
				 initiator=Location(synchronizer)),
			    if (!location_service) {
				    list(location_service=LOCATION_SERVICE())
			    } else NULL))
	system2("ssh", shQuote(shQuote(c(host, "R", "-e", deparse1(summon)))))
	message <- receive.socket(synchronizer)
	send.socket(synchronizer, NULL)
	message
}

service <- function(host, location, ...) {
	HOSTNAME(host)
	REPLIER()
	lapply(list(...), notify, replier)
	identifier_table <- if (missing(location)) IdentifierTable() else
		IdentifierTable("Locations", location)
	process(replier, identifier_table)
}

notify <- function(location, replier) {
	stopifnot(is.Location(location))

	context <- init.context()
	requester <- init.socket(context, "ZMQ_REQ")
	connect.socket(requester, as.character(location))
	send.socket(requester, Location(replier))
	message <- receive.socket(requester)
	disconnect.socket(requester, initiator_address)
	message
}
