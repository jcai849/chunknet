spawn <- function(initiatee, self, other) {
	stopifnot(is.Location(host),
		  is.Location(self),
		  is.missing(other) || is.Location(other))
	synchronizer <- Replier(Location(host(self), port=0L))
	summon <- as.call(c(alist(largerscale::service),
			    list(self=initiatee,
				 initiator=synchronizer),
			    if (!missing(other)) list(other=other) else NULL))
	system2("ssh", shQuote(shQuote(c(host, "R", "-e", deparse1(summon)))))
	message <- receive.socket(synchronizer)
	send.socket(synchronizer, NULL)
	message
}

# "Location Service" is one without any other. Use the "Location Service" as other.
service <- function(self, initiator, other) {
	stopifnot(is.Location(self),
		  is.missing(initiator) || is.Location(initiator),
		  is.missing(other) || is.Location(other))
	replier <- Replier(self)
	if (!missing(initiator)) notify(initiator, replier)
	associations <- if (!missing(other)) {
		notify(other, replier)
		associate("Locations", other)
	} else AssociativeArray()
	process(replier, associations)
}

notify <- function(location, replier) {
	stopifnot(is.Location(location),
		  is.Replier(replier))
	requester <- Requester(location)
	send.socket(requester, as.Location(replier))
	message <- receive.socket(requester)
	disconnect.socket(requester, as.character(location))
	message
}
