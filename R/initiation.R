spawn <- function(initiatee, self, other) {
	stopifnot(is.Location(host),
		  is.Location(self),
		  is.missing(other) || is.Location(other))
	synchronizer <- Replier(Location(host(self), port=0L))
	summon <- as.call(c(alist(largerscale::service),
			    list(self_reply=initiatee,
				 self_publish=initiatee,
				 initiator=synchronizer),
			    if (!missing(other)) list(other=other) else NULL,
			    list(live=TRUE)))
	system2("ssh", shQuote(shQuote(c(host, "R", "-e", deparse1(summon)))))
	replier_message <- receive.socket(synchronizer)
	send.socket(synchronizer, TRUE)
	publisher_message <- receive.socket(synchronizer)
	send.socket(synchronizer, TRUE)
	location_message <- receive.socket(synchronizer)
	send.socket(synchronizer, TRUE)
	replier_message
}

service <- function(self_reply, self_publish, initiator, other, live=TRUE) {
	stopifnot(is.Location(self_reply),
		  is.Location(self_publish),
		  is.missing(initiator) || is.Location(initiator),
		  is.missing(other) || is.Location(other))
	replier <- Replier(self_reply)
	publisher <- Publisher(self_publish)
	if (!missing(initiator)) notify(initiator, replier, publisher)
	if (!missing(other)) {
		notifications <- notify(other, replier, publisher)
		nodes <- nodes(notifications)
		subscriptions <- subscriptions(notifications)
		chunk_locations <- chunk_locations(notifications)
	} else {
		nodes <- list()
		subscriptions <- list()
		chunk_locations <- AssociativeArray()
	}
	chunk_values <- AssociativeArray()
	subscriber <- Subscriber()
	for (subscription in subscriptions) 
		connect.socket(subscriber, subscription)
	knowledge <- Knowledge(nodes, subscriptions,
			       chunk_locations, chunk_values)
	if (live)
		process_loop(replier, publisher, subscriber, knowledge)
}

notify <- function(location, replier, publisher) {
	stopifnot(is.Location(location),
		  is.Replier(replier),
		  is.Publisher(publisher))
	requester <- Requester(location)
	send.socket(requester, POST(as.Location(replier)))
	replier_message <- receive.socket(requester)
	send.socket(requester, POST(as.Location(publisher)))
	publisher_message <- receive.socket(requester)
	send.socket(requester, GET("location", '*'))
	chunk_message <- receive.socket(requester)
	disconnect.socket(requester, as.character(location))
	message <- list(nodes=replier_message,
			subscriptions=publisher_message,
			chunk_locations=chunk_message)
	structure(message, class=c("Notifications", class(message)))
}

nodes <- function(x) x$nodes
subscriptions <- function(x) x$subscriptions
chunk_locations <- function(x) x$chunk_locations
