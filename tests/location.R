library(largerscale)
library(rzmq)

location_req_rep <- Location("127.0.0.1", 4444L)
stopifnot(is.Location(location_req_rep),
	  !is.Location(NULL))
stopifnot(identical(as.character(location_req_rep),
		    "tcp://127.0.0.1:4444"))
print(location_req_rep)
replier <- Replier(location_req_rep)
replier_location <- as.Location(replier)
stopifnot(identical(unclass(replier_location),
		    unclass(location_req_rep)))
stopifnot(is.ReplierLocation(replier_location))
stopifnot(is.EndpointLocation(replier_location))
requester_location <- as.Location(requester)
stopifnot(identical(unclass(requester_location),
		    unclass(location_req_rep)))
stopifnot(is.RequesterLocation(requester_location))
stopifnot(is.EndpointLocation(requester_location))

location_pub_sub <- Location("127.0.0.1", 55555L)
stopifnot(identical(as.character(location_pub_sub),
		    "tcp://127.0.0.1:55555"))
print(location_pub_sub)
publisher <- Publisher(location_pub_sub)
publisher_location <- as.Location(publisher)
stopifnot(identical(unclass(publisher_location),
		    unclass(location_pub_sub)))
stopifnot(is.PublisherLocation(publisher_location))
stopifnot(is.EndpointLocation(publisher_location))
subscriber <- Subscriber(location_pub_sub)
subscriber_location <- as.Location(subscriber)
stopifnot(identical(unclass(subscriber_location),
		    unclass(location_pub_sub)))
stopifnot(is.SubscriberLocation(subscriber_location))
stopifnot(is.EndpointLocation(subscriber_location))
