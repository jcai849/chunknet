library(largerscale)
library(rzmq)

location_request_reply <- Location("127.0.0.1", 4444L)

replier <- Replier(location_request_reply)
stopifnot(is.Replier(replier), !is.Replier(NULL))
stopifnot(is.Endpoint(replier), !is.Endpoint(NULL))
print(replier)

requester <- Requester(location_request_reply)
stopifnot(is.Requester(requester), !is.Requester(NULL))
stopifnot(is.Endpoint(requester))
print(requester)

send.socket(requester, TRUE)
stopifnot(receive.socket(replier, dont.wait=TRUE))

location_publish_subscribe <- Location("127.0.0.1", 55555L)

publisher <- Publisher(location_publish_subscribe)
stopifnot(is.Publisher(publisher), !is.Publisher(NULL))
stopifnot(is.Endpoint(publisher))

subscriber <- Subscriber(location_publish_subscribe)
stopifnot(is.Subscriber(subscriber), !is.Subscriber(NULL))
stopifnot(is.Endpoint(subscriber))

send.socket(publisher, TRUE)
stopifnot(receive.socket(subscriber, dont.wait=TRUE))
