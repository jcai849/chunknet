process_loop <- function(communicator, repository) {
	stopifnot(is.Communicator(communicator),
		  is.Repository(repository))
	repeat {
		to_read <- listen(Listener(communicator), timeout=-1L)
		requests <- lapply(Listener(communicator)[to_read], function(x) {read(x)})
		repository <- Reduce(function(repository, request)
					    process(request, repository, communicator),
				    requests,
				    init=repository)
	}
}

process <- function(request, repository, communicator, ...) UseMethod("process")

process.GET_Request <- function(request, repository, communicator, ...)
	process_GET_Request(value(request), what(request), return_address(request),
			    repository, communicator, ...)
process.POST_Request <- function(request, repository, communicator, ...)
	process_POST_Request(value(request), repository, communicator, ...)
process.PUT_Request <- function(request, repository, communicator, ...)
	process_PUT_Request(value(request), repository, communicator, ...)

process_GET_Request <- function(identifier, what, return_address, repository, communicator, ...)
	UseMethod("process_GET_Request")
process_POST_Request <- function(value, repository, communicator, ...)
	UseMethod("process_POST_Request")
process_PUT_Request <- function(value, repository, communicator, ...)
	UseMethod("process_PUT_Request")

process_POST_Request.Index <- function(value, repository, communicator, ...) {
	POST(Replier(communicator), Index(repository))
        merge(value, repository)
}
process_PUT_Request.Chunk <- function(value, repository, communicator, ...) {
	POST(Replier(communicator), TRUE)
	new_identified_eventual <- IdentifiedEventuals(Identifier(value),
	                                                promise_resolve(value))
	merge(new_identified_eventual, repository)
}
process_GET_Request.Identifier <- function(identifier, what, return_address, repository, communicator, ...) {
    eventual <- switch(what,
                       value=get(identifier, envir=IdentifiedEventuals(repository)),
                       location=get(identifier, envir=IdentifiedLocations(repository)))
    force(return_address)
    callback <- function(value) {
        # oplan <- plan("multicore")
        # on.exit(plan(oplan), add = TRUE)
        # future_promise({
            context <- rzmq::init.context()
            requester <- rzmq::init.socket(context, "ZMQ_REQ")
            rzmq::connect.socket(requester, return_address)
            rzmq::send.socket(requester, value)
            value
       #  },
       # globals=list(return_address=return_address))
    }
    if (what=="value"){
        new_promise <- then(eventual, onFulfilled = callback)
        browser()
        merge(IdentifiedEventuals(identifier, new_promise), repository,
              conflict_fun=function(x, y) x)
    } else repository
}

### OLD
process.list <- function(x, associations) {
	identifier <- x$identifier
	location <- x$location
	stopifnot(is.Identifier(identifier),
		  is.Location(location))
	value <- if (exists(identifier, associations))
		associations[[identifier]] else associations[["Locations"]]
	association(x, value %...>% future_promise(send(., to=location)))
}
process.Location <- function(x, associations) {
	association("Locations",
		    associations[["Locations"]] %...>%
			future_promise(function(locations) c(locations, list(x))))
}
process.Computation <- function(x, associations) {
	available <- sapply(input(message), Identifier) %in% associations
	associations_with_all_input <- if (!all(available)) {
		unavailable_input <- input(message)[!available]
		promised_input <- Reduce(merge,
					 mapply(association,
						lapply(unavailable_input, Identifier),
						lapply(unavailable_input, function(x) future_promise(emerge(x)))))
		merge(promised_input, associations)
	} else associations
	input_identifiers <- sapply(Identifier, input(message))
	input <- associations_with_all_input[input_identifiers]
	promised_output <- promise_all(input) %...>%
		future_promise(run(message))
	saved_value <- association(Identifier(message), promise_resolved(message))
	saved_output <- association(output(message), promised_output)
	result <- merge(saved_value, saved_output)
	if (!all(available)) merge(result, saved_input) else result
}
process.AssociativeArray <- function(x, associations) {
	Reduce(merge, mapply(association,
			     keys(x),
			     lapply(keys(x), function(k) resolved_promise(x[[k]]))))
}

respond <- function(message, respond_to, result) UseMethod("respond")
respond.Identifier <- function(message, respond_to, result) {
	send.socket(respond_to, result)
	invisible()
}
respond.Location <- respond.Computation <- respond.AssociativeArray <-
	function(message, respond_to, result) {
		send.socket(respond_to, NULL)
		result
}
