process_loop <- function(communicator, repository) {
	stopifnot(is.Communicator(communicator),
		  is.Repository(repository))
	repeat {
		to_read <- listen(communicator, timeout=-1L)
		messages <- lapply(communicator[to_read], read)
		repository <- Reduce(function(repository, message)
					    process(message, repository, communicator),
				    messages,
				    init=repository)
	}
}

process <- function(x, associations) {
	stopifnot(is.AssociativeArray(associations))
	UseMethod("process")
}
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
