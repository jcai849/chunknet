process <- function(x, associations) {
	stopifnot(is.AssociativeArray(associations))
	UseMethod("process")
}

process.Endpoint <- function(x, associations) {
	repeat {
		message <- receive(x)
		result <- respond(message, x, process(x=message, associations))
		associations <- merge(associations, result)
	}
}
process.Identifier <- function(x, associations) {
	if (exists(x, associations)) associations[x] else
		associations["Locations"]
}
process.Location <- function(x, associations) {
	associate("Locations", location)
}
process.Computation <- function(x, associations) {
	available <- sapply(input(message), Identifier) %in% associations
	associations_with_all_input <- if (!all(available)) {
		unavailable_input <- input(message)[!available]
		promised_input <- associate(lapply(unavailable_input, Identifier),
					    lapply(unavailable_input, function(x)
						   promise_future(emerge(x))))
		merge(promised_input, associations)
	} else associations
	input_identifiers <- sapply(Identifier, input(message))
	input <- associations_with_all_input[input_identifiers]
	promised_output <- promise_all(input) %...>%
		promise_future(run(message))
	saved_value <- associate(Identifier(message), promise_resolved(message))
	saved_output <- associate(output(message), promised_output)
	result <- merge(saved_value, saved_output)
	if (!all(available)) merge(result, saved_input) else result
}
process.AssociativeArray <- function(x, associations)  x 
process.NULL <- function(x, associations) associations

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
