process <- function(x, identifier_table) UseMethod("process")

process.Endpoint <- function(x, identifier_table) {
	repeat {
		message <- receive(x)
		result <- process(x=message, identifier_table)
		respond(message, x, result)
		identifier_table <- identifier_table | result
	}
}
process.Identifier <- function(x, identifier_table) {
	if (exists(x, identifier_table)) identifier_table[x] else
		identifier_table["Locations"]
}
process.Location <- function(x, identifier_table) {
	IdentifierTable("Locations", location)
}
process.Computation <- function(x, identifier_table) {
	available <- sapply(input(message), Identifier) %in% identifier_table
	IdentifierTable(Identifier(message), promise_resolved(message)) |
	if (all(available)) {
		IdentifierTable(Identifier(output(message)),
				promise_future(run(message)))
	} else {
		Reduce(`|`, lapply(input(message)[!avail], function(input)
				   IdentifierTable(Identifier(input),
						   promise_future(emerge(input))))) |
		IdentifierTable(Identifier(output(message)),
				  promise_all(identifier_table[Identifier(input(message)[!avail])]) %...>%
					  promise_future(run(message)))
	}
}
process.IdentifierTable <- function(x, identifier_table)  x 
process.NULL <- function(x, identifier_table) identifier_table

respond <- function(message, respond_to, result) UseMethod("respond")
respond.Identifier <- function(message, respond_to, result) {
	send.socket(respond_to, result)
	result
}
respond.Location <- respond.Computation <- respond.IdentifierTable <-
	function(message, respond_to, result) {
		send.socket(respond_to, NULL)
		result
}
