Events <- new.env() # event_name (header) -> handler function

on <- function(event, handler) {
    log("Adding handler for event %s", event)
    assign(event, handler, Events)
}

handle <- function(event) {
    events <- names(Events)
    event_name <- orcv::header(event)
    handler_name <- events[Vectorize(grepl)(glob2rx(events), event_name)]
    handler <- get(handler_name, Events)
    handler(event)
}

non_responding <- function(handler) {
	function(event) {
		handler(event)
		close(event)
	}
}

extract <- function(text, pattern) {
	regmatches(text, regexec(pattern, text))[[1]][-1]
}
