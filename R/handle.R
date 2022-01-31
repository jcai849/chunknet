# handlers
# Event {
#       FD fd
#       list data {
#                  character header
#                  (any) payload
#                 }
# }

handle <- function(event) {
    events <- names(Events)
    event_name <- event$data$header
    log("Handling Event: %s", event_name)
    handler_name <- events[Vectorize(grepl)(glob2rx(events), event_name)]
    handler <- get(handler_name, Events)
    handler(event)
}

non_responding_handle <- function(handler) {
	function(event) {
		handler(event$data$payload)
		orcv::event_complete(event)
	}
}
