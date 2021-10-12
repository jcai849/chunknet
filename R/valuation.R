set_sticky_value <- function(type_test) {
	value <- NULL
	function(set) {
		stopifnot(type_test(set))
		if (!missing(set)) value <<- set; value
	}
}
HOSTNAME <- set_sticky_value(is.character)
LOCATION_SERVICE <- set_sticky_value(is.Location)
initialise_sticky_value <- function(initialisation) {
	value <- NULL
	function() {
		if (is.null(value)) value <<- initialisation(); value
	}
}
CONTEXT <- initialise_sticky_value(init.context)
initialise_sticky_communicator <- function(SOCK_TYPE) {
	binder_initialisation <- function() {
		replier <- init.socket(CONTEXT(), SOCK_TYPE)
		bind.socket(replier, paste0("tcp://", HOSTNAME(), ":0"))
		replier
	}
	initialise_sticky_value(binder_initialisation)
}
REPLIER <- initialise_sticky_communicator("ZMQ_REP")
