Payload <- function(x, ...) UseMethod("Payload")
Payload.default <- function(x, ...) {
	payload <- list(value=x)
	structure(payload, class=c("Payload", class(payload)))
}
is.Payload <- function(x) inherits(x, "Payload")
value.Payload <- function(x, ...) x$value
print.Payload <- function(x, ...)
	cat("Payload Value:", paste0(' ', capture.output(str(value(x)))),
	    sep="\n")

GET_Payload <- function(x, what, return_address, ...) {
	stopifnot(is.character(what),
	          is.character(return_address))
	get_payload <- list(value=x, what=what, return_address=return_address)
	structure(get_payload, class=c("GET_Payload", "Payload", class(get_payload)))
}
what <- function(x, ...) UseMethod("what")
what.GET_Payload <- function(x, ...) x$what
return_address <- function(x, ...) UseMethod("return_address")
return_address.GET_Payload <- function(x, ...) x$return_address

Header <- function(x, ...) UseMethod("Header")
Header.character <- function(x, ...) {
	structure(x, class="Header")
}
is.Header <- function(x) inherits(x, "Header")
print.Header <- function(x, ...) {
	cat("Header: ", format(x))
}

Request <- function(header, payload) {
		stopifnot(is.Header(header), is.Payload(payload))
		request <- list(header=header, payload=payload)
		structure(request, class=c(paste0(header, "_Request"), "Request", class(request)))
}
is.Request <- function(x) inherits(x, "Request")
Header.Request <- function(x, ...) x$header
Payload.Request <- function(x, ...) x$payload
print.Request <- function(x, ...)
	cat("Request: ",
	    paste0(' ', capture.output(print(Header(x)))),
	    paste0(' ', capture.output(print(Payload(x)))))
value.Request <- function(x, ...) value(Payload(x))

GET_Request <- function(x, what, return_address) {
	stopifnot(is.Identifier(x))
	Request(Header("GET"), GET_Payload(x, what, return_address))
}
what.GET_Request <- function(x, ...) what(Payload(x))
return_address.GET_Request <- function(x, ...) return_address(Payload(x))
POST_Request <- function(x) {
	Request(Header("POST"), Payload(x))
}
PUT_Request <- function(x) {
	stopifnot(is.Chunk(x))
	Request(Header("PUT"), Payload(x))
}

POST <- function(endpoint, payload) {
	send.socket(endpoint, POST_Request(payload))
}
GET <- function(endpoint, identifier, what, return_address) {
	send.socket(endpoint, GET_Request(identifier, what, return_address))
}
PUT <- function(endpoint, payload) {
	send.socket(endpoint, PUT_Request(payload))
}
