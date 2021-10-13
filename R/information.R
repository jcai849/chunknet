data <- function(identifier, input) {
	# input is identifier for generating computation if derived from some
	# computation, but the raw data itself otherwise
	stopifnot(is.Identifier(identifier))
	data <- list(input=input, identifier=identifier)
	class(data) <- c("Data", class(data))
	data
}
is.Data <- function(data) inherits(data, "Data")
identifier.Data <- function(x) x$identifier
input.Data <- function(x, ...) x$input
format.Data <- function(x, ...) c(format(identifier(x)),
				  format(input(x)))
print.Data <- function(x, ...) cat("Data:", format(x), "\n")

computation <- function(identifier, input, value, output) {
	stopifnot(is.Identifier(identifier),
		  is.list(input), sapply(input, is.Data),
		  is.function(value),
		  is.Identifier(output))
	computation <- structure(list(identifier=identifier, input=input,
				      value=value, output=output))
	class(computation) <- c("Computation", class(computation))
	computation
}
is.Computation <- function(computation) inherits(computation, "Computation")
identifier.Computation <- function(x) x$identifier
input.Computation <- function(x, ...) x$input
output.Computation <- function(x) x$output
value.Computation <- function(x) x$value
format.Computation <- function(computation, ...)
	c(format(identifier(computation)), format(input(computation)),
	  format(value(computation)), format(output(computation)))
print.Computation <- function(computation, ...)
	cat("Computation:", format(computation), "\n")

distributed.do.call <- function(what, args, quote = FALSE, envir = parent.frame) {
	# distribute any non-distributed args through sending AssociativeArray
	# id->value, then save the resulting data and pass them to computation as input
	send(args)
	comp <- computation(identifier=identifier(), input=args,
			    value=what, output=identifier())
	send(comp)
	data(identifier=output(comp), input=identifier(comp))
}
