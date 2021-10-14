Data <- function(identity, generator) {
	# generator is Identifier for generating computation if derived from
	# some computation, but the raw data itself otherwise
	stopifnot(is.Identifier(identity))
	data <- list(generator=generator, identity=identity)
	class(data) <- c("Data", class(data))
	data
}
is.Data <- function(data) inherits(data, "Data")
identity.Data <- function(x) x$identity
generator.Data <- function(x, ...) x$generator
format.Data <- function(x, ...) c(format(identity(x)),
				  format(generator(x)))
print.Data <- function(x, ...) cat("Data:", format(x), "\n")

Computation <- function(identity, input, value, output) {
	stopifnot(is.Identifier(identity),
		  is.list(input), sapply(input, is.Data),
		  is.function(value),
		  is.Identifier(output))
	computation <- structure(list(identity=identity, input=input,
				      value=value, output=output))
	class(computation) <- c("Computation", class(computation))
	computation
}
is.Computation <- function(computation) inherits(computation, "Computation")
identity.Computation <- function(x) x$identity
input.Computation <- function(x, ...) x$input
output.Computation <- function(x) x$output
value.Computation <- function(x) x$value
format.Computation <- function(computation, ...)
	c(format(identity(computation)), format(input(computation)),
	  format(value(computation)), format(output(computation)))
print.Computation <- function(computation, ...)
	cat("Computation:", format(computation), "\n")

distributed.do.call <- function(what, args, quote = FALSE, envir = parent.frame) {
	# convert non-distributed args to Data first
	comp <- Computation(identity=identity(), input=args,
			    value=what, output=identity())
	send(comp)
	data(identity=output(comp), input=identity(comp))
}
