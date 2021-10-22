Chunk <- function(x, ...) UseMethod("Chunk")
Chunk.Identifier <- function(x, generator, ...) {
	stopifnot(is.Identifier(generator) || is.null(generator))
	chunk <- list(identifier=x, generator=generator)
	structure(chunk, class=c("Chunk", class(chunk)))
}
is.Chunk <- function(chunk) inherits(chunk, "Chunk")
Identifier.Chunk <- function(x, ...) x$identifier
generator.Chunk <- function(x, ...) x$generator
format.Chunk <- function(x, ...) c(format(identity(x)), format(generator(x)))
print.Chunk <- function(x, ...) cat("Chunk:", format(x), "\n")

Chunks <- function(...) {
	chunks <- list(...)
	stopifnot(sapply(chunks, is.Chunk))
	structure(chunks, class=c("Chunks", class(chunks)))
}

Data <- function(chunk, value) {
	stopifnot(is.Chunk(chunk))
	data <- list(chunk=chunk, value=value)
	structure(data, class=c("Data", class(data)))
}
is.Data <- function(data) inherits(data, "Data")
Chunk.Data <- function(x, ...) x$chunk
value.Data <- function(x, ...) x$value
Identifier.Data <- function(x, ...) Identifier(Chunk(x))
generator.Data <- function(x, ...) generator(Chunk(x))
print.Data <- function(x, ...) {
	cat("Data: value:", capture.output(str(value(x))))
	NextMethod()
}

value <- function(x, ...) UseMethod("value")
Computation <- function(data, input, output) {
	stopifnot(is.Chunks(input),
	          is.Data(data),
	          is.null(generator(data)),
		  is.function(value(data)),
		  is.Identifier(output))
	computation <- list(data=data, input=input, output=output)
	structure(computation, class=c("Computation", class(computation)))
}
is.Computation <- function(computation) inherits(computation, "Computation")
Data.Computation <- function(x, ...) x$data
output.Computation <- function(x, ...) x$output
Identifier.Computation <- function(x, ...) Identifier(Data(x, ...))
input.Computation <- function(x, ...) x$input
value.Computation <- function(x, ...) value(Data(x, ...))
generator.Computation <- function(x, ...) NULL
print.Computation <- function(x, ...) {
	cat("Computation: output: ", capture.output(print(output(x))))
	NextMethod()
}

IdentifiedEventuals <- function(x, ...) {
	if (missing(x)) {
		IdentifiedEventuals.Identifier()
	} else UseMethod("IdentifiedEventuals")
}
IdentifiedEventuals.Identifier <- function(x, eventual) {
	stopifnot(missing(eventual) || is.promise(eventual))
	identified_eventuals <- if (missing(x)) {
		AssociativeArray()
	} else AssociativeArray(x, eventual)
	structure(identified_eventuals,
		  class=c("IdentifiedEventuals", class(identified_eventuals)))
}
is.IdentifiedEventuals <- function(x) inherits(x, "IdentifiedEventuals")

distributed.do.call <- function(what, args, quote = FALSE, envir = parent.frame) {
	# convert non-distributed args to Data and PUT, POST computation, return chunk
}
