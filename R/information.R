chunk.identifier <- function(id, computation) {
	stopifnot(is.identifier(computation),
		  is.identifier(id))
	structure(list(comp=computation, id=id), class="Chunk")
}
identifier.Chunk <- function(chunk) chunk$id
computation.Chunk <- function(chunk) chunk$comp
is.Chunk <- function(chunk) inherits(chunk, "Chunk")
format.Chunk <- function(chunk, ...) c("Chunk", format(identifier(chunk)))
print.Chunk <- function(chunk, ...) cat(format(chunk), "\n")

data <- function(id, computation, value) {
	data <- chunk(id, computation)
	class(data) <- c("Data", class(d))
	value(data) <- value
	data
}
value.Data <- function(data) data$val
`value<-.Data` <- function(x, value) {x$val <- value; x}

computation.function <- function(fun, input, id, output, ...) {
	id <- if (missing(id)) identifier() else id
	output <- if (missing(output)) identifier() else output
	structure(list(id=id,
		       input=if (!inherits(input, "list"))  list(input) else input,
		       val=fun,
		       output=output),
		  class="Computation")
}
value.Computation <- function(comp) comp$val
input.Computation <- function(comp) comp$input
identifier.Computation <- function(comp) comp$id
output.Computation <- function(comp) comp$output
is.Computation <- function(comp) inherits(comp, "Computation")
format.Computation <- function(comp, ...) {
	c("Computation", format(identifier(comp)))
}
print.Computation <- function(comp, ...) cat(format(comp), "\n")

do.function <- function(fun, input) {
    comp <- computation(fun=fun, input=input)
    send(comp)
    chunk(id=output(comp), computation=identifier(comp))
}
