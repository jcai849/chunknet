computation.function <- function(fun, id, input, output, ...) {
	stopifnot(is.identifier(id),
		  if (missing(output)) TRUE else is.identifier(output))
	structure(list(id=id,
		       input=if (!is.list(input))  list(input) else input,
		       val=fun,
		       output=if (missing(output)) NULL else output),
		  class="computation")
}
value.computation <- function(comp) comp$val
input.computation <- function(comp) comp$input
identifier.computation <- function(comp) comp$id
output.computation <- function(comp) comp$output

is.computation <- function(comp) inherits(comp, "computation")

format.computation <- function(comp) {
	as.character(c(underline("Computation", '='),
		       underline("Identifier :", '-'),
		       with.spacing(format(identifier(comp))),
		       underline("Value:", '-'),
		       with.spacing(capture.output(str(fun(comp)))),
		       underline("Input:", '-'),
		       lapply(input(comp),
			      function(i) with.spacing(
			       if (is.computation(i) || is.data(i))
			       format(i) else capture.output(str(i)))),
		       underline("Output:", '-'),
		       with.spacing(format(output(comp)))))
}
print.computation <- function(comp) cat(format(comp), sep="\n")
str.computation <- function(comp) cat("Computation: ", str(identifier(comp)))

request.computation <- function(comp, wait) {
	# send request to remote process, return reference to the data
	send(comp, to=l)
}
