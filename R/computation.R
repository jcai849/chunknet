computation <- function(fun, input, id, ...) {
	stopifnot(is.function(fun),
		  if (missing(id)) TRUE else is.identifier(id))
	structure(list(fun=fun,
		       input=if (is.data(input) || is.computation(input))
			       identifier(input) else input,
		       id=if (missing(id)) NULL else id,
		       addl=if (missing(...)) NULL else list(...)),
		  class="computation")
}
format.computation <- function(comp) {
	as.character(c(underline("Computation", '='),
		       underline("Function:", '-'),
		       with.spacing(capture.output(str(fun(comp)))),
		       underline("Input :", '-'),
		       if (is.identifier(input(comp))) {
			       with.spacing(format(input(comp)))
		       } else {
			       with.spacing(capture.output(str(input(comp))))
		       },
		       if (is.null(identifier(comp))) {
			       NULL
		       } else c(underline("Output Identifier:", '-'),
				with.spacing(format(identifier(comp)))),
		       if (is.null(addl(comp))) {
			       NULL
		       } else c(underline("Additional Input Identifiers:", '-'),
				with.spacing(sapply(sapply(addl(comp), identifier),
						    format)))))
}
print.computation <- function(comp) {
	with.comment(cat(format(comp), sep="\n"))
}
fun.computation <- function(comp) comp$fun
input.computation <- function(comp) comp$input
identifier.computation <- function(comp) comp$id
addl.computation <- function(comp) comp$addl
is.computation <- function(comp) inherits(comp, "computation")

request.computation <- function(comp, wait) {
	# send request to remote process, return computation
	comp
}
data.computation <- function(comp) {
	data(identifier(comp))
}
value.computation <- function(comp) {
	value(identifier(comp))
}
