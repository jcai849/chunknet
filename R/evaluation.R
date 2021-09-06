do <- function(comp) {
	stopifnot(is.computation(comp))
	what <- if (is.identifier(input(comp))) value(input(comp)) else input(comp)
	data <- structure(list(val=tryCatch(do.call(fun(comp), list(what)),
					    error=identity),
			       id=identifier("Data", key(identifier(comp)))),
			  class="data")
	store(data)
}
format.data <- function(data) {
	as.character(c(underline("Data", '='),
		       underline("Value:", '-'),
		       with.spacing(capture.output(str(value(data)))),
		       underline("Identifier:", '-'),
		       with.spacing(format(identifier(data)))))
}
print.data <- function(data) {
	with.comment(cat(format(data), sep="\n"))
}
value.data <- function(data) data$val
identifier.data <- function(data) data$id
is.data <- function(data) inherits(data, "data")
