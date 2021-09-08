data.identifier <- function(id, computation) {
	stopifnot(is.identity(computation),
		  is.identity(id))
	structure(list(comp=comp, id=id), class="data")
}
identifier.data <- function(data) data$id
computation.data <- function(data) data$comp
value.data <- function(data) emerge(identifier(data))

is.data <- function(data) inherits(data, "data")

format.data <- function(data) {
	as.character(c(underline("Data", '='),
		       underline("Value:", '-'),
		       with.spacing(capture.output(str(value(data)))),
		       underline("Computation Identifier:", '-'),
		       with.spacing(format(computationid(data))),
		       underline("Identifier:", '-'),
		       with.spacing(format(identifier(data)))))
}
print.data <- function(data) {
	with.comment(cat(format(data), sep="\n"))
}
str.data <- function(data) cat("Data: ", str(identifier(data)))

fixedData <- function(id, computation, value) {
	d <- data(id, computation)
	class(d) <- c("fixedData", class(d))
	value(d) <- value
	d
}
value.fixedData <- function(data) data$val
`value<-.fixedData` <- function(x, value) {x$val <- value; x}

do.computation <- function(comp) {
	store(comp)
	input <- input(comp)
	input[sapply(input, is.identifier)] <- value(input[sapply(input, is.identifier)])
	data <- fixedData(id=output(comp),
			  computation=comp,
			  value=tryCatch(do.call(value(comp), input),
					 error=identity))
	store(data)
}
do.function <- function(fun, input) {
	comp <- computation(fun=fun,
			    input=input,
			    id=genid(),
			    output=genid())
	request(comp)
	data(id=output(comp), computation=id(comp))
}
