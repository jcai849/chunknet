data.identifier <- function(id, computation) {
	stopifnot(is.identifier(computation),
		  is.identifier(id))
	structure(list(comp=computation, id=id), class="data")
}
identifier.data <- function(data) data$id
computation.data <- function(data) data$comp
value.data <- function(data) value(identifier(data))

is.data <- function(data) inherits(data, "data")

format.data <- function(data, ...) c("<Data", format(identifier(data)), ">")
print.data <- function(data, ...) cat(format(data), "\n")
str.data <- function(data, ...) {
	cat("Data:\n")
	strfields(data, 
		  "Identifier", identifier,
		  "Computation Identifier", computation)
}

fixedData <- function(id, computation, value) {
	d <- data(id, computation)
	class(d) <- c("fixedData", class(d))
	value(d) <- value
	d
}
value.fixedData <- function(data) data$val
`value<-.fixedData` <- function(x, value) {x$val <- value; x}

value.default <- identity

str.fixedData <- function(fd, ...) {
	strfields(fd, "Value", value)
	NextMethod()
}

do.computation <- function(comp) {
	store(comp)
	input <- lapply(input(comp), value)
	data <- fixedData(id=output(comp),
			  computation=identifier(comp),
			  value=tryCatch(do.call(value(comp), input),
					 error=identity))
	store(data)
}
do.function <- function(fun, input) {
	comp <- computation(fun=fun,
			    input=input)
	send(comp)
	data(id=output(comp), computation=identifier(comp))
}
