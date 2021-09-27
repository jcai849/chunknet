data.identifier <- function(id, computation) {
	stopifnot(is.identifier(computation),
		  is.identifier(id))
	structure(list(comp=computation, id=id), class="data")
}
identifier.data <- function(data) data$id
computation.data <- function(data) data$comp
value.data <- function(data, autorecover=TRUE) {
	if (!lost(data)) {
		value(identifier(data))
	} else {
		if (autorecover) recover(data)
		stop("Data lost. Recovering...")
	}
}

is.data <- function(data) inherits(data, "data")

format.data <- function(data, ...) c("Data", format(identifier(data)))
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
	strfields(fd, 
		  "Identifier", identifier,
		  "Value", value,
		  "Computation", computation)
}

regulariseinput <- function(comp, continuation) {
    x <- input(comp)
    asis <- sapply(x, is.AsIs)
    if (length(x) && !all(asis))
        x[!asis] <- tryCatch(lapply(x[!asis], value),
                             error=function(e) continuation(e))
    if (length(x) && any(asis))
        x[asis] <- lapply(x[asis], un.AsIs)
    x
}
do.computation <- function(comp, continuation=stop) {
    if (!is.null(identifier(comp))) store(comp)
    input <- regulariseinput(comp, continuation)
    val <- tryCatch(do.call(value(comp), input),
                    error=identity)
    if (!is.null(output(comp)))
        store(fixedData(id=output(comp),
                        computation=identifier(comp),
                        value=val))
    val
}
do.function <- function(fun, input) {
    comp <- computation(fun=fun,
                        input=input)
    send(comp)
    data(id=output(comp), computation=identifier(comp))
}
work <- function(port) {
	while (T) {
		r <- receive(port)
		callCC(function(k) do(r, k))
	}
}
