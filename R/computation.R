computation.function <- function(fun, input, ...) {
	structure(list(id=identifier(),
		       input=if (!inherits(input, "list"))  list(input) else input,
		       val=fun,
		       output=identifier()),
		  class="computation")
}
value.computation <- function(comp) comp$val
input.computation <- function(comp) comp$input
identifier.computation <- function(comp) comp$id
output.computation <- function(comp) comp$output

is.computation <- function(comp) inherits(comp, "computation")

format.computation <- function(comp, ...) {
	c("<Computation", format(identifier(comp)), ">")
}
print.computation <- function(comp, ...) cat(format(comp), "\n")
str.computation <- function(comp, ...) {
	cat("Computation:\n")
	strfields(comp,
		  "Identifier", identifier,
		  "Input", input,
		  "Value", value,
		  "Output", output)
}
