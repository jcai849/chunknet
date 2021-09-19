recover.data <- function(x) {
	send(computation(do, input=list(I(emerge(computation(x)))), output=NULL))
	x
}
lost.data <- function(x) {
	!exists(identifier(x), datapool(), inherits=FALSE)
}
