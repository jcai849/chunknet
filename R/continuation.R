pool <- function() {
	structure(new.env(),
		  class="datapool")

}
pool <- function(name) {
	local({
		pool <- structure(new.env(), class=c(name, "pool"))
		function(x, ...) if (missing(x)) pool else UseMethod(name)
	})
}
computationpool <- pool("computationPool")
datapool <- pool("dataPool")
format.pool <- function(dp, ...) {
	c("<Pool:", length(dp), "Items>")
}
print.pool <- function(dp, ...) cat(format(dp), "\n")
str.pool <- function(dp, ...) {
	cat("pool: ", length(dp), " Items\n")
	cat(with.spacing(capture.output(ls.str(envir=dp))), sep="\n")
}

store.data <- store.computation <- datapool.computation <- datapool.data <-
	function(data) {
		assign(identifier(data), data, datapool())
	}
send.computation <- computationpool.computation <- function(comp) {
	assign(identifier(comp), comp, computationpool())
}
receive <- function() {
	topop <- ls(computationpool())[1]
	popped <- get(topop, computationpool())
	rm(list=topop, envir=computationpool())
	popped
}
