pool <- function(name) {
	local({
		pool <- structure(new.env(), class=c(name, "pool"))
		function(x, ...) if (missing(x)) pool else UseMethod(name)
	})
}
datapool <- pool("dataPool")
workerpool <- pool("workerPool")
format.pool <- function(dp, ...) {
	c("Pool:", length(dp), "Items")
}
print.pool <- function(dp, ...) cat(format(dp), "\n")
str.pool <- function(dp, ...) {
	print(dp)
	cat(with.spacing(capture.output(ls.str(envir=dp))), sep="\n")
}

store.data <- store.computation <- datapool.computation <- datapool.data <-
	function(data) {
		assign(identifier(data), data, datapool())
	}
unstore.data <- function(data) {
	unstore(identifier(data))
}
unstore.identifier <- function(id) {
	rm(list=id, envir=datapool())
}
