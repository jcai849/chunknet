pool <- function(name) local({
	pool <- structure(new.env(), class=c(name, "pool"))
	function(x, ...) if (missing(x)) pool else UseMethod(name)
})
format.pool <- function(dp, ...)
	c("Pool:", length(dp),
	  paste0("Item", if (length(dp) > 1) 's' else NULL))
print.pool <- function(dp, ...) cat(format(dp), "\n")
str.pool <- function(dp, ...) {
	print(dp)
	cat(capture.output(ls.str(envir=dp)), sep="\n")
}
store.pool <- function(x, pool, idfun) assign(idfun(x), x, pool)
unstore.pool <- function(x, pool, idfun) rm(list=idfun(x), envir=pool)

workerpool <- local({ wp <- pool("workerpool"); function() wp })
store.workerpool <- function(x, pool, idfun) NextMethod(,,idfun=identity)
unstore.workerpool <- function(x, pool, idfun) NextMethod(,,idfun=identity)

store <- function(x, pool, idfun) UseMethod("store", pool)
unstore <- function(x, pool, idfun) UseMethod("unstore", pool)
