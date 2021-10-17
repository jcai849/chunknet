Repository <- function(index, identified_eventuals) {
	stopifnot(is.Index(index),
		  is.IdentifiedEventuals(identified_eventuals))
	repository <- list(index=index, identified_eventuals=identified_eventuals)
	structure(repository, class=c("Repository", class(repository)))
}
is.Repository <- function(x) inherits(x, "Repository")
Index.Repository <- function(x) x$index
IdentifiedEventuals.Repository <- function(x) x$identified_eventuals

AssociativeArray <- function(key, value) {
	stopifnot(is.missing(key) || is.character(key))
	table <- new.env(parent=emptyenv())
	if (!is.missing(key)) assign(key, value, envir=table)
	structure(table, class=c("AssociativeArray", class(table)))
}

is.AssociativeArray <- function(x) inherits(x, "AssociativeArray")
`|.AssociativeArray` <- merge.AssociativeArray <- function(x, y, ...) {
	stopifnot(is.AssociativeArray(x),
		  is.AssociativeArray(y))
	 y_keys <- ls(y)
	 x_keys <- ls(x)
	 y_values <- mget(y_keys, y)
	 x_values <- mget(x_keys, x)
	 intersections <- mapply(c,
				 x_values[intersect(x_keys, y_keys)],
				 y_values[intersect(x_keys, y_keys)])
	 uniques <- c(x_values[setdiff(x_keys, y_keys)],
		      y_values[setdiff(y_keys, x_keys)])
	 keys <- c(names(intersections), names(uniques))
	 values <- c(intersections, uniques)
	 table <- AssociativeArray()
	 mapply(assign, keys, values, MoreArge=list(envir=table))
	 table
}
keys.AssociativeArray <- ls
