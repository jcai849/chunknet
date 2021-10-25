Repository <- function(index, identified_eventuals, ...) {
	stopifnot(is.Index(index),
		  is.IdentifiedEventuals(identified_eventuals))
	repository <- list(index=index, identified_eventuals=identified_eventuals)
	structure(repository, class=c("Repository", class(repository)))
}
is.Repository <- function(x) inherits(x, "Repository")
Index.Repository <- function(x, ...) x$index
IdentifiedEventuals.Repository <- function(x, ...) x$identified_eventuals
Nodes.Repository <- function(x, ...) Nodes(Index(x))
IdentifiedLocations.Repository <- function(x, ...) IdentifiedLocations(Index(x))

AssociativeArray <- function(key, value) {
	stopifnot(missing(key) || is.character(key))
	table <- new.env(parent=emptyenv())
	if (!missing(key)) assign(key, value, envir=table)
	structure(table, class=c("AssociativeArray", class(table)))
}

is.AssociativeArray <- function(x) inherits(x, "AssociativeArray")
merge.AssociativeArray <- function(x, y, ...) merge_associative_array(x, y, ...)
merge_associative_array <- function(x, y, ...) UseMethod("merge_associative_array", y)
merge_associative_array.AssociativeArray <- function(x, y, conflict_fun, ...) {
	stopifnot(is.AssociativeArray(x),
		  is.AssociativeArray(y))
	 x_keys <- ls(x)
	 y_keys <- ls(y)
	 x_values <- mget(x_keys, x)
	 y_values <- mget(y_keys, y)
	 intersecting_keys <- intersect(x_keys, y_keys)
	 combined_intersections <-
            if (!length(intersecting_keys)) {
                NULL } else mapply(conflict_fun,
                                   x_values[intersecting_keys],
                                   y_values[intersecting_keys],
                                   SIMPLIFY=FALSE)
	 uniques <- c(x_values[setdiff(x_keys, y_keys)],
		      y_values[setdiff(y_keys, x_keys)])
	 keys <- c(intersecting_keys, names(uniques))
	 values <- c(combined_intersections, uniques)
	 table <- AssociativeArray()
	 if (length(keys)) mapply(assign, keys, values,
	                          MoreArgs=list(envir=table))
	 table
}
keys <- function(x, ...) UseMethod("keys")
keys.AssociativeArray <- function(x, ...) ls(x)
