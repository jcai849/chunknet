associative_array <- function() {
	table <- new.env(parent=emptyenv())
	class(table) <- c("AssociativeArray", class(table))
}
associate <- function(key, value) {
	stopifnot(is.character(key))
	table <- associative_array()
	mapply(assign, x=key, value=value, Moreargs=list(envir=table))
	table
}
[.AssociativeArray <- function(x, i) get(i, envir=x)
`|.AssociativeArray` <- merge.AssociativeArray <- function(x, y, ...) {
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
	 mapply(associate, keys, values)
 }
