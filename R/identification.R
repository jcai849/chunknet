identifier.integer <- function(n) identifier(substr(runif(1), 3, 3+n-1))
identifier.character <- function(key) structure(key, class="identifier")
key.identifier <- function(id) unclass(id)

is.identifier <- function(id) inherits(id, "identifier")

format.identifier <- function(id, ...) c("Identifier", key(id))
print.identifier <- function(id, ...)  cat(format(id), "\n")
str.identifier <- function(id, ...) cat("Identifier: ", format(key(id)), "\n")

emerge.identifier <- function(id) get(id, datapool(), inherits=FALSE)
value.identifier <- function(id) value(emerge(id))
