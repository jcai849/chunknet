Identifier <- function(x, ...) UseMethod("Identifier")
Identifier.numeric <- function(n, ...) Identifier(as.integer(n))
Identifier.integer <- function(n, ...) Identifier(UUIDgenerate(n=n))
Identifier.character <- function(key, ...) structure(key, class="Identifier")
key.Identifier <- function(id) unclass(id)

is.Identifier <- function(id) inherits(id, "Identifier")

format.Identifier <- function(id, ...) c("Identifier", key(id))
print.Identifier <- function(id, ...)  cat(format(id), "\n")
str.Identifier <- function(id, ...) cat("Identifier: ", format(key(id)), "\n")
