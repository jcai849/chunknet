identifier <- function(x, ...) UseMethod("Identifier")
identifier.numeric <- function(n, ...) identifier(as.integer(n))
identifier.integer <- function(n, ...) identifier(UUIDgenerate(n=n))
identifier.character <- function(key, ...) structure(key, class="Identifier")
is.Identifier <- function(id) inherits(id, "Identifier")

format.Identifier <- function(id, ...) c("Identifier", as.character(id))
print.Identifier <- str.Identifier <- function(id, ...)  cat(format(id), "\n")
