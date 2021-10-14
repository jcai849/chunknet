Identifier <- function(x, ...) UseMethod("Identifier")
Identifier.numeric <- function(n, ...) identifier(as.integer(n))
Identifier.integer <- function(n, ...) identifier(UUIDgenerate(n=n))
Identifier.character <- function(key, ...) structure(key, class="Identifier")
is.Identifier <- function(id) inherits(id, "Identifier")

format.Identifier <- function(id, ...) c("Identifier", as.character(id))
print.Identifier <- str.Identifier <- function(id, ...)  cat(format(id), "\n")
