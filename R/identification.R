Identifier <- function(x, ...) {
	if (missing(x)) Identifier(1L) else UseMethod("Identifier")
}
Identifier.numeric <- function(x, ...) Identifier(as.integer(x))
Identifier.integer <- function(x, ...) Identifier(UUIDgenerate(n=x))
Identifier.character <- function(x, ...) structure(x, class="Identifier")
is.Identifier <- function(x) inherits(x, "Identifier")

format.Identifier <- function(x, ...) format(as.character(x))
print.Identifier <- str.Identifier <- function(id, ...)
	cat("Identifier: ", format(id), "\n")
