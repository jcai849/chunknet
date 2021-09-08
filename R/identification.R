identifier.character <- function(key) structure(key, class="identifier")
format.identifier <- function(id) {
	as.character(c(underline("Identifier", '='),
		       underline("Key:", '-'),
		       with.spacing(key(id))))
}
print.identifier <- function(id) {
	with.comment(cat(format(id), sep="\n"))
}
str.identifier <- function(id) cat("ID: ", format(key(id)), "\n")
key.identifier <- function(id) id$key
is.identifier <- function(id) inherits(id, "identifier")
emerge.identifier <- function(id) get(id, datapool(), inherits=FALSE)
value.identifier <- function(id) value(data(id))
