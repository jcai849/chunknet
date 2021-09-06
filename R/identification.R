identifier.character <- function(type, key) {
	stopifnot(is.character(key))
	structure(list(type=type, key=key),
		  class="identifier")
}
format.identifier <- function(id) {
	as.character(c(underline("Identifier", '='),
		       underline("Key:", '-'),
		       with.spacing(key(id)),
		       underline("Type:", '-'),
		       with.spacing(type(id))))
}
print.identifier <- function(id) {
	with.comment(cat(format(id), sep="\n"))
}
type.identifier <- function(id) id$type
key.identifier <- function(id) id$key
is.identifier <- function(id) inherits(id, "identifier")
data.identifier <- function(id) get(key(id), datapool(), inherits=FALSE)
value.identifier <- function(id) value(data(id))
