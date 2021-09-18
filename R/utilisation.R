underline <- function(word, char) {
	c(word, paste0(rep(char, nchar(word)), collapse=''))
}
with.comment <- function(x) {
	cat(paste0("# ", capture.output(x)), sep="\n")
	invisible(x)
}
with.spacing <- function(x) {
	paste0("  ", x)
}
strfields <- function(x, ...) {
	i <- 1
	while (i < ...length()) {
		field <- ...elt(i)
		cat(field, ": ", sep="")
		fieldval <- capture.output(str(...elt(i+1)(x)))
		if (length(fieldval) == 1) {
			cat(fieldval, "\n")
		} else {
			cat(with.spacing(fieldval), sep="\n")
		}
		i <- i + 2
	}
	cat()
}
is.AsIs <- function(x) inherits(x, "AsIs")
un.AsIs <- function(x) structure(x, class=class(x)[class(x) != "AsIs"])
