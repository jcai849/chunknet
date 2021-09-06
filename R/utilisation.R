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
