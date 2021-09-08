datapool <- local({
	datapool <- structure(new.env(),
			      class="datapool")
	function() datapool
})
format.datapool <- function(dp) {
	as.character(c(underline("Data Pool", '='),
		       underline("Data:", '-'),
		       with.spacing(unlist(eapply(dp, format)))))
}
print.datapool <- function(dp) {
	with.comment(cat(format(dp), sep='\n'))
}
str.datapool <- function(dp) {
	cat("Datapool: ", length(dp), " Fixed Data Objects \n")
	if (!length(dp)) ls.str(envir=dp)
}

store.data <- function(data) {
	assign(identifier(data), data, datapool())
}
store.computation <- store.data
