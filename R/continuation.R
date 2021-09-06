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

store.data <- function(data) {
	assign(key(identifier(data)), data, datapool())
}
