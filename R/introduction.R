read.hdfs <- function(path) {
	comp <- computation(function(path) data.frame(y=1:100, x=1:100),
			    input=path)
	send(comp)
	data(id=output(comp), computation=identifier(comp))
}
