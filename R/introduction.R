read.hdfs <- function(path) {
	comp <- computation(function(path) 1:100,
			    id=identifier("<unique-key-394>"),
			    input=path,
			    output=identifier("<unique-key-093>"))
	request(comp, wait=FALSE)
	data(id=output(comp), computation=identifier(comp))
}
