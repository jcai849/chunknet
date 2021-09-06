read.hdfs <- function(path) {
	comp <- computation(fun=function(path) 1:100,
			    input=path,
			    id=identifier("computation", "<unique-key-394>"))
	request(comp, wait=FALSE)
}
