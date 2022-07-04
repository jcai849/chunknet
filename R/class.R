Href <- function(id=uuid::UUIDgenerate()) {
       stopifnot(is.character(id))
       href <- structure(new.env(parent=emptyenv()), class="Href")
       href$href <- id
       href
}

ChunkReference <- function(id=uuid::UUIDgenerate()) {
	chunkref <- Href(id)
	class(chunkref) <- c("ChunkReference", class(chunkref))
	reg.finalizer(chunkref, delete)
	chunkref
}

Chunk <- function(id=uuid::UUIDgenerate(), data) {
	chunk <- Href(id)
	class(chunk) <- c("Chunk", class(chunk))
	chunk$data <- data
	chunk
}

ChunkStub <- function(id=uuid::UUIDgenerate(), audience=list()) {
	stopifnot(is.integer(audience))
	stub <- Href(id)
	class(stub) <- c("ChunkStub", class(stub))
	stub$audience <- audience
	stub
}

ComputationReference <- function(procedure, arguments) {
	stopifnot(is.character(procedure) || is.function(procedure))
	stopifnot(is.list(arguments))
	stopifnot(all(sapply(arguments, inherits, "ChunkReference")))
	compref <- Href()
	class(compref) <- c("ComputationReference", class(compref))
	compref$procedure <- procedure
	compref$output_href <- uuid::UUIDgenerate()
	compref$arguments <- arguments
	compref
}

Computation <- function(ComputationReference, arguments) {
	stopifnot(inherits(ComputationReference, "ComputationReference"))
	stopifnot(is.list(arguments))
	stopifnot(all(sapply(arguments, inherits, c("Chunk", "ChunkStub"))))
	comp <- ComputationReference
	comp$arguments <- arguments
	class(comp) <- c("Computation", class(comp))
	comp
}

delete <- function(x) {
	stopifnot(inherits(x, "ChunkReference"))
        locations <- get_location(x$href)
	log("Deleting data from location")
	lapply(locations, orcv::send, paste0("DELETE /data/", x$href))
	log("Deleting data location from locator")
        send(LOCATOR(), paste0("DELETE /data/", x$href))
}
