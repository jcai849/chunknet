Href <- function(id=uuid::UUIDgenerate()) {
       stopifnot(is.character(href))
       href <- structure(new.env(parent=emptyenv()), class="Href")
       href$href <- href
       href
}

ChunkReference <- function(id=uuid::UUIDgenerate()) {
	chunkref <- Href(id)
	class(chunk) <- c("ChunkReference", class(chunk))
	reg.finalizer(chunkref, delete)
	chunkref
}

Chunk <- function(id=uuid::UUIDgenerate(), data) {
	chunk <- Href(id)
	class(chunk) <- c("Chunk", class(chunk))
	chunk$data <- data
	chunk
}

ChunkStub <- function(id=uuid::UUIDgenerate(), audience=integer()) {
	stopifnot(is.integer(audience))
	stub <- Href(id)
	class(stub) <- c("ChunkStub", class(stub))
	stub$audience <- audience
}

ComputationReference <- function(procedure, arguments) {
	stopifnot(is.character(procedure) || is.function(procedure))
	stopifnot(is.list(arguments))
	stopifnot(sapply(arguments, inherits, "ChunkReference"))
	compref <- Href()
	class(compref) <- c("ComputationReference", class(compref))
	compref$procedure <- procedure
	compref$output_href <- uuid::UUIDgenerate()
	compref$arguments <- arguments
	compref
}

Computation <- function(ComputationReference, arguments) {
	stopifnot(is.character(procedure) || is.function(procedure))
	stopifnot(is.list(arguments))
	stopifnot(sapply(arguments, inherits, c("Chunk", "ChunkStub")))
	comp <- ComputationReference
	comp$arguments <- arguments
	class(comp) <- c("Computation", class(comp))
	comp
}

delete <- function(x) UseMethod("delete", x)

delete.ChunkReference <- function(x) {
        locations <- get_location(x)
        for (i in seq(NROW(locations))) {
                event_external_push(paste0("DELETE /data/", x$href), NULL, locations[i, "address"], locations[i, "port"])
        }
        event_external_push(paste0("DELETE /data/", x$href), NULL, LOCATOR()$address, LOCATOR()$port)
}
