Href <- function(id=uuid::UUIDgenerate()) {
       stopifnot(is.character(id))
       href <- structure(new.env(parent=emptyenv()), class="Href")
       href$href <- id
       href
}

ChunkReference <- function(id=uuid::UUIDgenerate(), init_loc, gen_comp) {
	chunkref <- Href(id)
	chunkref$init_loc <- init_loc
	chunkref$gen_comp <- gen_comp
	class(chunkref) <- c("ChunkReference", class(chunkref))
	reg.finalizer(chunkref, delete_chunk)
	chunkref
}

Chunk <- function(id=uuid::UUIDgenerate(), data) {
	chunk <- Href(id)
	class(chunk) <- c("Chunk", class(chunk))
	chunk$data <- data
	chunk
}

href <- function(x) get("href", x)
data <- function(x) get("data", x)

AwaitedChunk <- function(id=uuid::UUIDgenerate()) {
	stub <- Href(id)
	class(stub) <- c("AwaitedChunk", class(stub))
	stub$audience <- list()
	stub
}

add_audience <- function(stub, member) {
	stub$audience <- c(stub$audience, list(member))
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

output_href <- function(x) get("output_href", x)

Computation <- function(ComputationReference, arguments) {
	stopifnot(inherits(ComputationReference, "ComputationReference"))
	stopifnot(is.list(arguments))
	stopifnot(all(sapply(arguments, inherits, c("Chunk", "AwaitedChunk"))))
	comp <- ComputationReference
	comp$arguments <- arguments
	class(comp) <- c("Computation", class(comp))
	comp
}

delete_chunk <- function(x) delete_href(x$href)

delete_href <- function(hrefs, ...) {
	stopifnot(is.character(hrefs))
        locations <- get_locations(hrefs)
	lapply(locations, orcv::send, paste0("DELETE /data/", paste(hrefs, collapse=',')))
	log("Deleting data location from locator")
        orcv::send(LOCATOR(), paste0("DELETE /data/", paste(hrefs, collapse=',')))
}
