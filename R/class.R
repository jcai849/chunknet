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
	class(chunkref) <- "ChunkReference"
	reg.finalizer(chunkref, delete)
	chunkref
}

Chunk <- function(id=uuid::UUIDgenerate(), data) {
	chunk <- Href(id)
	class(chunk) <- c("Chunk", class(chunk))
	chunk$data <- data
	chunk
}

ChunkReferenceArray <- function(chunkrefs, dim) UseMethod("ChunkReferenceArray", chunkrefs)
ChunkReferenceArray.list <- function(chunkrefs, dim=length(chunkrefs)) {
	stopifnot(all(sapply(chunkrefs, is.ChunkReference)))
	as.ChunkReferenceArray(array(chunkrefs, dim))
}
ChunkReferenceArray.ChunkReference <- function(chunkrefs, dim) {
	ChunkReferenceArray(list(chunkrefs))
}

as.ChunkReferenceArray <- function(x, ...) {
	stopifnot(is.array(x),
		  all(sapply(x, is.ChunkReference)))
	class(x) <- "ChunkReferenceArray"
	x
}

is.ChunkReference <- function(x, ...) inherits(x, "ChunkReference")
is.ChunkReferenceArray <- function(x, ...) inherits(x, "ChunkReferenceArray")
is.Chunk <- function(x, ...) inherits(x, "Chunk")

href <- function(x) get("href", x)
data <- function(x) get("data", x)
init_loc <- function(x) get0("init_loc", x)

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
	stopifnot(all(sapply(arguments, is.ChunkReference)))
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

is.ComputationReference <- function(x, ...) inherits(x, "ComputationReference")
is.Computation <- function(x, ...) inherits(x, "Computation")

delete <- function(x, ...) {
	locations <- determine_locations(list(list(x)))
	hrefs <- href(x)
	lapply(locations, orcv::send, paste0("DELETE /data/", paste(hrefs, collapse=',')))
	log("Deleting data location from locator")
        orcv::send(LOCATOR(), paste0("DELETE /data/", paste(hrefs, collapse=',')))
}
