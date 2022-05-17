Chunk <- function(href, generator_href) {
       stopifnot(is.character(href),
                 is.character(generator_href))
       chunk <- structure(new.env(parent=emptyenv(), size=2L), class="Chunk")
       chunk$href <- href
       chunk$generator_href <- generator_href
       chunk
}

TransientChunk <- function(href, generator_href) {
        chunk <- Chunk(href, generator_href)
        class(chunk) <- c("TransientChunk", class(chunk))
        chunk
}

StableChunk <- function(href, generator_href) {
        chunk <- Chunk(href, generator_href)
        class(chunk) <- c("StableChunk", class(chunk))
        reg.finalizer(chunk, delete)
        chunk
}

delete <- function(chunk) {
        locations <- get_location(chunk)
        for (i in seq(NROW(locations))) {
                event_external_push(paste0("DELETE /data/", chunk$href), NULL, locations[i, "address"], locations[i, "port"])
        }
        event_external_push(paste0("DELETE /data/", chunk$href), NULL, LOCATOR()$address, LOCATOR()$port)
}

Computation <- function(procedure, arguments, alignments, href, output_href) {
	comp <- structure(new.env(parent=emptyenv(), size=5L), class="Computation")
	comp$procedure <- procedure
	comp$arguments <- arguments
	comp$alignments <- alignments
	comp$href <- href
	comp$output_href <- output_href
	comp
}
