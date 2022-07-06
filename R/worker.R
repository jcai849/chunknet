Worker <- new.env()
with(Worker, {
	DataStore <- new.env(emptyenv()) # data_href -> chunk (value/stub)
	CompStore <- new.env(emptyenv()) # (prereq|comp)_href -> env of computations
})

postData <- function(event) {
	href <- extract(orcv::header(event), "POST /data/(.*)")
	register_posted_data(href, orcv::payload(event))
}

register_posted_data <- function(href, data) {
	chunk <- Chunk(href, data)
	if (exists(href, Worker$DataStore)) {
		stub <- get(href, Worker$DataStore)
		respond_audience(chunk, stub$audience)
	}
	assign(href, chunk, Worker$DataStore)
	if (exists(href, Worker$CompStore)) {
		computations <- get(href, Worker$CompStore)
		eapply(computations, update_comp_args, chunk)
	}
}

update_comp_args <- function(computation, chunk) {
	stopifnot(inherits(computation, "Computation"))
	stopifnot(inherits(chunk, "Chunk"))
	computation$arguments[[match(chunk$href, sapply(computation$arguments, "[[", "href"))]] <- chunk

	if (all(data_avail(computation))) run_comp(computation)
}

data_avail <- function(computation) {
	stopifnot(inherits(computation, "Computation"))
	sapply(computation$arguments, inherits, "Chunk")
}

getData <- function(event) {
	data_href <- extract(orcv::header(event), "GET /data/(.*)")
	audience <- list(orcv::fd(event))
	data <- register_referenced_data(data_href, audience)
	respond_audience(data, audience)
}

asyncGetData <- function(event) {
	data_href <- extract(orcv::header(event), "GET /async/data/(.*)")
	audience <- list(orcv::location(event))
	data <- register_referenced_data(data_href, audience)
	respond_audience(data, audience)
}

respond_audience <- function(x, audience, ...) UseMethod("respond_audience", x)
respond_audience.ChunkStub <- function(x, audience, ...) {
	x$audience <- c(x$audience, audience)
}
respond_audience.Chunk <- function(x, audience, ...) {
	lapply(audience, orcv::send,
	       paste0("POST /data/", x$href), x$data)
}

putComputation <- function(event) {
	computation_href <- extract(orcv::header(event), "PUT /computation/(.*)")
	compref <- orcv::payload(event)
	arguments <- lapply(compref$arguments, register_arg, compref)
	computation <- Computation(compref, arguments)
	assign(computation$output_href, ChunkStub(computation$output_href), Worker$DataStore)
	if (!length(computation$arguments) || all(data_avail(computation))) run_comp(computation)
}

register_arg <- function(prereq, comp) {
	register_prereq(prereq, comp)		# fills out compstore
	register_referenced_data(prereq$href)	# fills out datastore
}

register_prereq <- function(prereq, comp) {
	associated_comps <- if (exists(prereq$href, Worker$CompStore)) {
				get(prereq$href, Worker$CompStore)
			    } else {
				assign(prereq$href, new.env(parent=emptyenv()), Worker$CompStore)
			    }
	assign(comp$href, comp, associated_comps)
}

register_referenced_data <- function(href, audience=list()) {
	if (!exists(href, Worker$DataStore)) {
		async_pull(href)
		assign(href, ChunkStub(href, audience), Worker$DataStore)
	} else get(href, Worker$DataStore)
}

run_comp <- function(computation) {
	log("Running computation")
	args <- lapply(computation$arguments, "[[", "data")
	result <- tryCatch(do.call(computation$procedure, args), error=identity)
	lapply(computation$arguments, prereq_cleanup, computation) # some way to do implicitly with finaliser?
	register_posted_data(computation$output_href, result)
}

prereq_cleanup <- function(prereq, associated_computation) {
	comprefs <- get(prereq$href, Worker$CompStore)
	rm(list=associated_computation$href, pos=comprefs)
	if (!length(comprefs)) rm(list=prereq$href, pos=Worker$CompStore)
}

deleteData <- function(event) {
       href <- extract(orcv::header(event), "DELETE /data/(.*)")
       if (length(href)) rm(list=href, pos=Worker$DataStore)
}
