Worker <- new.env()
with(Worker, {
	DataStore <- new.env(emptyenv()) # data_href -> chunk (value/stub)
	CompStore <- new.env(emptyenv()) # (prereq|comp)_href -> env of computations
})

register_audience <- function(x, audience) {
	stopifnot(is.list(audience))
	UseMethod("register_audience", x)
}
register_audience.Chunk <- function(x, audience) {
	lapply(audience, function(loc) if (!(orcv::is.Location(loc) && loc == orcv::location())) push(x, loc))
}
register_audience.ChunkStub <- function(x, audience) x$audience <- c(x$audience, audience)

register_posted_data <- function(href, data) {
	log("Registering data with href %s", href)
	chunk <- Chunk(href, data)
	if (exists(href, Worker$DataStore)) {
		stub <- get(href, Worker$DataStore)
		register_audience(chunk, stub$audience)
	}
	assign(href, chunk, Worker$DataStore)
	if (exists(href, Worker$CompStore)) {
		computations <- get(href, Worker$CompStore)
		eapply(computations, update_comp_args, chunk)
	}
}

register_referenced_data <- function(href) {
	if (!exists(href, Worker$DataStore)) {
		async_pull(href)
		assign(href, ChunkStub(href), Worker$DataStore)
	} else get(href, Worker$DataStore)
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

data_avail <- function(computation) {
	stopifnot(inherits(computation, "Computation"))
	sapply(computation$arguments, inherits, "Chunk")
}

postData <- function(event) {
	hrefs <- extract(orcv::header(event), "POST /data/(.*)")
	data <- orcv::payload(event)
	stopifnot(length(hrefs) == length(data))
	mapply(register_posted_data, hrefs, data)
}

get_data <- function(header_extraction, audience_extraction) {
	function(event) {
		data_hrefs <- extract(orcv::header(event), header_extraction)
		audience <- audience_extraction(event)
		data <- lapply(data_hrefs, register_referenced_data)
		register_audience(data, list(audience))
	}
}
getData <- get_data("GET /data/(.*)", orcv::fd)
asyncGetData <- get_data("GET /async/data/(.*)", orcv::location)

putComputation <- function(event) {
	computation_href <- extract(orcv::header(event), "PUT /computation/(.*)")
	compref <- orcv::payload(event)
	arguments <- lapply(compref$arguments, register_arg, compref)
	computation <- Computation(compref, arguments)
	assign(computation$output_href, ChunkStub(computation$output_href), Worker$DataStore)
	if (!length(computation$arguments) || all(data_avail(computation))) run_comp(computation)
}

deleteData <- function(event) {
       hrefs <- extract(orcv::header(event), "DELETE /data/(.*)")
       if (length(hrefs)) rm(list=hrefs, pos=Worker$DataStore)
}

update_comp_args <- function(computation, chunk) {
	stopifnot(inherits(computation, "Computation"))
	stopifnot(inherits(chunk, "Chunk"))
	computation$arguments[[match(chunk$href, sapply(computation$arguments, "[[", "href"))]] <- chunk
	if (all(data_avail(computation))) run_comp(computation)
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
